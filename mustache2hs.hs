module Main where

import Control.Arrow
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad.Trans.State (get, modify, evalState, State)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text
import Control.Applicative

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

type MuTree = (MuTypeHeader, [Mustache])
type MuTypeHeader = [(Text, MuType)]
data MuType = MuList | MuLambda deriving (Show, Eq)

data Mustache =
	MuText       Text |
	MuVar        Text Bool |
	MuSection    Text MuTree |
	MuSectionInv Text MuTree |
	MuComment
	deriving (Show, Eq)

isMuComment :: Mustache -> Bool
isMuComment MuComment = True
isMuComment _ = False

parser :: Parser MuTree
parser = do
	th <- option [] (skipSpace *> typeheader)
	body <- many1 (comment <|> sectionInv <|> section <|> tripleVar <|> ampVar <|> mustache (var True) <|> txt)
	return (th, filter (not . isMuComment) body)
	where
	typeheader = mustache $ do
		_ <- char '!'
		_ <- char '#'
		many1 (skipSpace *> onetype) <* skipSpace
	onetype = do
		n <- name
		skipSpace
		_ <- char ':'
		_ <- char ':'
		skipSpace
		t <- typeList <|> typeLambda
		return (n, t)
	typeList = char '[' >> char ']' >> return MuList
	typeLambda = char '(' >> char '-' >> char '>' >> char ')' >> return MuLambda
	comment = mustache $ do
		_ <- char '!'
		many $ do
				c1 <- anyChar
				c2 <- peekChar
				if c1 /= '}' || c2 /= Just '}' then return c1 else
					fail "End of comment text"
		return MuComment
	sectionInv = liftA2 MuSectionInv (sectionPiece '^') parser <* sectionPiece '/'
	section = liftA2 MuSection (sectionPiece '#') parser <* sectionPiece '/'
	sectionPiece c = mustache $ do
		_ <- char c
		name
	tripleVar = mustache $ do
		_ <- char '{'
		v <- var False
		_ <- char '}'
		return v
	ampVar = mustache $ do
		_ <- char '&'
		skipSpace
		var False
	var escaped = (`MuVar` escaped) <$> name
	txt = MuText <$> takeWhile1 (/='{') -- TODO: allow single { in text
	name = takeWhile1 (\c -> isAlpha c || isDigit c)
	mustache f = do
		_ <- char '{'
		_ <- char '{'
		v <- f
		_ <- char '}'
		_ <- char '}'
		return v

mintercalate :: (Monoid a) => a -> [a] -> a
mintercalate xs xss = mconcat (intersperse xs xss)

originalMustache :: MuTree -> Builder
originalMustache (_, tree) = mconcat $ map origOne tree
	where
	origOne (MuText txt) = Builder.fromText txt
	origOne (MuVar name True) = mconcat [
			Builder.fromString "{{",
			Builder.fromText name,
			Builder.fromString "}}"
		]
	origOne (MuVar name False) = mconcat [
			Builder.fromString "{{{",
			Builder.fromText name,
			Builder.fromString "}}}"
		]
	origOne (MuSection name tree) = mconcat [
			Builder.fromString "{{#",
			Builder.fromText name,
			Builder.fromString "}}",
			originalMustache tree,
			Builder.fromString "{{/",
			Builder.fromText name,
			Builder.fromString "}}"
		]
	origOne (MuSectionInv name tree) = mconcat [
			Builder.fromString "{{^",
			Builder.fromText name,
			Builder.fromString "}}",
			originalMustache tree,
			Builder.fromString "{{/",
			Builder.fromText name,
			Builder.fromString "}}"
		]
	origOne _ = mempty

ctxVars :: MuTree -> [Text]
ctxVars (types, tree) = nub $ concatMap oneVars tree
	where
	oneVars (MuVar name _) = [name]
	oneVars (MuSection name (stypes, stree))
		| isJust (lookup name types) = [name]
		| otherwise = name: ctxVars ((stypes ++ types), stree)
	oneVars (MuSectionInv name (stypes, stree)) =
		name : ctxVars ((stypes ++ types), stree)
	oneVars _ = []

codeGenTree :: (Show a, Enum a) => Text -> Text -> MuTree -> State (a, Map Text [Text]) Builder
codeGenTree fname name (types, tree) = do
	(code, helpers) <- (second concat . unzip) <$> mapM (codeGen types name) tree
	(_, recs) <- get
	return $ mconcat [
			Builder.fromText fname,
			Builder.fromString " escapeFunction ctx@(",
			pattern (Map.lookup name recs),
			Builder.fromString ") = mconcat [",
			mintercalate comma code,
			Builder.fromString "]\n",
			if null helpers then mempty else Builder.fromString "\twhere\n\t",
			mintercalate wsep helpers
		]
	where
	pattern (Just ctx) = mconcat [
			Builder.fromText name,
			Builder.fromString "Record {",
			mintercalate comma $ map (\x -> mconcat [
					Builder.fromText x,
					Builder.fromString "=",
					Builder.fromText x
				]) ctx,
			Builder.fromString "}"
		]
	wsep = Builder.fromString "\n\t"
	comma = Builder.fromString ", "

codeGen :: (Show a, Enum a) => MuTypeHeader -> Text -> Mustache -> State (a, Map Text [Text]) (Builder, [Builder])
codeGen _ _ (MuText txt) = return (Builder.fromShow (T.unpack txt), [])
codeGen _ _ (MuVar name False) = return (mconcat [
		Builder.fromString "fromMaybe mempty ",
		Builder.fromText name
	], [])
codeGen _ _ (MuVar name True) = return (mconcat [
		Builder.fromString "fromMaybe mempty (escapeFunction (",
		Builder.fromText name,
		Builder.fromString "))"
	], [])
codeGen types ctxName (MuSection name (stypes, stree))
	| lookup name types == Just MuLambda =
		return (mconcat [
				Builder.fromText name,
				Builder.fromString " (",
				Builder.fromShow $ BS.toString $ Builder.toByteString $ originalMustache (stypes, stree),
				Builder.fromString " )"
			], [])
	| otherwise = do
		(id, recs) <- get
		modify (first succ)
		let nm = name `mappend` T.pack (show id)
		case lookup name types of
			Just MuList -> do
				let rec = concat $ maybeToList (Map.lookup name recs)
				modify (second $ Map.insert name
					(nub $ ctxVars (stypes ++ types, stree) ++ rec))
				helper <- codeGenTree nm name (stypes ++ types, stree)
				return (mconcat [
						Builder.fromString "map (",
						Builder.fromText nm,
						Builder.fromString " escapeFunction) ",
						Builder.fromText name
					], [helper])
			_ -> do
				helper <- codeGenTree nm ctxName (stypes ++ types, stree)
				return (mconcat [
						Builder.fromString "case ",
						Builder.fromText name,
						Builder.fromString " of { Just _ -> (",
						Builder.fromText nm,
						Builder.fromString " escapeFunction ctx); _ -> mempty }"
					], [helper])
codeGen types ctxName (MuSectionInv name (stypes, stree)) = do
		(id, _) <- get
		modify (first succ)
		let nm = name `mappend` T.pack (show id)
		helper <- codeGenTree nm ctxName (stypes ++ types, stree)
		return (mconcat [
				Builder.fromString "if foldr (\\_ _ -> False) True ",
				Builder.fromText name,
				Builder.fromString " then ",
				Builder.fromText nm,
				Builder.fromString " escapeFunction ctx else mempty"
			], [helper])
codeGen _ _ _ = return mempty

main :: IO ()
main = do
		[input] <- getArgs
		Right tree <- parseOnly parser <$> T.readFile input
		let name = T.pack $ takeBaseName input
		Builder.toByteStringIO BS.putStr $ evalState (codeGenTree name name tree) (0, Map.fromList [(name, ctxVars tree)])
		putStrLn ""
