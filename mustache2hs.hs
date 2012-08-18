module Main where

import Control.Arrow
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad.Trans.State (get, modify, evalState, State)

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

import ParseRecords

type MuTree =  [Mustache]

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
	body <- many1 (comment <|> sectionInv <|> section <|> tripleVar <|> ampVar <|> mustache (var True) <|> txt)
	return $ filter (not . isMuComment) body
	where
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
originalMustache = mconcat . map origOne
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

codeGenTree :: (Show a, Enum a) => Text -> String -> Records -> MuTree -> State a Builder
codeGenTree fname rname recs tree = do
	let Just rec = lookup rname recs
	(code, helpers) <- (second concat . unzip) <$> mapM (codeGen (rname,rec) recs) tree
	return $ mconcat [
			Builder.fromText fname,
			Builder.fromString " escapeFunction ctx@(",
			pattern rec,
			Builder.fromString ") = mconcat [",
			mintercalate comma code,
			Builder.fromString "]\n",
			if null helpers then mempty else Builder.fromString "\twhere\n\t",
			mintercalate wsep helpers
		]
	where
	pattern rec = mconcat [
			Builder.fromString rname,
			Builder.fromString " {",
			mintercalate comma $ map (\x -> mconcat [
					Builder.fromText x,
					Builder.fromString "=",
					Builder.fromText x
				]) (map fst rec),
			Builder.fromString "}"
		]
	wsep = Builder.fromString "\n\t"
	comma = Builder.fromString ", "

codeGen :: (Show a, Enum a) => (String,Record) -> Records -> Mustache -> State a (Builder, [Builder])
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
codeGen (rname,rec) recs (MuSection name stree)
	| lookup name rec == Just MuLambda =
		return (mconcat [
				Builder.fromText name,
				Builder.fromString " (",
				Builder.fromShow $ BS.toString $ Builder.toByteString $ originalMustache stree,
				Builder.fromString " )"
			], [])
	| otherwise = do
		id <- get
		modify succ
		let nm = name `mappend` T.pack (show id)
		case lookup name rec of
			Just (MuList rname) -> do
				helper <- codeGenTree nm rname recs stree
				return (mconcat [
						Builder.fromString "map (",
						Builder.fromText nm,
						Builder.fromString " escapeFunction) ",
						Builder.fromText name
					], [helper])
			_ -> do
				helper <- codeGenTree nm rname recs stree
				return (mconcat [
						Builder.fromString "case ",
						Builder.fromText name,
						Builder.fromString " of { Just _ -> (",
						Builder.fromText nm,
						Builder.fromString " escapeFunction ctx); _ -> mempty }"
					], [helper])
codeGen (rname,rec) recs (MuSectionInv name stree) = do
	id <- get
	modify succ
	let nm = name `mappend` T.pack (show id)
	helper <- codeGenTree nm rname recs stree
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
		let name = takeBaseName input
		let fname = T.pack name
		let rname = (toUpper $ head name) : tail (name ++ "Record")
		recs <- extractRecords <$> readFile "Records.hs"
		Builder.toByteStringIO BS.putStr $ evalState (codeGenTree fname rname recs tree) 0
		putStrLn ""
