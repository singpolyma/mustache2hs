module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.FilePath (takeBaseName)
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad
import Control.Arrow
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

data Flag = Help | RecordModule String deriving (Show, Read, Eq)

flags :: [OptDescr Flag]
flags = [
		Option ['m'] ["record-module"] (ReqArg RecordModule "MODULE") "Module containing records to be used as context.",
		Option ['h'] ["help"] (NoArg Help) "Show this help text."
	]

usage :: [String] -> IO ()
usage errors = do
	mapM_ (hPutStrLn stderr) errors
	name <- getProgName
	hPutStrLn stderr $ usageInfo (name ++ " [-m MODULE] <input-files>") flags

type MuTree =  [Mustache]

data Mustache =
	MuText       Text |
	MuVar        Text Bool |
	MuSection    Text MuTree |
	MuSectionInv Text MuTree |
	MuPartial    Text |
	MuComment
	deriving (Show, Eq)

isMuComment :: Mustache -> Bool
isMuComment MuComment = True
isMuComment _ = False

parser :: Parser MuTree
parser = do
	body <- many1 (
			comment <|>
			sectionInv <|>
			section <|>
			partial <|>
			tripleVar <|>
			ampVar <|>
			mustache (var True) <|>
			txt
		)
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
	sectionInv =
		liftA2 MuSectionInv (sectionPiece '^') parser <* sectionPiece '/'
	section = liftA2 MuSection (sectionPiece '#') parser <* sectionPiece '/'
	sectionPiece c = mustache $ do
		_ <- char c
		name
	partial = mustache $ do
		_ <- char '>'
		skipSpace
		MuPartial <$> takeWhile1 (/='}')
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

codeGenTree :: (Show a, Enum a) => Text -> String -> Records -> MuTree -> State a (Builder, [Text])
codeGenTree fname rname recs tree = do
	let rec = recordMustExist $ lookup rname recs
	(code, helpers', partials) <- unzip3 <$> mapM (codeGen (rname,rec) recs) tree
	let helpers = concat helpers'
	return (mconcat [
			Builder.fromText fname,
			Builder.fromString " escapeFunction ctx@(",
			pattern rec,
			Builder.fromString ") = mconcat [",
			mintercalate comma code,
			Builder.fromString "]",
			if null helpers then mempty else Builder.fromString " where {",
			mintercalate wsep helpers,
			if null helpers then mempty else Builder.fromString "}",
			Builder.fromString "\n"
		], concat partials)
	where
	recordMustExist (Just r) = r
	recordMustExist _ = error ("No record named: " ++ rname)
	pattern rec = mconcat [
			Builder.fromString (fst rec),
			Builder.fromString " {",
			mintercalate comma $ map (\x -> mconcat [
					Builder.fromText x,
					Builder.fromString "=",
					Builder.fromText x
				]) (map fst $ snd rec),
			Builder.fromString "}"
		]
	wsep = Builder.fromString "; "
	comma = Builder.fromString ", "

codeGen :: (Show a, Enum a) => (String,Record) -> Records -> Mustache -> State a (Builder, [Builder], [Text])
codeGen _ _ (MuText txt) = return (mconcat [
		Builder.fromString "Builder.fromString ",
		Builder.fromShow (T.unpack txt)
	], [], [])
codeGen _ _ (MuVar name False) = return (mconcat [
		Builder.fromString "fromMaybe mempty (fmap ",
		Builder.fromString "(Builder.fromText . toPathPiece) (",
		Builder.fromText name,
		Builder.fromString "))"
	], [], [])
codeGen _ _ (MuVar name True) = return (mconcat [
		Builder.fromString "fromMaybe mempty (fmap ",
		Builder.fromString "(Builder.fromText . escapeFunction . toPathPiece) (",
		Builder.fromText name,
		Builder.fromString "))"
	], [], [])
codeGen (rname,rec) recs (MuSection name stree)
	| lookup name (snd rec) == Just MuLambda =
		return (mconcat [
				Builder.fromText name,
				Builder.fromString " (",
				Builder.fromShow $ BS.toString $
					Builder.toByteString $ originalMustache stree,
				Builder.fromString " )"
			], [], [])
	| otherwise = do
		id <- get
		modify succ
		let nm = name `mappend` T.pack (show id)
		case lookup name (snd rec) of
			Just (MuList rname) -> do
				(helper, partials) <- codeGenTree nm rname recs stree
				return (mconcat [
						Builder.fromString "mconcat $ map (",
						Builder.fromText nm,
						Builder.fromString " escapeFunction) ",
						Builder.fromText name
					], [helper], partials)
			_ -> do
				(helper, partials) <- codeGenTree nm rname recs stree
				return (mconcat [
						Builder.fromString "case ",
						Builder.fromText name,
						Builder.fromString " of { Just _ -> (",
						Builder.fromText nm,
						Builder.fromString " escapeFunction ctx); _ -> mempty }"
					], [helper], partials)
codeGen (rname,rec) recs (MuSectionInv name stree) = do
	id <- get
	modify succ
	let nm = name `mappend` T.pack (show id)
	(helper, partials) <- codeGenTree nm rname recs stree
	return (mconcat [
			Builder.fromString "if foldr (\\_ _ -> False) True ",
			Builder.fromText name,
			Builder.fromString " then ",
			Builder.fromText nm,
			Builder.fromString " escapeFunction ctx else mempty"
		], [helper], partials)
codeGen (rname,rec) recs (MuPartial name) =
	let fname = takeBaseName $ T.unpack name in
	return (mconcat [
		Builder.fromString fname,
		Builder.fromString " escapeFunction ctx"
	], [], [name])
codeGen _ _ _ = return (mempty, [], [])

codeGenFile :: Records -> FilePath -> IO (Builder, [FilePath])
codeGenFile recs input = do
	Right tree <- parseOnly parser <$> T.readFile input
	let name = takeBaseName input
	let fname = T.pack name
	let rname = (toUpper $ head name) : tail (name ++ "Record")
	let (builder, partials) = evalState (codeGenTree fname rname recs tree) 0
	return (builder, map T.unpack partials)

codeGenFiles :: Records -> [FilePath] -> IO Builder
codeGenFiles _ [] = return mempty
codeGenFiles recs inputs = do
		(builders, partials) <- unzip <$> mapM (codeGenFile recs) inputs
		builder <- codeGenFiles recs (concat partials)
		return $ (mconcat builders) `mappend` builder

main :: IO ()
main = do
	(flags, args, errors) <- fmap (getOpt RequireOrder flags) getArgs
	case (args, errors) of
		_ | Help `elem` flags -> usage errors
		_ | null (getRecordModules flags) -> usage errors >> exitFailure
		_ | null args -> usage errors >> exitFailure
		_ -> main' (getRecordModules flags) args
	where
	main' recordModules inputs = do
		(ms, recs) <- unzip <$> mapM (fmap extractRecords . readFile) recordModules
		builder <- codeGenFiles (concat recs) inputs
		putStrLn "import Prelude hiding (foldr)"
		putStrLn "import Data.Foldable (foldr)"
		putStrLn "import Data.Maybe"
		putStrLn "import Data.Monoid"
		putStrLn "import Web.PathPieces" -- Maybe use a different typeclass?
		putStrLn "import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder"
		mapM_ (\m -> putStrLn $ "import " ++ m ++ "\n") ms
		Builder.toByteStringIO BS.putStr builder
	getRecordModules = foldr (\x ms -> case x of
			RecordModule m -> m : ms
			_ -> ms
		) []
