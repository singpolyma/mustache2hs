module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder(..), OptDescr(..), ArgDescr(..))
import System.FilePath (dropExtension, takeDirectory, (</>), normalise)
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.Word
import Data.List
import Control.Monad
import Control.Arrow
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get, modify, evalState, State, StateT, evalStateT)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Parsec hiding ((<|>), State, many)
import Text.Parsec.Text
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
	hPutStrLn stderr $ usageInfo (name ++ " [-m MODULE] <input-file> <record-name> ...") flags

type MuTree =  [MustachePos]
type MustachePos = (SourcePos, Mustache)

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
			try (withPos comment) <|>
			try (withPos sectionInv) <|>
			try (withPos section) <|>
			try (withPos partial) <|>
			try (withPos tripleVar) <|>
			try (withPos ampVar) <|>
			try (withPos (mustache False (var True))) <|>
			try (withPos singlebrace) <|>
			try (withPos txt)
		)
	return $ filter (not . isMuComment . snd) body
	where
	comment = mustache True $ do
		_ <- char '!'
		_ <- many $ do
				c1 <- anyChar
				c2 <- peekChar
				if c1 /= '}' || c2 /= Just '}' then return c1 else
					fail "End of comment text"
		return MuComment
	sectionInv =
		liftA2 MuSectionInv (sectionPiece '^') parser <* sectionPiece '/'
	section = liftA2 MuSection (sectionPiece '#') parser <* sectionPiece '/'
	sectionPiece c = mustache True $ do
		_ <- char c
		name
	partial = mustache False $ do
		_ <- char '>'
		skipSpace
		MuPartial <$> takeWhile1 (/='}')
	tripleVar = mustache False $ do
		_ <- char '{'
		v <- var False
		_ <- char '}'
		return v
	ampVar = mustache False $ do
		_ <- char '&'
		skipSpace
		var False
	var escaped = (`MuVar` escaped) <$> name
	singlebrace = MuText <$> T.singleton <$> (do
			c <- char '{'
			n <- peekChar
			if n == Just '{' then fail "singlebrace not match {{" else return c
		)
	txt = MuText <$> takeWhile1 (/='{')
	name = takeWhile1 (\c -> isAlpha c || isDigit c || c == '_')
	mustache ws f = do
		_ <- char '{'
		_ <- char '{'
		v <- f
		_ <- char '}'
		_ <- char '}'
		when ws (try endOfLine <|> pure ())
		return v
	withPos = liftA2 (,) getPosition
	-- Parsec compat with Attoparsec
	peekChar = lookAhead (option Nothing (Just <$> anyChar))
	skipSpace = skipMany (satisfy isSpace)
	endOfLine = void ((char '\r' >> char '\n') <|> char '\n')
	takeWhile1 f = T.pack <$> many1 (satisfy f)

mintercalate :: (Monoid a) => a -> [a] -> a
mintercalate xs xss = mconcat (intersperse xs xss)

originalMustache :: MuTree -> Builder
originalMustache = mconcat . map (origOne . snd)
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

monoidSpecialCase :: Text -> Record -> Builder
monoidSpecialCase name rec = Builder.fromText $ case lookup name (snd rec) of
	Just MuBool ->
		T.pack "(Any " `mappend` name `mappend` T.pack ")"
	Just MuNum ->
		T.pack "(Sum " `mappend` name `mappend` T.pack ")"
	_ -> name

codeGenTree :: (Show a, Enum a) => FilePath -> Text -> String -> Records -> MuTree -> Word -> State a (Builder, [(FilePath, String)])
codeGenTree path fname rname recs tree level = do
	let rec = recordMustExist $ lookup rname recs
	(code, helpers', partials) <- unzip3 <$> mapM (\(pos,m) -> do
			(code, helpers, partials) <- codeGen path (rname,rec) recs level m
			let code' = mconcat [
					linePragma pos,
					Builder.fromString "\n\t",
					indent,
					code
				]
			return (code', helpers, partials)
		) tree
	let helpers = concat helpers'
	return (mconcat [
			Builder.fromText fname,
			Builder.fromString " escapeFunction ctx@(",
			pattern rec,
			Builder.fromString ") = mconcat [\n\t",
			indent,
			mintercalate comma code,
			Builder.fromString "\n",
			indent,
			Builder.fromString "]",
			if null helpers then mempty else mconcat [
				wsep,
				Builder.fromString "where",
				wsep
			],
			mintercalate wsep helpers
		], concat partials)
	where
	recordMustExist (Just r) = r
	recordMustExist _ = error ("No record named: " ++ rname)
	pattern rec = mconcat [
			Builder.fromString (fst rec),
			Builder.fromString " {",
			mintercalate icomma $ map ((\x -> mconcat [
					Builder.fromText x,
					Builder.fromString "=",
					Builder.fromText x
				]) . fst) (snd rec),
			Builder.fromString "}"
		]
	indent = Builder.fromString $ concat $
		replicate (fromIntegral level + 1) "\t"
	wsep = Builder.fromString "\n" `mappend` indent
	icomma = Builder.fromString ", "
	comma = Builder.fromString ",\n\t" `mappend` indent

codeGen :: (Show a, Enum a) => FilePath -> (String,Record) -> Records -> Word -> Mustache -> State a (Builder, [Builder], [(FilePath, String)])
codeGen _ _ _ _ (MuText txt) = return (mconcat [
		Builder.fromString "Builder.fromString ",
		Builder.fromShow (T.unpack txt)
	], [], [])
codeGen _ _ _ _ (MuVar name False) = return (mconcat [
		Builder.fromString "Builder.fromLazyText $ displayT $ renderPretty 0.4 80 $ pretty ",
		Builder.fromText name
	], [], [])
codeGen _ _ _ _ (MuVar name True) = return (mconcat [
		Builder.fromString "Builder.fromLazyText $ escapeFunction $ displayT $ renderPretty 0.4 80 $ pretty ",
		Builder.fromText name
	], [], [])
codeGen path (rname,rec) recs level (MuSection name stree)
	| lookup name (snd rec) == Just MuLambda =
		return (mconcat [
				Builder.fromText name,
				Builder.fromString " (",
				Builder.fromShow $ BS.toString $
					Builder.toByteString $ originalMustache stree,
				Builder.fromString " )"
			], [], [])
	| otherwise = do
		nm <- nextName name
		case lookup name (snd rec) of
			Just (MuList rname) -> do
				(helper, partials) <- codeGenTree path nm rname recs stree (level+1)
				return (mconcat [
						Builder.fromString "mconcat $ map (",
						Builder.fromText nm,
						Builder.fromString " escapeFunction) ",
						Builder.fromText name
					], [helper], partials)
			_ -> do
				(helper, partials) <- codeGenTree path nm rname recs stree (level+1)
				return (mconcat [
						Builder.fromString "if mempty /= ",
						monoidSpecialCase name rec,
						Builder.fromString " then ",
						Builder.fromText nm,
						Builder.fromString " escapeFunction ctx else mempty"
					], [helper], partials)
codeGen path (rname,rec) recs level (MuSectionInv name stree) = do
	nm <- nextName name
	(helper, partials) <- codeGenTree path nm rname recs stree (level+1)
	return (mconcat [
			Builder.fromString "if mempty == ",
			monoidSpecialCase name rec,
			Builder.fromString " then ",
			Builder.fromText nm,
			Builder.fromString " escapeFunction ctx else mempty"
		], [helper], partials)
codeGen path (rname,_) _ _ (MuPartial name) =
	let
		file = takeDirectory path </> T.unpack name
		fname = camelCasePath (dropExtension file)
	in
	return (mconcat [
		Builder.fromText fname,
		Builder.fromString " escapeFunction ctx"
	], [], [(file, rname)])
codeGen _ _ _ _ _ = return (mempty, [], [])

linePragma :: SourcePos -> Builder
linePragma s = mconcat [
		Builder.fromString "{-# LINE ",
		Builder.fromShow $ sourceLine s,
		Builder.fromString " ",
		Builder.fromShow $ sourceName s,
		Builder.fromString " #-}"
	]

nextName :: (Show a, Enum a) => Text -> State a Text
nextName name = do
	id <- get
	modify succ
	return $ name `mappend` T.pack (show id)

camelCasePath :: FilePath -> Text
camelCasePath = T.pack . lowerHead . go
	where
	lowerHead [] = []
	lowerHead (c:cs) = toLower c : cs
	go ('/':'/':cs) = go ('/' : cs)
	go ('/':c:cs)   = toUpper c : go cs
	go (c:cs) -- skip characters that cannot got in a name
		| isAlpha c || isDigit c || c == '_' = c : go cs
		| otherwise  = go cs
	go []           = []

codeGenFile :: Records -> (FilePath, String) -> StateT [(FilePath, String)] IO (Maybe Builder, [(FilePath, String)])
codeGenFile recs (input, rname) = do
	alreadyGen <- lookup input' <$> get
	case alreadyGen of
		Just r
			| r == rname -> return (Nothing, [])
			| otherwise -> fail ("Type mismatch, template " ++ input ++ " expects both " ++ r ++ " and " ++ "rname")
		Nothing -> do
			modify ((input',rname):)
			Right tree <- lift $ parse parser input <$> T.readFile input
			let fname = camelCasePath (dropExtension input)
			let (builder, partials) = evalState (codeGenTree input fname rname recs tree 0) (0::Int)
			return (Just builder, partials)
	where
	input' = normalise input

codeGenFiles :: Records -> [(FilePath, String)] -> StateT [(FilePath, String)] IO Builder
codeGenFiles _ [] = return mempty
codeGenFiles recs inputs = do
		(builders, partials) <- unzip <$> mapM (codeGenFile recs) inputs
		builder <- codeGenFiles recs (concat partials)
		return $ mintercalate nl (catMaybes builders) `mappend` nl `mappend` builder
		where
		nl = Builder.fromString "\n\n"

main :: IO ()
main = do
	(flags, args, errors) <- fmap (getOpt RequireOrder flags) getArgs
	case (args, errors) of
		_ | Help `elem` flags -> usage errors
		_ | null (getRecordModules flags) -> usage errors >> exitFailure
		_ | null args -> usage errors >> exitFailure
		_ -> main' (getRecordModules flags) (pairs args)
	where
	main' recordModules inputs = do
		(ms, recs, types) <- unzip3 <$> mapM (fmap extractRecords . readFile) recordModules
		let types' = concat types
		let inputs' = map (second (\r -> fromMaybe r (join $ lookup r types'))) inputs
		builder <- evalStateT (codeGenFiles (concat recs) inputs') []
		-- GHC pragma turns off warnings we know about
		-- Should be ignored by other compilers, so is safe
		putStrLn "{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-matches #-}"
		putStrLn "module MustacheTemplates where"
		putStrLn ""
		putStrLn "import Data.Monoid"
		putStrLn "import Text.PrettyPrint.Leijen.Text"
		putStrLn "import qualified Data.Text.Lazy.Builder as Builder"
		mapM_ (\m -> putStrLn $ "import " ++ m ++ "\n") ms
		Builder.toByteStringIO BS.putStr builder
		putStrLn ""
	getRecordModules = foldr (\x ms -> case x of
			RecordModule m -> m : ms
			_ -> ms
		) []
	pairs [] = []
	pairs (x1:x2:xs) = (x1,x2) : pairs xs
	pairs _ = error "You must pass in arguments in pairs of <input-file> <record-name>"
