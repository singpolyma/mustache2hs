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
import Control.Monad (when, join)
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

-- | Available as of base-4.3.0.0 in Control.Monad
void :: Functor f => f a -> f ()
void = fmap (const ())

data Flag = Help | RecordModule String | OutputName String deriving (Show, Read, Eq)

flags :: [OptDescr Flag]
flags = [
		Option ['m'] ["record-module"] (ReqArg RecordModule "MODULE") "Module containing records to be used as context.",
		Option ['o'] ["output-name"] (ReqArg OutputName "NAME") "Name of the output module. (Defaults to MustacheTemplates)",
		Option ['h'] ["help"] (NoArg Help) "Show this help text."
	]

usage :: [String] -> IO ()
usage errors = do
	mapM_ (hPutStrLn stderr) errors
	name <- getProgName
	hPutStrLn stderr $ usageInfo (name ++ " [-m MODULE] [-o NAME] <input-file> <record-name> ...") flags

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
				c1 <- peekChar
				c2 <- peekChar
				if c1 /= Just '}' || c2 /= Just '}' then anyChar else
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
	name = do
		nm <- takeWhile1 (\c -> isAlpha c || isDigit c || c == '_' || c == '.')
		return $ if nm == T.singleton '.' then T.pack "ctx" else nm
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
	maybeDot name
		| name == T.pack "ctx" = Builder.fromString "."
		| otherwise = Builder.fromText name
	origOne (MuText txt) = Builder.fromText txt
	origOne (MuVar name True) = mconcat [
			Builder.fromString "{{",
			maybeDot name,
			Builder.fromString "}}"
		]
	origOne (MuVar name False) = mconcat [
			Builder.fromString "{{{",
			maybeDot name,
			Builder.fromString "}}}"
		]
	origOne (MuSection name tree) = mconcat [
			Builder.fromString "{{#",
			maybeDot name,
			Builder.fromString "}}",
			originalMustache tree,
			Builder.fromString "{{/",
			maybeDot name,
			Builder.fromString "}}"
		]
	origOne (MuSectionInv name tree) = mconcat [
			Builder.fromString "{{^",
			maybeDot name,
			Builder.fromString "}}",
			originalMustache tree,
			Builder.fromString "{{/",
			maybeDot name,
			Builder.fromString "}}"
		]
	origOne _ = mempty

monoidSpecialCase :: Text -> [Field] -> Builder
monoidSpecialCase name allFields = Builder.fromText $
	case lookup name allFields of
		Just MuBool ->
			T.pack "(Any " `mappend` name `mappend` T.pack ")"
		Just MuNum ->
			T.pack "(Sum " `mappend` name `mappend` T.pack ")"
		_ -> name

codeGenTree :: (Show a, Enum a) => FilePath -> Text -> String -> Records -> MuTree -> Word -> [Field] -> State a (Builder, [(FilePath, String)])
codeGenTree path fname rname recs tree level fields = do
	(code, helpers', partials) <- unzip3 <$> mapM (\(pos,m) -> do
			(code, helpers, partials) <- codeGen path rname recs level lFields m
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
			maybe (Builder.fromString "_") pattern rec,
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
	lFields = maybe id (\(_,fs) -> (fs++)) rec fields
	rec = lookup rname recs
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

codeGen :: (Show a, Enum a) => FilePath -> String -> Records -> Word -> [Field] -> Mustache -> State a (Builder, [Builder], [(FilePath, String)])
codeGen _ _ _ _ _ (MuText txt) = return (mconcat [
		Builder.fromString "build ",
		Builder.fromShow (T.unpack txt)
	], [], [])
codeGen _ _ _ _ _ (MuVar name False) = return (mconcat [
		Builder.fromString "build ",
		Builder.fromText name
	], [], [])
codeGen _ _ _ _ _ (MuVar name True) = return (mconcat [
		Builder.fromString "build $ escapeFunction $ TL.unpack $ TL.toLazyText $ build ",
		Builder.fromText name
	], [], [])
codeGen pth _ recs lvl fs (MuSection name stree) = do
	nm <- nextName name
	case lookup name fs of
		Just MuLambda ->
			return (mconcat [
					Builder.fromText name,
					Builder.fromString " (",
					Builder.fromShow $ BS.toString $
						Builder.toByteString $ originalMustache stree,
					Builder.fromString " )"
				], [], [])
		Just (MuList rname) -> do
			(helper, partials) <- codeGenTree pth nm rname recs stree (lvl+1) fs
			return (mconcat [
					Builder.fromString "mconcat $ map (",
					Builder.fromText nm,
					Builder.fromString " escapeFunction) ",
					Builder.fromText name
				], [helper], partials)
		_ -> do
			(helper, partials) <- codeGenTree pth nm "monoid" recs stree (lvl+1) fs
			return (mconcat [
					Builder.fromString "if mempty /= ",
					monoidSpecialCase name fs,
					Builder.fromString " then ",
					Builder.fromText nm,
					Builder.fromString " escapeFunction ",
					Builder.fromText name,
					Builder.fromString " else mempty"
				], [helper], partials)
codeGen pth rname recs lvl fs (MuSectionInv name stree) = do
	nm <- nextName name
	(helper, partials) <- codeGenTree pth nm rname recs stree (lvl+1) fs
	return (mconcat [
			Builder.fromString "if mempty == ",
			monoidSpecialCase name fs,
			Builder.fromString " then ",
			Builder.fromText nm,
			Builder.fromString " escapeFunction ctx else mempty"
		], [helper], partials)
codeGen pth rname _ _ _ (MuPartial name) =
	let
		file = takeDirectory pth </> T.unpack name
		fname = camelCasePath (dropExtension file)
	in
	return (mconcat [
		Builder.fromText fname,
		Builder.fromString " escapeFunction ctx"
	], [], [(file, rname)])
codeGen _ _ _ _ _ _ = return (mempty, [], [])

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
			| r == rname || rname == "monoid" -> return (Nothing, [])
			| r == "monoid" -> do
				modify ((input',rname):)
				return (Nothing, [])
			| otherwise -> fail ("Type mismatch, template " ++ input ++ " expects both " ++ r ++ " and " ++ rname)
		Nothing -> do
			modify ((input',rname):)
			parsed <- lift $ parse (parser <* eof) input <$> T.readFile input
			case parsed of
				Right tree -> do
					let fname = camelCasePath (dropExtension input)
					let (builder, partials) = evalState (codeGenTree input fname rname recs tree 0 []) (0::Int)
					return (Just builder, partials)
				Left msg -> error (show msg)
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
		_ -> main' (getOutputName flags) (getRecordModules flags) (pairs args)
	where
	main' outputName recordModules inputs = do
		(ms, recs, types) <- unzip3 <$> mapM (fmap extractRecords . readFile) recordModules
		let types' = concat types
		let inputs' = map (second (\r -> fromMaybe r (join $ lookup r types'))) inputs
		builder <- evalStateT (codeGenFiles (concat recs) inputs') []
		-- GHC pragma turns off warnings we know about
		-- Should be ignored by other compilers, so is safe
		putStrLn "{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-matches #-}"
		putStrLn $ "module " ++ outputName ++ " where"
		putStrLn ""
		putStrLn "import Data.Monoid"
		putStrLn "import Data.Text.Buildable (build)"
		putStrLn "import qualified Data.Text.Lazy as TL"
		putStrLn "import qualified Data.Text.Lazy.Builder as TL"
		mapM_ (\m -> putStrLn $ "import " ++ m ++ "\n") ms
		Builder.toByteStringIO BS.putStr builder
		putStrLn ""
	getOutputName = foldr (\new cur -> case new of
			OutputName n -> n
			_ -> cur
		) "MustacheTemplates"
	getRecordModules = foldr (\x ms -> case x of
			RecordModule m -> m : ms
			_ -> ms
		) []
	pairs [] = []
	pairs (x1:x2:xs) = (x1,x2) : pairs xs
	pairs _ = error "You must pass in arguments in pairs of <input-file> <record-name>"
