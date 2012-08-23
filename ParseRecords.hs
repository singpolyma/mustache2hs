module ParseRecords (extractRecords, MuType(..), Records, Record, Field) where

import Data.List
import Data.Maybe
import Control.Arrow

import Language.Haskell.Parser
import Language.Haskell.Syntax

import Data.Text (Text)
import qualified Data.Text as T

type Records = [(String, Record)] -- Typename, data constructor
type Record = (String, [Field]) -- Data constructor name, fields
type Field = (Text, MuType)
data MuType = MuList String | MuLambda | MuVariable | MuBool | MuNum deriving (Show, Eq)

isDataDecl :: HsDecl -> Bool
isDataDecl (HsDataDecl {}) = True
isDataDecl _ = False

isTypeDecl :: HsDecl -> Bool
isTypeDecl (HsTypeDecl {}) = True
isTypeDecl _ = False

hsNameToString :: HsName -> String
hsNameToString (HsIdent s) = s
hsNameToString (HsSymbol s) = s

extractTypeFromBangType :: HsBangType -> HsType
extractTypeFromBangType (HsBangedTy t) = t
extractTypeFromBangType (HsUnBangedTy t) = t

hsTypeName' :: [(String, HsType)] -> HsType -> Maybe String
hsTypeName' types (HsTyCon (UnQual s)) | isJust $ lookup (hsNameToString s) types =
	hsTypeName' types =<< lookup (hsNameToString s) types
hsTypeName' _ (HsTyCon (UnQual s)) = Just $ hsNameToString s
hsTypeName' _ t = Nothing

hsTypeName :: [(String, HsType)] -> HsType -> String
hsTypeName types t =
	fromMaybe (error $ "Trying to get type name for: " ++ show t)
		(hsTypeName' types t)

hsTypeToMuType :: [(String, HsType)] -> HsType -> MuType
hsTypeToMuType types (HsTyApp (HsTyCon (Special HsListCon)) t) = MuList (hsTypeName types t)
hsTypeToMuType _ (HsTyFun {}) = MuLambda
hsTypeToMuType types (HsTyCon (UnQual s)) | isJust $ lookup (hsNameToString s) types =
	hsTypeToMuType types $ fromJust $ lookup (hsNameToString s) types
hsTypeToMuType types t | hsTypeName' types t == Just "Bool" = MuBool
hsTypeToMuType types t | hsTypeName' types t `elem` map Just [
		"Int", "Int8", "Int16", "Int32", "Int64", "Integer", "Word", "Word8",
		"Word16", "Word32", "Word64", "Double", "Float", "Rational"
	] = MuNum
hsTypeToMuType _ _ = MuVariable

extractFromField :: [(String, HsType)] -> ([HsName], HsBangType) -> Field
extractFromField types (name, t) =
	(T.pack $ concatMap hsNameToString name, hsTypeToMuType types $ extractTypeFromBangType t)

extractFromRecordConstructor :: [(String, HsType)] -> HsConDecl -> Record
extractFromRecordConstructor types (HsRecDecl _ cname fields) =
	(hsNameToString cname, map (extractFromField types) fields)
extractFromRecordConstructor _ _ = error "Only single data-constructor records may be used as context"

extractFromDataDecl :: [(String, HsType)] -> HsDecl -> (String, Record)
extractFromDataDecl types (HsDataDecl _ _ typeName _ [constructor] _) =
	(hsNameToString typeName, extractFromRecordConstructor types constructor)
extractFromDataDecl _ (HsDataDecl _ _ typeName  _ _ _) =
	(hsNameToString typeName, error "Only single data-constructor records may be used as context")
extractFromDataDecl _ _ = error "Programmer error, only call extractFromDataDecl with DataDecl"

extractFromTypeDecl :: HsDecl -> (String, HsType)
extractFromTypeDecl (HsTypeDecl _ name _ t) = (hsNameToString name, t)
extractFromTypeDecl _ = error "Programmer error, only call extractFromTypeDecl with TypeDecl"

extractRecords :: String -> (String, Records, [(String, Maybe String)])
extractRecords moduleSrc =
	(mod, map (extractFromDataDecl types) datas, simpleTypes)
	where
	simpleTypes = map (second $ hsTypeName' types) types
	types = map extractFromTypeDecl typeDecls
	(typeDecls, datas) = partition (isTypeDecl) $ filter (\d -> isDataDecl d || isTypeDecl d) decls
	ParseOk (HsModule _ (Module mod) _ _ decls) = parseModule moduleSrc
