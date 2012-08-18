module ParseRecords (extractRecords, MuType(..), Records, Record, Field) where

import Data.List
import Data.Maybe

import Language.Haskell.Parser
import Language.Haskell.Syntax

import Data.Text (Text)
import qualified Data.Text as T

type Records = [(String, Record)]
type Record = [Field]
type Field = (Text, MuType)
data MuType = MuList String | MuLambda | MuVariable deriving (Show, Eq)

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

hsTypeName :: [(String, HsType)] -> HsType -> String
hsTypeName types (HsTyCon (UnQual s)) | isJust $ lookup (hsNameToString s) types =
	hsTypeName types $ fromJust $ lookup (hsNameToString s) types
hsTypeName _ (HsTyCon (UnQual s)) = hsNameToString s
hsTypeName _ t = error ("Trying to get type name for: " ++ show t)

hsTypeToMuType :: [(String, HsType)] -> HsType -> MuType
hsTypeToMuType types (HsTyApp (HsTyCon (Special HsListCon)) t) = MuList (hsTypeName types t)
hsTypeToMuType _ (HsTyFun {}) = MuLambda
hsTypeToMuType types (HsTyCon (UnQual s)) | isJust $ lookup (hsNameToString s) types =
	hsTypeToMuType types $ fromJust $ lookup (hsNameToString s) types
hsTypeToMuType _ _ = MuVariable

extractFromField :: [(String, HsType)] -> ([HsName], HsBangType) -> Field
extractFromField types (name, t) =
	(T.pack $ concatMap hsNameToString name, hsTypeToMuType types $ extractTypeFromBangType t)

extractFromRecordConstructor :: [(String, HsType)] -> HsConDecl -> [Field]
extractFromRecordConstructor types (HsRecDecl _ _ fields) = map (extractFromField types) fields
extractFromRecordConstructor _ _ = []

extractFromDataDecl :: [(String, HsType)] -> HsDecl -> (String, Record)
extractFromDataDecl types (HsDataDecl _ _ typeName _ constructors _) =
	(hsNameToString typeName, concatMap (extractFromRecordConstructor types) constructors)
extractFromDataDecl _ _ = error "Programmer error, only call extractFromDataDecl with TypeDecl"

extractFromTypeDecl :: HsDecl -> (String, HsType)
extractFromTypeDecl (HsTypeDecl _ name _ t) = (hsNameToString name, t)
extractFromTypeDecl _ = error "Programmer error, only call extractFromTypeDecl with TypeDecl"

extractRecords :: String -> Records
extractRecords moduleSrc =
	map (extractFromDataDecl (map extractFromTypeDecl types)) datas
	where
	(types, datas) = partition (isTypeDecl) $ filter (\d -> isDataDecl d || isTypeDecl d) decls
	ParseOk (HsModule _ _ _ _ decls) = parseModule moduleSrc
