{-# LANGUAGE QuasiQuotes #-}

module Language.Java.Rules.NamingConventions (check) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Foldable (toList)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.Extra (none)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import Language.Java.Syntax.Ident as Ident
import qualified Language.Java.Syntax.Modifier as Modifier
import Language.Java.Syntax.VarDecl as VarDecl
import Language.Java.Syntax.VarDeclId as VarDeclId
import qualified Markdown
import qualified RDF
import Text.RE.TDFA.String

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  concatMap
    (\c -> c cUnit path)
    [ checkPackageNames,
      checkMethodNames,
      checkParameterNames,
      checkStaticVariableNames,
      checkLocalNames,
      checkMemberNames,
      checkTypeNames
    ]

{- Package Name -}

checkPackageNames :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkPackageNames (CompilationUnit (Just pDecl) _ _) path = checkTLD (extractPackageNames pDecl) path
checkPackageNames (CompilationUnit {}) _ = []

checkTLD :: NonEmpty Ident -> FilePath -> [RDF.Diagnostic]
checkTLD (ident :| idents) path =
  checkIdent path (Package First) ident ++ concatMap (checkIdent path (Package Rest)) idents

extractPackageNames :: PackageDecl -> NonEmpty Ident
extractPackageNames (PackageDecl (Name _ packageNames)) = packageNames

{- MethodName -}

checkMethodNames :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkMethodNames cUnit path = do
  MethodDecl _ _ _ _ ident _ _ _ _ <- universeBi cUnit :: [MemberDecl Parsed]
  checkIdent path Method ident

{- ParameterName -}

checkParameterNames :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkParameterNames cUnit path = do
  memberDecl <- universeBi cUnit
  formalParam <- extractBody memberDecl
  checkIdent path Parameter formalParam
  where
    extractBody :: MemberDecl Parsed -> [Ident]
    extractBody (MethodDecl _ _ _ _ _ formalParams _ _ _) =
      map (\(FormalParam _ _ _ _ varDeclIds) -> VarDeclId.ident varDeclIds) formalParams
    extractBody _ =
      mzero

{- StaticVariableName -}

checkStaticVariableNames :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkStaticVariableNames cUnit path = do
  fieldName <- universeBi cUnit
  (ident, modifier) <- extractMemberDecl fieldName
  checkIdent path (StaticVariable modifier) ident
  where
    extractMemberDecl :: MemberDecl Parsed -> [(Ident, VariableModifier)]
    extractMemberDecl (FieldDecl _ modifier _ varDecls)
      | any Modifier.isStatic modifier =
          map (\varDecl -> (VarDecl.ident varDecl, variableModifier modifier)) (NonEmpty.toList varDecls)
      | otherwise = mzero
    extractMemberDecl _ = mzero

{- LocalFinalVariableName & LocalVariableName -}

checkLocalNames :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkLocalNames cUnit path = do
  (ident, modifier) <- localVars cUnit <|> forVars cUnit
  checkIdent path (LocalVariable modifier) ident

localVars :: CompilationUnit Parsed -> [(Ident, VariableModifier)]
localVars cUnit = do
  blockStmt <- universeBi cUnit
  extractLocalVars blockStmt
  where
    extractLocalVars :: BlockStmt Parsed -> [(Ident, VariableModifier)]
    extractLocalVars (LocalVars _ modifiers _ varDecls) =
      toList (fmap (\varDecl -> (VarDecl.ident varDecl, variableModifier modifiers)) varDecls)
    extractLocalVars _ = mzero

forVars :: CompilationUnit Parsed -> [(Ident, VariableModifier)]
forVars cUnit = do
  stmt <- universeBi cUnit
  extractForVars stmt
  where
    extractForVars :: Stmt Parsed -> [(Ident, VariableModifier)]
    extractForVars (BasicFor _ (Just (ForLocalVars modifiers _ varDecls)) _ _ _) =
      toList (fmap (\varDecl -> (VarDecl.ident varDecl, variableModifier modifiers)) varDecls)
    extractForVars (EnhancedFor _ modifiers _ ident _ _) =
      pure (ident, variableModifier modifiers)
    extractForVars _ = mzero

{- MemberName -}

checkMemberNames :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkMemberNames cUnit path = do
  fieldNames <- universeBi cUnit
  member <- extractMemberDecl fieldNames
  checkIdent path InstanceVariable member
  where
    extractMemberDecl :: MemberDecl Parsed -> [Ident]
    extractMemberDecl (FieldDecl _ modifier _ varDecls)
      | none Modifier.isStatic modifier = map VarDecl.ident (NonEmpty.toList varDecls)
      | otherwise = mzero
    extractMemberDecl _ = mzero

{- TypeName -}

checkTypeNames :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
checkTypeNames cUnit path = do
  typeName <- extractCLassesAndInterfaces cUnit ++ extractEnums cUnit
  checkIdent path Type typeName

extractCLassesAndInterfaces :: CompilationUnit Parsed -> [Ident]
extractCLassesAndInterfaces cUnit = do
  classesAndInterfaces <- universeBi cUnit
  extractClassAndInterface classesAndInterfaces
  where
    extractClassAndInterface :: TypeDecl Parsed -> [Ident]
    extractClassAndInterface (ClassTypeDecl (ClassDecl _ _ ident _ _ _ _)) = pure ident
    extractClassAndInterface (InterfaceTypeDecl (InterfaceDecl _ _ _ ident _ _ _ _)) = pure ident
    extractClassAndInterface _ = mzero

extractEnums :: CompilationUnit Parsed -> [Ident]
extractEnums cUnit = do
  enums <- universeBi cUnit
  extractEnum enums
  where
    extractEnum :: ClassDecl Parsed -> [Ident]
    extractEnum (EnumDecl _ _ ident _ _) = pure ident
    extractEnum _ = mzero

nameDiagnostic :: FilePath -> VariableKind -> Convention -> Ident -> RDF.Diagnostic
nameDiagnostic path variableKind convention ident =
  RDF.rangeDiagnostic
    "Language.Java.Rules.NamingConventions"
    [ "Der Name",
      Markdown.code (Ident.name ident),
      "entspricht nicht den Java-Namenskonventionen.",
      "Der Name",
      variableKindToString variableKind,
      "sollte",
      conventionToString convention,
      "verwenden."
    ]
    (sourceSpan ident)
    path

{- -}

checkIdent :: FilePath -> VariableKind -> Ident -> [RDF.Diagnostic]
checkIdent path variableKind ident
  | matched (Ident.name ident ?=~ regex conv) = mzero
  | otherwise = pure (nameDiagnostic path variableKind conv ident)
  where
    conv = convention variableKind

{- -}

data VariableModifier
  = FinalVariable
  | NonFinalVariable

variableModifier :: [Modifier p] -> VariableModifier
variableModifier modifiers
  | any Modifier.isFinal modifiers = FinalVariable
  | otherwise = NonFinalVariable

data Part
  = First
  | Rest

data VariableKind
  = Package Part
  | Type
  | Method
  | StaticVariable VariableModifier
  | Parameter
  | InstanceVariable
  | LocalVariable VariableModifier

variableKindToString :: VariableKind -> String
variableKindToString (Package part) = "des " ++ partToString part ++ " Teils eines Pakets"
  where
    partToString First = "ersten"
    partToString Rest = "restlichen"
variableKindToString Type = "eines Typs"
variableKindToString Method = "einer Methode"
variableKindToString (StaticVariable modifier) = "einer statischen, " ++ modifierToString modifier ++ " Variable"
variableKindToString Parameter = "eines Parameters"
variableKindToString InstanceVariable = "einer Instanzvariable"
variableKindToString (LocalVariable modifier) = "einer lokalen, " ++ modifierToString modifier ++ " Variable"

modifierToString :: VariableModifier -> String
modifierToString FinalVariable = "finalen"
modifierToString NonFinalVariable = "nicht finalen"

convention :: VariableKind -> Convention
convention (Package First) = PackageCase Lower
convention (Package Rest) = PackageCase LowerAndUpper
convention Type = PascalCase
convention Method = CamelCase
convention (StaticVariable FinalVariable) = UnderscoreCase
convention (StaticVariable NonFinalVariable) = CamelCase
convention Parameter = CamelCase
convention InstanceVariable = CamelCase
convention (LocalVariable FinalVariable) = CamelCase
convention (LocalVariable NonFinalVariable) = CamelCase

data LetterCase
  = Lower
  | LowerAndUpper

data Convention
  = PascalCase
  | CamelCase
  | PackageCase LetterCase
  | UnderscoreCase

conventionToString :: Convention -> String
conventionToString PascalCase = "\"Camel Case\" mit einem großen Anfangsbuchstaben"
conventionToString CamelCase = "\"Camel Case\" mit einem kleinen Anfangsbuchstaben"
conventionToString (PackageCase letterCase) = "Java-Paket-Stil mit " ++ letterCaseToString letterCase ++ " Buchstaben"
  where
    letterCaseToString Lower = "kleinen"
    letterCaseToString LowerAndUpper = "kleinen und großen"
conventionToString UnderscoreCase = "\"Underscore Case\""

{- Regular Expressions -}

regex :: Convention -> RE
regex PascalCase = [re|^[A-Z][a-zA-Z0-9]*$|]
regex CamelCase = [re|^[a-z][a-zA-Z0-9]*$|]
regex (PackageCase Lower) = [re|^[a-z]*$|]
regex (PackageCase LowerAndUpper) = [re|^[a-zA-Z_][a-zA-Z0-9_]*$|]
regex UnderscoreCase = [re|^[A-Z][A-Z0-9]*(_[A-Z0-9]+)*$|]
