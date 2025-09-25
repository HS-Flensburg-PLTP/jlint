module Language.Java.Rules.UseLocalTypeInference (check) where

import Control.Monad (mzero)
import Data.Function ((&))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Maybe as Maybe
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Ident as Ident
import Language.Java.Syntax.VarDecl as VarDecl (ident)
import qualified Markdown
import QualifiedIdent (QualifiedIdent (..))
import qualified RDF
import qualified String

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  Maybe.mapMaybe checkLocalVar (universeBi cUnit)
  where
    -- ++ Map.maybe checkForLocalVar (universeBi cUnit)

    checkLocalVar :: BlockStmt Parsed -> Maybe RDF.Diagnostic
    checkLocalVar (LocalVars span _ type_ varDecls)
      | isVarType type_ = fmap (implicitObjectError path span) (implicitObjectInVarDecls varDecls)
      | otherwise = fmap (useVarError path span) (useVarInVarDecls varDecls)
    checkLocalVar (BlockStmt _) =
      mzero
    checkLocalVar (LocalClass _) =
      mzero

{-
The constructor ForLocalVars does currently not provide a span
checkForLocalVar :: ForInit Parsed -> Maybe (SourceSpan, NonEmpty Ident)
checkForLocalVar (ForLocalVars span _ type_ varDecls)
  | isVarType type_ = mzero
  | otherwise = pure (undefined, fmap VarDecl.ident varDecls)
checkForLocalVar (ForInitExps _) =
  mzero
-}

isVarType :: Type -> Bool
isVarType (RefType (ClassRefType (ClassType _ ((Ident _ "var", []) :| [])))) = True
isVarType _ = False

useVarInVarDecls :: NonEmpty (VarDecl Parsed) -> Maybe (NonEmpty (VarDecl Parsed))
useVarInVarDecls varDecls =
  varDecls
    & List.NonEmpty.filter (isGoodVarInit . rightHandSide)
    & List.NonEmpty.nonEmpty

implicitObjectInVarDecls :: NonEmpty (VarDecl p) -> Maybe (NonEmpty (VarDecl p))
implicitObjectInVarDecls varDecls =
  varDecls
    & List.NonEmpty.filter (hasImplicitObject . rightHandSide)
    & List.NonEmpty.nonEmpty

rightHandSide :: VarDecl p -> Maybe (VarInit p)
rightHandSide (VarDecl _ _ maybeVarInit) = maybeVarInit

isGoodVarInit :: Maybe (VarInit Parsed) -> Bool
isGoodVarInit Nothing = False
isGoodVarInit (Just (InitArray _)) = False
isGoodVarInit (Just (InitExp Lambda {})) = False
isGoodVarInit (Just (InitExp MethodRef {})) = False
isGoodVarInit (Just (InitExp (MethodInv invocation))) = isGoodMethodInvocation invocation
isGoodVarInit (Just (InitExp _)) = True

data QualifiedName = QualifiedName String String
  deriving (Eq)

isGoodMethodInvocation :: MethodInvocation Parsed -> Bool
isGoodMethodInvocation (MethodCall _ (Just (Name _ (Ident _ className :| []))) (Ident _ methodName) _)
  | QualifiedIdent className methodName `elem` badMethods = False
  | otherwise = True
isGoodMethodInvocation _ = True

badMethods :: [QualifiedIdent]
badMethods =
  [ QualifiedIdent "Arrays" "asList",
    QualifiedIdent "List" "of"
  ]

hasImplicitObject :: Maybe (VarInit p) -> Bool
hasImplicitObject Nothing = False
hasImplicitObject (Just (InitArray _)) = False
hasImplicitObject (Just (InitExp (InstanceCreation _ _ typeDeclSpecifier _ _))) =
  isDiamond typeDeclSpecifier
hasImplicitObject (Just (InitExp _)) = False

isDiamond :: TypeDeclSpecifier -> Bool
isDiamond (TypeDeclSpecifier _) = False
isDiamond (TypeDeclSpecifierWithDiamond {}) = True
isDiamond (TypeDeclSpecifierUnqualifiedWithDiamond {}) = True

useVarError :: FilePath -> SourceSpan -> NonEmpty (VarDecl p) -> RDF.Diagnostic
useVarError path span varDecls =
  RDF.rangeDiagnostic
    "Language.Java.Rules.UseLocalTypeInference"
    [ "Der Typ der",
      String.plural (List.NonEmpty.length varDecls) "Variable" "Variablen",
      String.enumerate (fmap (Markdown.code . Ident.name . VarDecl.ident) (List.NonEmpty.toList varDecls)),
      "kann durch lokale Typinferenz inferiert werden. Daher sollte der explizite Typ durch",
      Markdown.code "var",
      "ersetzt werden."
        ++ if any (containsDiamond . rightHandSide) varDecls
          then " Der Diamond-Operator muss dabei allerdings durch einen expliziten Typ ersetzt werden, da dieser Typ sonst als Object inferiert wird."
          else ""
    ]
    span
    path

containsDiamond :: Maybe (VarInit p) -> Bool
containsDiamond Nothing = False
containsDiamond (Just (InitArray _)) = False
containsDiamond (Just (InitExp (InstanceCreation _ _ (TypeDeclSpecifier _) _ _))) = False
containsDiamond (Just (InitExp (InstanceCreation _ _ (TypeDeclSpecifierUnqualifiedWithDiamond {}) _ _))) = True
containsDiamond (Just (InitExp (InstanceCreation _ _ (TypeDeclSpecifierWithDiamond {}) _ _))) = True
containsDiamond (Just (InitExp _)) = False

implicitObjectError :: FilePath -> SourceSpan -> NonEmpty (VarDecl p) -> RDF.Diagnostic
implicitObjectError path span varDecls =
  RDF.rangeDiagnostic
    "Language.Java.Rules.UseLocalTypeInference"
    [ "Ein Teil des Typs der",
      String.plural (List.NonEmpty.length varDecls) "Variable" "Variablen",
      String.enumerate (fmap (Markdown.code . Ident.name . VarDecl.ident) (List.NonEmpty.toList varDecls)),
      "wird durch die lokale Typinferenz als",
      Markdown.code "Object",
      "inferiert. Daher sollte",
      Markdown.code "var",
      "durch einen expliziten Typ ersetzt werden."
    ]
    span
    path
