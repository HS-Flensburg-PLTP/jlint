{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.ModifiedControlVariable where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Language.Java.Pretty
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified RDF
import qualified Data.List.NonEmpty as NonEmpty

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkBasicFor stmt path

checkBasicFor :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkBasicFor (BasicFor _ (Just (ForLocalVars _ _ forVarDecls)) _ _ forStmt) path = do
  expr :: Exp Parsed <- universeBi forStmt
  checkBasicForExprForModifiedControlVariable expr
  where
    controlVariables = NonEmpty.map VarDecl.ident forVarDecls

    checkBasicForExprForModifiedControlVariable (Assign sourceSpan (NameLhs (Name _ (exprIdent :| []))) _ _) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PostIncrement sourceSpan (ExpName (Name _ (exprIdent :| [])))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PreIncrement sourceSpan (ExpName (Name _ (exprIdent :| [])))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PostDecrement sourceSpan (ExpName (Name _ (exprIdent :| [])))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PreDecrement sourceSpan (ExpName (Name _ (exprIdent :| [])))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable _ = mzero
checkBasicFor _ _ = mzero

checkIdent :: Ident -> NonEmpty Ident -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkIdent ident idents sourceSpan path =
  if any (eq IgnoreSourceSpan ident) idents
    then return (createRangeDiagnostic ident sourceSpan path)
    else mzero

createRangeDiagnostic :: Ident -> SourceSpan -> FilePath -> RDF.Diagnostic
createRangeDiagnostic ident =
  RDF.rangeDiagnostic
    "Language.Java.Rules.ModifiedControlVariable"
    ("Laufvariable " ++ prettyPrint ident ++ " darf nicht innerhalb der Schleife modifiziert werden!")
