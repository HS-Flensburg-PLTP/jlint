module Language.Java.Rules.ModifiedControlVariable where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkBasicFor stmt path

checkBasicFor :: Stmt -> FilePath -> [RDF.Diagnostic]
checkBasicFor (BasicFor (Just (ForLocalVars _ _ forVarDecls)) _ _ forStmt) path = do
  expr <- universeBi forStmt
  checkBasicForExprForModifiedControlVariable expr
  where
    controlVariables = map VarDecl.ident forVarDecls

    checkBasicForExprForModifiedControlVariable (Assign sourceSpan (NameLhs (Name [exprIdent])) _ _) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PostIncrement sourceSpan (ExpName (Name [exprIdent]))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PreIncrement sourceSpan (ExpName (Name [exprIdent]))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PostDecrement sourceSpan (ExpName (Name [exprIdent]))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable (PreDecrement sourceSpan (ExpName (Name [exprIdent]))) =
      checkIdent exprIdent controlVariables sourceSpan path
    checkBasicForExprForModifiedControlVariable _ = mzero
checkBasicFor _ _ = mzero

checkIdent :: Ident -> [Ident] -> SourceSpan -> FilePath -> [RDF.Diagnostic]
checkIdent ident idents sourceSpan path =
  if ident `elem` idents
    then return (createRangeDiagnostic ident sourceSpan path)
    else mzero

createRangeDiagnostic :: Ident -> SourceSpan -> FilePath -> RDF.Diagnostic
createRangeDiagnostic ident =
  RDF.rangeDiagnostic
    "Language.Java.Rules.ModifiedControlVariable"
    ("Laufvariable " ++ prettyPrint ident ++ " darf nicht innerhalb der Schleife modifiziert werden!")
