module Language.Java.Rules.ModifiedControlVariable where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Pretty
import Language.Java.Syntax
import Language.Java.Syntax.VarDecl (ident)
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkBasicFor stmt path

checkBasicFor :: Stmt -> FilePath -> [RDF.Diagnostic]
checkBasicFor (BasicFor (Just (ForLocalVars _ _ forVarDecls)) _ _ forStmt) path = do
  expr <- universeBi forStmt
  checkBasicForModifiedControlVariable expr
  where
    checkBasicForModifiedControlVariable (Assign sourceSpan (NameLhs (Name [controlVariable])) _ _) =
      map
        (rangeDiag sourceSpan)
        (varDeclIdentInExpr controlVariable)
    checkBasicForModifiedControlVariable (PostIncrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (rangeDiag sourceSpan)
        (varDeclIdentInExpr controlVariable)
    checkBasicForModifiedControlVariable (PreIncrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (rangeDiag sourceSpan)
        (varDeclIdentInExpr controlVariable)
    checkBasicForModifiedControlVariable (PostDecrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (rangeDiag sourceSpan)
        (varDeclIdentInExpr controlVariable)
    checkBasicForModifiedControlVariable (PreDecrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (rangeDiag sourceSpan)
        (varDeclIdentInExpr controlVariable)
    checkBasicForModifiedControlVariable _ = mzero

    rangeDiag = createRangeDiagnostic path
    varDeclIdentInExpr = checkVarDeclIdentInExp forVarDecls
checkBasicFor _ _ = mzero

checkVarDeclIdentInExp :: [VarDecl] -> Ident -> [VarDecl]
checkVarDeclIdentInExp varDecls controlVariable = filter (\varDecl -> ident varDecl == controlVariable) varDecls

createRangeDiagnostic :: FilePath -> SourceSpan -> VarDecl -> RDF.Diagnostic
createRangeDiagnostic path sourceSpan varDecl =
  RDF.rangeDiagnostic
    "Language.Java.Rules.ModifiedControlVariable"
    ("Laufvariable " ++ prettyPrint (ident varDecl) ++ " darf nicht innerhalb der Schleife modifiziert werden!")
    sourceSpan
    path
