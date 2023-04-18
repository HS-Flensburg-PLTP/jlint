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
        (createRangeDiag sourceSpan path)
        (checkVarDeclIdentInExp controlVariable forVarDecls)
    checkBasicForModifiedControlVariable (PostIncrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (createRangeDiag sourceSpan path)
        (checkVarDeclIdentInExp controlVariable forVarDecls)
    checkBasicForModifiedControlVariable (PreIncrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (createRangeDiag sourceSpan path)
        (checkVarDeclIdentInExp controlVariable forVarDecls)
    checkBasicForModifiedControlVariable (PostDecrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (createRangeDiag sourceSpan path)
        (checkVarDeclIdentInExp controlVariable forVarDecls)
    checkBasicForModifiedControlVariable (PreDecrement sourceSpan (ExpName (Name [controlVariable]))) =
      map
        (createRangeDiag sourceSpan path)
        (checkVarDeclIdentInExp controlVariable forVarDecls)
    checkBasicForModifiedControlVariable _ = mzero
checkBasicFor _ _ = mzero

checkVarDeclIdentInExp :: Ident -> [VarDecl] -> [VarDecl]
checkVarDeclIdentInExp controlVariable = filter (\varDecl -> ident varDecl == controlVariable)

createRangeDiag :: SourceSpan -> FilePath -> VarDecl -> RDF.Diagnostic
createRangeDiag sourceSpan path varDecl =
  RDF.rangeDiagnostic
    "Language.Java.Rules.ModifiedControlVariable"
    ("Laufvariable " ++ prettyPrint (ident varDecl) ++ " darf nicht innerhalb der Schleife modifiziert werden!")
    sourceSpan
    path
