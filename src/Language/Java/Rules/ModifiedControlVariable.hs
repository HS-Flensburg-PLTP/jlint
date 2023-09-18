module Language.Java.Rules.ModifiedControlVariable (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Language.Java.Pretty
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.VarDecl as VarDecl
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  stmt <- universeBi cUnit
  checkBasicFor stmt path

checkBasicFor :: Stmt Parsed -> FilePath -> [RDF.Diagnostic]
checkBasicFor (BasicFor _ (Just (ForLocalVars _ _ forVarDecls)) _ _ forStmt) path = do
  (ident, sourceSpan) <- assignedVariables forStmt
  let controlVariables = NonEmpty.map VarDecl.ident forVarDecls
  if any (eq IgnoreSourceSpan ident) controlVariables
    then
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.ModifiedControlVariable"
            [ "Die Laufvariable",
              Markdown.code (prettyPrint ident),
              "sollte nicht innerhalb der Schleife modifiziert werden."
            ]
            sourceSpan
            path
        )
    else mzero
checkBasicFor _ _ = mzero

assignedVariables :: Stmt Parsed -> [(Ident, SourceSpan)]
assignedVariables stmt = mapMaybe assignedVariablesInExp (universeBi stmt)
  where
    assignedVariablesInExp :: Exp Parsed -> Maybe (Ident, SourceSpan)
    assignedVariablesInExp (Assign sourceSpan (NameLhs (Name _ (ident :| []))) _ _) =
      return (ident, sourceSpan)
    assignedVariablesInExp (PostIncrement sourceSpan (ExpName (Name _ (ident :| [])))) =
      return (ident, sourceSpan)
    assignedVariablesInExp (PreIncrement sourceSpan (ExpName (Name _ (ident :| [])))) =
      return (ident, sourceSpan)
    assignedVariablesInExp (PostDecrement sourceSpan (ExpName (Name _ (ident :| [])))) =
      return (ident, sourceSpan)
    assignedVariablesInExp (PreDecrement sourceSpan (ExpName (Name _ (ident :| [])))) =
      return (ident, sourceSpan)
    assignedVariablesInExp _ =
      mzero
