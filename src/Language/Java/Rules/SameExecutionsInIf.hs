module Language.Java.Rules.SameExecutionsInIf (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  (IfThenElse span _ stmt1 stmt2) <- universeBi cUnit
  compareStmts stmt1 stmt2 span path

compareStmts :: Stmt Parsed -> Stmt Parsed -> SourceSpan -> FilePath -> [RDF.Diagnostic]
compareStmts stmt1 stmt2 span path
  | eq IgnoreSourceSpan stmt1 stmt2 =
      [RDF.rangeDiagnostic "Language.Java.Rules.SameExecutionsInIf" "Die Fallunterscheidung ist unnötig, da der Code identisch ist." span path]
compareStmts (StmtBlock (Block blockstmts)) (StmtBlock (Block blockstmts2)) span path
  | eq IgnoreSourceSpan (head blockstmts) (head blockstmts2) || eq IgnoreSourceSpan (last blockstmts) (last blockstmts2) =
      [RDF.rangeDiagnostic "Language.Java.Rules.SameExecutionsInIf" "In dem If Else Block befindet sich identischer Code, welcher ausgelagert werden kann." span path]
compareStmts _ _ _ _ = mzero
