module Language.Java.Rules.AvoidMultipleTopLevelDecl (check) where
import Control.Monad (MonadPlus(mzero))
import Language.Java.Syntax
import qualified RDF
import Data.Generics.Uniplate.Data (universeBi)

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  comUnit <- universeBi cUnit
  checkComUnit comUnit
  where
    checkComUnit (CompilationUnit _ _ typeDecls ) =
        if length typeDecls > 1
            then
                [
                    RDF.rangeDiagnostic
                    "Language.Java.Rules.AvoidMultipleTopLevelDecl"
                    "Jede Datei sollte immer nur eine Top-Level-Klasse beinhalten."
                    dummySourceSpan
                    path
                ]
            else mzero
