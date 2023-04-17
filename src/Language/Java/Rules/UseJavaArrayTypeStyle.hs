module Language.Java.Rules.UseJavaArrayTypeStyle where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  varDeclId <- universeBi cUnit
  checkVarDeclId varDeclId
  where
    checkVarDeclId (VarDeclArray span (VarId _)) =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.UseJavaArrayTypeStyle"
            "Array-Typen sollten im Java-Stil und nicht im C-Stil definiert werden. Die Arrayklammern `[]` gehören also hinter den Typ und nicht hinter den Namen der Variable."
            span
            path
        )
    checkVarDeclId _ =
      mzero
