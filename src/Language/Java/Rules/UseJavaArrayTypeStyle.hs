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
    checkVarDeclId (VarDeclArray (VarId _)) =
      return
        ( RDF.rangeDiagnostic
            "Language.Java.Rules.UseJavaArrayTypeStyle"
            "Array Typen sollten in Java-Style und nicht C-Style definiert werden. Die Arrayklammern `[]` gehören also hinter den Typen und nicht hinter den Namen."
            dummySourceSpan
            path
        )
    checkVarDeclId _ =
      mzero
