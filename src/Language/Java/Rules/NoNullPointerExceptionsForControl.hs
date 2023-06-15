module Language.Java.Rules.NoNullPointerExceptionsForControl (check) where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  blocks <- universeBi cUnit
  checkStatement blocks
  where
    checkStatement (Try range _ _ catches _) =
      if any catchesNullPointerException catches
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.NoExceptionsForControl"
                "NullPointerExceptions sollten nicht verwendet werden, um den Kontrollfluss einer Methode zu implementieren. Stattdessen sollte an der richtigen Stelle explizit auf `null` geprüft werden."
                range
                path
            )
        else mzero
    checkStatement _ = mzero

catchesNullPointerException :: Catch Parsed -> Bool
catchesNullPointerException (Catch (FormalParam _ _ (RefType (ClassRefType (ClassType typeName))) _ _) _) =
  isNullPointerExceptionName (map fst typeName)
catchesNullPointerException _ = False

isNullPointerExceptionName :: [Ident] -> Bool
isNullPointerExceptionName [Ident _ "NullPointerException"] = True
isNullPointerExceptionName [Ident _ "java", Ident _ "lang", Ident _ "NullPointerException"] = True
isNullPointerExceptionName _ = False
