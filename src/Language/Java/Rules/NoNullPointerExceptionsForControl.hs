module Language.Java.Rules.NoNullPointerExceptionsForControl (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = mapMaybe checkStatement (universeBi cUnit)
  where
    checkStatement (Try range _ _ catches _) =
      if any catchesNullPointerException catches
        then
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.NoExceptionsForControl"
                [ "Eine",
                  Markdown.code "NullPointerException",
                  "sollte nicht verwendet werden, um den Kontrollfluss einer Methode zu implementieren.",
                  "Stattdessen sollte an der richtigen Stelle explizit auf",
                  Markdown.code "null",
                  "geprüft werden."
                ]
                range
                path
            )
        else mzero
    checkStatement _ = mzero

catchesNullPointerException :: Catch Parsed -> Bool
catchesNullPointerException (Catch (FormalParam _ _ (RefType (ClassRefType (ClassType typeName))) _ _) _) =
  isNullPointerExceptionName (fmap fst typeName)
catchesNullPointerException _ = False

isNullPointerExceptionName :: NonEmpty Ident -> Bool
isNullPointerExceptionName (Ident _ "NullPointerException" :| []) = True
isNullPointerExceptionName (Ident _ "java" :| [Ident _ "lang", Ident _ "NullPointerException"]) = True
isNullPointerExceptionName _ = False
