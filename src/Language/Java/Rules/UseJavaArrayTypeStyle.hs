module Language.Java.Rules.UseJavaArrayTypeStyle (check) where

import Control.Monad (mplus, mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.SourceSpan (SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  fmap
    (message path)
    (mapMaybe varDeclSourceSpan (universeBi cUnit) `mplus` mapMaybe paramSourceSpan (universeBi cUnit))
  where
    varDeclSourceSpan :: VarDecl Parsed -> Maybe SourceSpan
    varDeclSourceSpan (VarDecl _ (VarDeclArray span _) _) = return span
    varDeclSourceSpan _ = mzero

    paramSourceSpan :: FormalParam Parsed -> Maybe SourceSpan
    paramSourceSpan (FormalParam _ _ _ _ (VarDeclArray span _)) = return span
    paramSourceSpan _ = mzero

message :: FilePath -> SourceSpan -> RDF.Diagnostic
message path span =
  RDF.rangeDiagnostic
    "Language.Java.Rules.UseJavaArrayTypeStyle"
    [ "Array-Typen sollten im Java-Stil und nicht im C-Stil definiert werden.",
      "Die Array-Klammern",
      Markdown.code "[]",
      "gehören also hinter den Typ und nicht hinter den Namen der Variable."
    ]
    span
    path
