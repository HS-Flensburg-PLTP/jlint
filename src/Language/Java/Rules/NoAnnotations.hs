module Language.Java.Rules.NoAnnotations (check) where

import Control.Monad (mzero)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Maybe (mapMaybe)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan (sourceSpan)
import Language.Java.Syntax
import qualified Language.Java.Syntax.Annotation.Extra as Annotation
import qualified RDF

check :: [String] -> CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check whitelist cUnit path = mapMaybe (checkAnnotation path whitelist) (universeBi cUnit)

checkAnnotation :: FilePath -> [String] -> Annotation Parsed -> Maybe RDF.Diagnostic
checkAnnotation path whitelist annotation =
  let name = prettyPrint (Annotation.name annotation)
   in if name `elem` whitelist
        then mzero
        else
          return
            ( RDF.rangeDiagnostic
                "Language.Java.Rules.NoAnnotations"
                ["Die Nutzung der Annotation", name, "ist nicht erlaubt."]
                (sourceSpan annotation)
                path
            )
