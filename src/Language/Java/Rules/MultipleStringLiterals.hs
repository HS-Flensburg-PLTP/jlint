module Language.Java.Rules.MultipleStringLiterals where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (groupBy, intercalate, sortBy)
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let annotationStringLiterals = do
        annotation <- universeBi cUnit
        checkAnnotation annotation
  let stringLiterals = do
        strLit@(String _ _) <- universeBi cUnit
        return strLit
  let validLiterals = filter (\lit -> not (any (eq IncludeSourceSpan lit) annotationStringLiterals)) stringLiterals
  checkForDuplicates validLiterals path

checkAnnotation :: Annotation Parsed -> [Literal]
checkAnnotation annotation = do
  strLit@(String _ _) <- universeBi annotation
  return strLit

checkForDuplicates :: [Literal] -> FilePath -> [RDF.Diagnostic]
checkForDuplicates literals path =
  concatMap
    ( ( \(x : xs) ->
          if not (null xs)
            then
              return
                ( RDF.rangeDiagnostic
                    "Language.Java.Rules.MultipleStringLiterals"
                    ( "Das String-Literal "
                        ++ Markdown.code (prettyPrint x)
                        ++ " wird an dieser und an "
                        ++ show (length xs)
                        ++ " weiteren Stellen verwendet. Auch noch in den Zeilen "
                        ++ intercalate ", " (map ((\(Location _ line _, _) -> show line) . sourceSpan) xs)
                        ++ ". Bitte eine Konstante dafür einführen."
                    )
                    (sourceSpan x)
                    path
                )
            else mzero
      )
        . sortBy (\l r -> compare (sourceSpan l) (sourceSpan r))
    )
    ( groupBy
        (\(String _ strL) (String _ strR) -> strL == strR)
        ( sortBy
            (\(String _ strL) (String _ strR) -> compare strL strR)
            literals
        )
    )
