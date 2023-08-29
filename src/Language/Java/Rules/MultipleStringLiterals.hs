{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Java.Rules.MultipleStringLiterals where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (groupBy, intercalate, sortBy)
import Data.List.Extra
import qualified Data.Maybe as Maybe
import Language.Java.Pretty (prettyPrint)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified Language.Java.Syntax.Literal as Literal
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  let annotationStringLiterals = do
        (annotation :: Annotation Parsed) <- universeBi cUnit
        filter Literal.isString (universeBi annotation)
      stringLiterals = filter Literal.isString (universeBi cUnit)
      validLiterals =
        filter
          (\lit -> Maybe.fromJust (Literal.string lit) /= "")
          ( filter
              (\lit -> none (eq IncludeSourceSpan lit) annotationStringLiterals)
              stringLiterals
          )
   in checkForDuplicates validLiterals path

checkForDuplicates :: [Literal] -> FilePath -> [RDF.Diagnostic]
checkForDuplicates literals path =
  concatMap
    ( ( \case
          [] -> mzero
          x : xs ->
            if null xs
              then mzero
              else
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
      )
        . sortBy (\l r -> compare (sourceSpan l) (sourceSpan r))
    )
    ( groupBy
        ( \strL strR ->
            Maybe.fromJust (Literal.string strL) == Maybe.fromJust (Literal.string strR)
        )
        ( sortBy
            ( \strL strR ->
                compare (Maybe.fromJust (Literal.string strL)) (Maybe.fromJust (Literal.string strR))
            )
            literals
        )
    )
