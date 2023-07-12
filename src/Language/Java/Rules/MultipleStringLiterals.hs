module Language.Java.Rules.MultipleStringLiterals where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (groupBy, intercalate, sortBy)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let exps = universeBi cUnit
  let pairs = do
        exp <- exps
        checkStringLiteral exp
  checkForDuplicates pairs path

checkStringLiteral :: Exp Parsed -> [(SourceSpan, String)]
checkStringLiteral (Lit sourceSpan (String string)) = [(sourceSpan, string)]
checkStringLiteral _ = []

checkForDuplicates :: [(SourceSpan, String)] -> FilePath -> [RDF.Diagnostic]
checkForDuplicates pairs path =
  concatMap
    ( \(x : xs) ->
        if length (x : xs) > 1
          then do
            let lines = map (\((Location _ line _, _), _) -> show line) xs
            return
              ( RDF.rangeDiagnostic
                  "Language.Java.Rules.MultipleStringLiterals"
                  ( "Das String-Literal "
                      ++ Markdown.code (snd x)
                      ++ " wird an "
                      ++ show (length xs + 1)
                      ++ " Stellen verwendet. Auch noch in den Zeilen "
                      ++ intercalate ", " lines
                      ++ ". Bitte eine Konstante dafür einführen."
                  )
                  (fst x)
                  path
              )
          else mzero
    )
    ( groupBy
        (\l r -> snd l == snd r)
        ( sortBy
            (\l r -> compare (snd l) (snd r))
            pairs
        )
    )
