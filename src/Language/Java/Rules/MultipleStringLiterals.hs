module Language.Java.Rules.MultipleStringLiterals where

import Control.Monad (MonadPlus (..))
import Data.Generics.Uniplate.Data (universeBi)
import Data.List (groupBy, sortBy)
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
    ( \xs ->
        if length xs > 1
          then do
            (sourceSpan, string) <- xs
            return
              ( RDF.rangeDiagnostic
                  "Language.Java.Rules.MultipleStringLiterals"
                  ( "Das String-Literal "
                      ++ Markdown.code string
                      ++ " wird mehrfach verwendet. Bitte eine Konstante dafür einführen."
                  )
                  sourceSpan
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
