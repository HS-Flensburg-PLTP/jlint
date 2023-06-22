module Language.Java.Rules.MultipleStringLiterals where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified Markdown
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let exps = universeBi cUnit
  let pairs = concatMap checkStringLiteral exps
  checkForDuplicates pairs [] path

checkStringLiteral :: Exp Parsed -> [(SourceSpan, String)]
checkStringLiteral (Lit sourceSpan (String string)) = [(sourceSpan, string)]
checkStringLiteral _ = []

checkForDuplicates :: [(SourceSpan, String)] -> [(SourceSpan, String)] -> FilePath -> [RDF.Diagnostic]
checkForDuplicates ((sourceSpan, string) : xs) predecessors path =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.MultipleStringLiterals"
      ( "Das String-Literal "
          ++ Markdown.code string
          ++ " wird mehrfach verwendet. Bitte eine Konstante dafür einführen."
      )
      sourceSpan
      path
    | string
        `elem` map snd (predecessors ++ xs)
  ]
    ++ checkForDuplicates xs (predecessors ++ [(sourceSpan, string)]) path
checkForDuplicates [] _ _ = []
