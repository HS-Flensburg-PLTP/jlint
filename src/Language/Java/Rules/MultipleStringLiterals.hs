module Language.Java.Rules.MultipleStringLiterals where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let exps = universeBi cUnit
  let pairs = concatMap checkStringLiteral exps
  checkForDuplicates pairs [] path

checkStringLiteral :: Exp Parsed -> [(SourceSpan, String)]
checkStringLiteral (Lit (String string)) = [(dummySourceSpan, string)]
checkStringLiteral _ = []

checkForDuplicates :: [(SourceSpan, String)] -> [(SourceSpan, String)] -> FilePath -> [RDF.Diagnostic]
checkForDuplicates ((sourceSpan, string) : xs) precedors path =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.MultipleStringLiterals"
      ( "Mehrfaches Vorkommen gleicher Stringliterale (== Inhalt von Strings gleich) nicht erlaubt: '"
          ++ string
          ++ "' Bitte Konstante verwenden."
      )
      sourceSpan
      path
    | string
        `elem` map snd (precedors ++ xs)
  ]
    ++ checkForDuplicates xs (precedors ++ [(sourceSpan, string)]) path
checkForDuplicates [] _ _ = []
