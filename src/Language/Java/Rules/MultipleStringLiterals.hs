module Language.Java.Rules.MultipleStringLiterals where

import Data.Generics.Uniplate.Data (universeBi)
import Language.Java.SourceSpan
import Language.Java.Syntax
import qualified RDF

check :: CompilationUnit -> FilePath -> [RDF.Diagnostic]
check cUnit path = do
  let varDecls = universeBi cUnit
  let assignments = universeBi cUnit
  let pairs = concatMap checkVarDecl varDecls ++ concatMap checkAssigment assignments
  checkForDuplicates pairs pairs path

checkVarDecl :: VarDecl -> [(SourceSpan, String)]
checkVarDecl (VarDecl sourceSpan _ (Just (InitExp (Lit (String string))))) = [(sourceSpan, string)]
checkVarDecl _ = []

checkAssigment :: Exp -> [(SourceSpan, String)]
checkAssigment (Assign sourceSpan (NameLhs _) EqualA (Lit (String string))) = [(sourceSpan, string)]
checkAssigment _ = []

checkForDuplicates :: [(SourceSpan, String)] -> [(SourceSpan, String)] -> FilePath -> [RDF.Diagnostic]
checkForDuplicates ((sourceSpan, string) : xs) allPairs path =
  [ RDF.rangeDiagnostic
      "Language.Java.Rules.MultipleStringLiterals"
      ( "Mehrfaches Vorkommen gleicher Stringliterale (== Inhalt von Strings gleich) nicht erlaubt: '"
          ++ string
          ++ "' Bitte Konstante verwenden."
      )
      sourceSpan
      path
    | string
        `elem` map
          snd
          ( filter
              (\(span, _) -> span /= sourceSpan)
              allPairs
          )
  ]
    ++ checkForDuplicates xs allPairs path
checkForDuplicates [] _ _ = []
