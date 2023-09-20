module Language.Java.Rules.MultipleStringLiterals (check) where

import Data.Generics.Uniplate.Data (universeBi)
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import Language.Java.SourceSpan (Location (..), SourceSpan)
import Language.Java.Syntax
import qualified Markdown
import qualified RDF
import qualified String

check :: CompilationUnit Parsed -> FilePath -> [RDF.Diagnostic]
check cUnit path =
  let stringLiterals = mapMaybe filterStringLiteral (universeBi cUnit)
      validLiterals = filter isValidStringLiteral stringLiterals
   in map (message path) (duplicates validLiterals)
  where
    annotations = universeBi cUnit :: [Annotation Parsed]
    annotationStringLiterals = mapMaybe filterStringLiteral (universeBi annotations)
    isValidStringLiteral literal =
      not (isEmptyString literal) && literal `notElem` annotationStringLiterals

filterStringLiteral :: Literal -> Maybe StringLiteral
filterStringLiteral (String span str) = Just (StringLiteral span str)
filterStringLiteral _ = Nothing

message :: FilePath -> NonEmpty StringLiteral -> RDF.Diagnostic
message path (literal :| additionalOccurrences) =
  RDF.rangeDiagnostic
    "Language.Java.Rules.MultipleStringLiterals"
    [ "Das String-Literal",
      Markdown.code (show (show literal)),
      "wird an dieser und an",
      show (length additionalOccurrences),
      String.plural (length additionalOccurrences) "weiteren Stelle" "weiteren Stellen",
      "verwendet, nämlich in",
      String.plural (length additionalOccurrences) "der Zeile" "den Zeilen",
      intercalate ", " (map (show . startLine . sourceSpan) additionalOccurrences) ++ ".",
      "Bitte eine Konstante dafür einführen."
    ]
    (sourceSpan literal)
    path

-- List helpers

duplicates :: Ord a => [a] -> [NonEmpty a]
duplicates list =
  filter (not . isSingleton) (NonEmpty.group (sort list))

isSingleton :: NonEmpty a -> Bool
isSingleton (_ :| []) = True
isSingleton _ = False

-- Helper -> Move to language-java

startLine :: SourceSpan -> Int
startLine (Location _ line _, _) = line

-- Helping data structure

data StringLiteral = StringLiteral SourceSpan String

instance Show StringLiteral where
  show (StringLiteral _ string) = string

instance Eq StringLiteral where
  StringLiteral _ str1 == StringLiteral _ str2 = str1 == str2

instance Ord StringLiteral where
  compare (StringLiteral _ str1) (StringLiteral _ str2) = compare str1 str2

sourceSpan :: StringLiteral -> SourceSpan
sourceSpan (StringLiteral span _) = span

isEmptyString :: StringLiteral -> Bool
isEmptyString (StringLiteral _ str) = str == ""
