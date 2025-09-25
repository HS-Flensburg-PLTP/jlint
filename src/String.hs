module String (plural, enumerate) where

import qualified Data.List as List

plural :: Int -> String -> String -> String
plural n single plural = if n == 1 then single else plural

enumerate :: [String] -> String
enumerate [] = ""
enumerate [word] = word
enumerate words = List.intercalate ", " (List.init words) ++ " und " ++ List.last words
