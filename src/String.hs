module String (plural, enumerate) where

import Data.List (intercalate)

plural :: Int -> String -> String -> String
plural n single plural = if n == 1 then single else plural

enumerate :: [String] -> String
enumerate [word] = word
enumerate words = intercalate ", " (init words) ++ " und " ++ last words
