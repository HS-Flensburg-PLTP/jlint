module Data.List.Extra where

import Language.Java.Syntax (EqOptions (IncludeSourceSpan), Equality (eq))

none :: Foldable t => (a -> Bool) -> t a -> Bool
none x = not . any x

(//) :: Equality a => [a] -> [a] -> [a]
xs // ys = filter (\x -> none (eq IncludeSourceSpan x) ys) xs
