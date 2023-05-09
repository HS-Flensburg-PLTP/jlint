module Data.List.Extra where

none :: Foldable t => (a -> Bool) -> t a -> Bool
none x = not . any x
