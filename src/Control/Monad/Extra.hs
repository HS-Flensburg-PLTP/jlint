module Control.Monad.Extra (concatMapM) where

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = return []
concatMapM func (x:xs) = do
    m <- func x
    cm <- concatMapM func xs
    return (m ++ cm)