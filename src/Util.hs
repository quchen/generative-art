module Util where



import qualified Data.Set as S



-- | 'nub', but based on 'S.Set' for performance reasons.
nubOrd :: Ord a => [a] -> [a]
nubOrd = go S.empty
  where
    go _ [] = []
    go seen (x:xs)
        | S.member x seen = go seen xs
        | otherwise       = x : go (S.insert x seen) xs
