module Data.List.Extended (
    module Data.List
    , minimumOn
    , nubOrd
) where



import           Data.List
import qualified Data.Set  as S



minimumOn :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe a2
minimumOn _ [] = Nothing
minimumOn f xs = Just (minimumBy (\a b -> compare (f a) (f b)) xs)

-- | 'nub', but based on 'S.Set' for performance reasons.
nubOrd :: Ord a => [a] -> [a]
nubOrd = go S.empty
  where
    go _ [] = []
    go seen (x:xs)
        | S.member x seen = go seen xs
        | otherwise       = x : go (S.insert x seen) xs
