module Util where



import qualified Data.Set as S



-- | Rotate a list n times.
rotateList :: Int -> [a] -> [a]
rotateList n xs = let (a,b) = splitAt n xs in b ++ a

-- | All rotations of a list.
--
-- prop> \n xs -> rotations xs !! n == rotate n xs
rotations :: [a] -> [[a]]
rotations = go []
  where
    go _ [] = []
    go xs (y:ys) = let xs' = xs ++ [y]
                       rotation = y:ys ++ xs
                   in rotation : go xs' ys

-- | Rotate a list until the predicate holds. If it never holds, return the
-- input list.
rotateUntil :: (a -> Bool) -> [a] -> [a]
rotateUntil p xs = zipWith
    (flip const)
    xs
    (dropWhile (not . p) (cycle xs))

-- | 'nub', but based on 'S.Set' for performance reasons.
nub' :: Ord a => [a] -> [a]
nub' = go S.empty
  where
    go _ [] = []
    go seen (x:xs)
        | S.member x seen = go seen xs
        | otherwise       = x : go (S.insert x seen) xs
