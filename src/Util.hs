module Util where



-- | Rotate a list n times.
rotate :: Int -> [a] -> [a]
rotate n xs = let (a,b) = splitAt n xs in b ++ a

-- | Rotate a list until the predicate holds. If it never holds, return the
-- input list.
rotateUntil :: (a -> Bool) -> [a] -> [a]
rotateUntil p xs = zipWith
    (flip const)
    xs
    (dropWhile (not . p) (cycle xs))
