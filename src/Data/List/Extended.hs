module Data.List.Extended (
    module Data.List
    , minimumOn
) where

import Data.List

minimumOn :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe a2
minimumOn _ [] = Nothing
minimumOn f xs = Just (minimumBy (\a b -> compare (f a) (f b)) xs)
