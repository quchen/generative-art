{-# LANGUAGE DeriveFunctor #-}
module Data.Grid.Hexagonal (
      Grid(..)
    , left
    , right
    , upLeft
    , upRight
    , downLeft
    , downRight
    , fromList
    , toList
    , mapAt
    , setAt
    , mapCurrent

    , Comonad(..)
) where



import Control.Comonad
import qualified Data.Map.Strict as M

import Geometry.Coordinates.Hexagonal



data Grid a = Grid Hex (M.Map Hex a) deriving (Eq, Ord, Show, Functor)

fromList :: [(Hex, a)] -> Grid a
fromList = Grid hexZero . M.fromList

toList :: Grid a -> [(Hex, a)]
toList (Grid _ xs) = M.toList xs

left, right, upLeft, upRight, downLeft, downRight :: Grid a -> Maybe (Grid a)
left (Grid h xs)
    | h `M.member` xs = Just (Grid (move L 1 h) xs)
    | otherwise       = Nothing
right (Grid h xs)
    | h `M.member` xs = Just (Grid (move R 1 h) xs)
    | otherwise       = Nothing
upLeft (Grid h xs)
    | h `M.member` xs = Just (Grid (move UL 1 h) xs)
    | otherwise       = Nothing
upRight (Grid h xs)
    | h `M.member` xs = Just (Grid (move UR 1 h) xs)
    | otherwise       = Nothing
downLeft (Grid h xs)
    | h `M.member` xs = Just (Grid (move DL 1 h) xs)
    | otherwise       = Nothing
downRight (Grid h xs)
    | h `M.member` xs = Just (Grid (move DR 1 h) xs)
    | otherwise       = Nothing

instance Foldable Grid where
    foldr plus zero (Grid _ zz) = foldr plus zero zz

instance Comonad Grid where
    extract (Grid h xs) = xs M.! h

    duplicate (Grid h xs) = Grid h xss
      where
        xss = M.fromList [ (h', Grid h' xs) | h' <- M.keys xs ]

mapAt :: Hex -> (a -> a) -> Grid a -> Grid a
mapAt h' f (Grid h xs) = Grid h (M.adjust f h' xs)

setAt :: Hex -> a -> Grid a -> Grid a
setAt h a = mapAt h (const a)

mapCurrent :: (a -> a) -> Grid a -> Grid a
mapCurrent f (Grid h xs) = Grid h (M.adjust f h xs)
