{-# LANGUAGE DeriveFunctor #-}
module Data.Grid.Generic (
      Grid(..)
    , fromList
    , toList
    , mapAt
    , setAt
    , mapCurrent

    , Comonad(..)
) where



import Control.Comonad
import qualified Data.Map.Strict as M



data Grid k a = Grid k (M.Map k a) deriving (Eq, Ord, Show, Functor)

fromList :: Ord k => k -> [(k, a)] -> Grid k a
fromList k = Grid k . M.fromList

toList :: Grid k a -> [(k, a)]
toList (Grid _ xs) = M.toList xs

instance Foldable (Grid k) where
    foldr plus zero (Grid _ zz) = foldr plus zero zz

instance Ord k => Comonad (Grid k) where
    extract (Grid h xs) = xs M.! h

    duplicate (Grid h xs) = Grid h xss
      where
        xss = M.fromList [ (h', Grid h' xs) | h' <- M.keys xs ]

mapAt :: Ord k => k -> (a -> a) -> Grid k a -> Grid k a
mapAt h' f (Grid h xs) = Grid h (M.adjust f h' xs)

setAt :: Ord k => k -> a -> Grid k a -> Grid k a
setAt h a = mapAt h (const a)

mapCurrent :: Ord k => (a -> a) -> Grid k a -> Grid k a
mapCurrent f (Grid h xs) = Grid h (M.adjust f h xs)

