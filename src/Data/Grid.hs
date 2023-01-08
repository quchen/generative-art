{-# LANGUAGE DeriveFunctor #-}
module Data.Grid (
      Grid(..)
    , left
    , right
    , up
    , down
    , fromList
    , toList
    , mapCurrent
    , setCurrent
    , size

    , iterate1
    , Comonad(..)
) where

import Control.Applicative
import Control.Comonad

import Data.Zipper hiding (fromList, toList)
import qualified Data.Zipper as Z

newtype Grid a = Grid (Zipper (Zipper a)) deriving (Eq, Ord, Show, Functor)

fromList :: [[a]] -> Grid a
fromList xss = Grid (Z.fromList (fmap Z.fromList xss))

toList :: Grid a -> [[a]]
toList (Grid zs) = Z.toList (Z.toList <$> zs)

left, right, up, down :: Grid a -> Maybe (Grid a)
left  (Grid zipper) = Grid <$> traverse prev zipper
right (Grid zipper) = Grid <$> traverse next zipper
up    (Grid zipper) = Grid <$> prev zipper
down  (Grid zipper) = Grid <$> next zipper

size :: Grid a -> (Int, Int)
size (Grid z) = (Z.length (extract z), Z.length z)

instance Foldable Grid where
    foldr plus zero (Grid zz) = foldr plus zero (Z.toList zz >>= Z.toList)

instance Applicative Grid where
    pure a = Grid (pure (pure a))
    liftA2 f (Grid xs) (Grid ys) = Grid (liftA2 (liftA2 f) xs ys)

instance Comonad Grid where
    extract (Grid zipper) = extract (extract zipper)

    duplicate = Grid . fmap duplicateH . duplicateV
      where
        duplicateH :: Grid a -> Zipper (Grid a)
        duplicateH g = Zipper (iterate1 left g) g (iterate1 right g)
        duplicateV :: Grid a -> Zipper (Grid a)
        duplicateV g = Zipper (iterate1 up g) g (iterate1 down g)

mapCurrent :: (a -> a) -> Grid a -> Grid a
mapCurrent f (Grid (Zipper xs (Zipper as b cs) zs)) = Grid (Zipper xs (Zipper as (f b) cs) zs)

setCurrent :: a -> Grid a -> Grid a
setCurrent x = mapCurrent (const x)
