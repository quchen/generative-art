{-# LANGUAGE DeriveFunctor #-}
module Data.Zipper (
      Zipper(..)
    , prev
    , next
    , fromList
    , toList
    , Data.Zipper.length

    , iterate1
    , Comonad(..)
) where

import Control.Applicative
import Control.Comonad

data Zipper a = Zipper ![a] a ![a] deriving (Eq, Ord, Show, Functor)

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs
fromList [] = error "Cannot construct Zipper from empty list"

toList :: Zipper a -> [a]
toList (Zipper xs y zs) = reverse xs ++ [y] ++ zs

prev, next :: Zipper a -> Maybe (Zipper a)
prev (Zipper xs y zs) = case xs of
    [] -> Nothing
    x:xs' -> Just (Zipper xs' x (y:zs))
next (Zipper xs y zs) = case zs of
    [] -> Nothing
    z:zs' -> Just (Zipper (y:xs) z zs')

length :: Zipper a -> Int
length = Prelude.length . toList

instance Comonad Zipper where
    extract (Zipper _ y _) = y
    duplicate z = Zipper (iterate1 prev z) z (iterate1 next z)

instance Applicative Zipper where
    pure a = Zipper [] a []
    liftA2 f (Zipper as b cs) (Zipper xs y zs) = Zipper (zipWith f as xs) (f b y) (zipWith f cs zs)

instance Foldable Zipper where
    foldr plus zero = foldr plus zero . toList

instance Traversable Zipper where
    sequenceA (Zipper xs y zs) = liftA3 Zipper (sequenceA xs) y (sequenceA zs)

iterate1 :: (a -> Maybe a) -> a -> [a]
iterate1 f = go
  where
    go x = case f x of
        Nothing -> []
        Just fx -> fx : go fx
