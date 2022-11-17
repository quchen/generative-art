{-# LANGUAGE DeriveFunctor #-}
module Geometry.Algorithms.WaveFunctionCollapse where

import System.Random.MWC as MWC
import Control.Monad.ST
import Control.Applicative
import Data.List (sortOn, find, nub)
import Data.Maybe (catMaybes)

data Zipper a = Zipper [a] a [a] deriving (Eq, Ord, Functor)

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

extractZ :: Zipper a -> a
extractZ (Zipper _ y _) = y

duplicateZ :: Zipper a -> Zipper (Zipper a)
duplicateZ z = Zipper (iterate1 prev z) z (iterate1 next z)

iterate1 :: (a -> Maybe a) -> a -> [a]
iterate1 f = go
  where
    go x = case f x of
        Nothing -> []
        Just fx -> fx : go fx

instance Applicative Zipper where
    pure a = Zipper [] a []
    liftA2 f (Zipper as b cs) (Zipper xs y zs) = Zipper (zipWith f as xs) (f b y) (zipWith f cs zs)

instance Foldable Zipper where
    foldr plus zero = foldr plus zero . toList

instance Traversable Zipper where
    sequenceA (Zipper xs y zs) = liftA3 Zipper (sequenceA xs) y (sequenceA zs)

newtype Grid a = Grid (Zipper (Zipper a)) deriving (Eq, Ord, Functor)

left, right, up, down :: Grid a -> Maybe (Grid a)
left  (Grid zipper) = Grid <$> traverse prev zipper
right (Grid zipper) = Grid <$> traverse next zipper
up    (Grid zipper) = Grid <$> prev zipper
down  (Grid zipper) = Grid <$> next zipper

instance Foldable Grid where
    foldr plus zero (Grid zz) = foldr plus zero (toList zz >>= toList)

extract :: Grid a -> a
extract (Grid zipper) = extractZ (extractZ zipper)

extend :: (Grid a -> b) -> Grid a -> Grid b
extend f = fmap f . duplicate

duplicate :: Grid a -> Grid (Grid a)
duplicate = Grid . fmap duplicateV . duplicateH
  where
    duplicateH :: Grid a -> Zipper (Grid a)
    duplicateH (Grid z) = Grid <$> fmap duplicateZ z
    duplicateV :: Grid a -> Zipper (Grid a)
    duplicateV (Grid z) = Grid <$> duplicateZ z

mapCurrent :: (a -> a) -> Grid a -> Grid a
mapCurrent f (Grid (Zipper xs (Zipper as b cs) zs)) = Grid (Zipper xs (Zipper as (f b) cs) zs)

setCurrent :: a -> Grid a -> Grid a
setCurrent x = mapCurrent (const x)

data WfcSettings a = WfcSettings
    { wfcTiles :: [a]
    , wfcLocalProjection :: Grid (WfState a) -> WfState a
    }

type Coordinate = (Int, Int)

data WfState a = Unobserved [a] | Observed a | Contradiction deriving Eq

instance Foldable WfState where
    foldr plus zero = \case
        Contradiction -> zero
        Observed   x  -> x `plus` zero
        Unobserved xs -> foldr plus zero xs


wfc :: Eq a => WfcSettings a -> Int -> Int -> MWC.GenST x -> ST x (Maybe (Grid a))
wfc settings@WfcSettings{..} width height gen = go initialGrid
  where
    initialGrid = Grid $ fromList $ replicate height (fromList $ replicate width (Unobserved wfcTiles))
    go grid = case findMin grid of
        Just grid' -> collapse settings grid' gen >>= go
        Nothing -> pure (Just (fmap (\(Observed a) -> a) grid))

findMin :: Grid (WfState a) -> Maybe (Grid (WfState a))
findMin = find isUnobserved . sortOn (length . extract) . foldr (:) [] . duplicate
  where
    isUnobserved grid = case extract grid of
        Unobserved _ -> True
        _ -> False

collapse :: Eq a => WfcSettings a -> Grid (WfState a) -> MWC.GenST x -> ST x (Grid (WfState a))
collapse settings grid = pick (eigenstates settings grid)

pick :: [a] -> GenST x -> ST x a
pick xs gen = do
    let len = length xs
    i <- uniformRM (0, len-1) gen
    pure (xs !! i)

eigenstates :: Eq a => WfcSettings a -> Grid (WfState a) -> [Grid (WfState a)]
eigenstates WfcSettings{..} grid = case extract grid of
    Contradiction -> []
    Observed _ -> [grid]
    Unobserved xs -> fmap (\x -> propagate wfcLocalProjection (setCurrent (Observed x) grid)) xs

propagate :: Eq a => (Grid (WfState a) -> WfState a) -> Grid (WfState a) -> Grid (WfState a)
propagate projection = go
  where
    go grid =
        let grid' = extend projection grid
        in if grid' == grid then grid else go grid'

type Stencil3x3 a = (Maybe a, Maybe a, Maybe a, Maybe a, Maybe a, Maybe a, Maybe a, Maybe a, Maybe a)

stencil3x3 :: Grid a -> Stencil3x3 a
stencil3x3 grid = toTuple $ fmap (fmap extract)
    [ left =<< upGrid, upGrid, right =<< upGrid
    , left grid, Just grid, right grid
    , left =<< downGrid, downGrid, right =<< downGrid
    ]
  where
    upGrid = up grid
    downGrid = down grid
    toTuple [a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)
    toTuple _ = undefined

stencils3x3 :: Grid a -> [Stencil3x3 a]
stencils3x3 = foldr (:) [] . extend stencil3x3

settingsFromGrid :: Eq a => Grid a -> WfcSettings (Stencil3x3 a)
settingsFromGrid grid = WfcSettings{..}
  where
    wfcTiles = nub (stencils3x3 grid)
    wfcLocalProjection grid' = case stencil3x3 grid' of _
