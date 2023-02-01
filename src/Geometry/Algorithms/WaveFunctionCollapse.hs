{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Geometry.Algorithms.WaveFunctionCollapse (
      WfcSettings(..)
    , settingsFromGrid

    , wfc
    , wfcStep
    , initialGrid
    , pickMin
    , collapse
    , propagate

    , Stencil3x3(..)
    , stencilToGrid
    , stencil3x3
    , extractStencil

    , Touched(..)
    , getTouched

    , module Data.Grid
) where



import Control.Monad ((>=>))
import Control.Monad.ST
import Data.Function (on)
import Data.List (sortOn, groupBy)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, catMaybes, mapMaybe)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as M
import System.Random.MWC as MWC

import Data.Grid
import Draw.Grid



data WfcSettings coord a = WfcSettings
    { wfcRange :: [coord]
    , wfcTiles :: MultiSet a
    , wfcLocalProjection :: Grid coord (Touched (MultiSet a)) -> Touched (MultiSet a)
    }

wfc :: (Ord coord, Eq a, Ord a) => WfcSettings coord a -> MWC.GenST x -> ST x [Grid coord (MultiSet a)]
wfc settings gen = go (initialGrid settings)
  where
    go grid = fmap (grid :) $ wfcStep settings gen grid >>= \case
        Just grid' -> go grid'
        Nothing -> pure []

wfcStep :: (Ord coord, Eq a, Ord a) => WfcSettings coord a -> MWC.GenST x -> Grid coord (MultiSet a) -> ST x (Maybe (Grid coord (MultiSet a)))
wfcStep WfcSettings{..} gen grid = pickMin gen grid >>= \case
    Just grid' -> Just . propagate wfcLocalProjection <$> collapse gen grid'
    Nothing -> pure Nothing

initialGrid :: Ord coord => WfcSettings coord a -> Grid coord (MultiSet a)
initialGrid WfcSettings{..} = fromList $ [ (c, wfcTiles) | c <- wfcRange ]

pickMin :: Ord coord => MWC.GenST x -> Grid coord (MultiSet a) -> ST x (Maybe (Grid coord (MultiSet a)))
pickMin gen grid = case gridsWithLowestEntropy of
    Nothing -> pure Nothing
    Just xs -> Just <$> pick gen xs
  where
    lengthIndexed = fmap (\g -> (M.distinctSize (extract g), g)) (foldr (:) [] (duplicate grid))
    entropyIndexed = (\g -> (entropy (extract g), g)) . snd <$> filter ((> 1) . fst) lengthIndexed
    gridsWithLowestEntropy = case groupBy ((==) `on` fst) $ sortOn fst entropyIndexed of
        [] -> Nothing
        (lengthIndexedShortestGrids : _) -> Just (snd <$> lengthIndexedShortestGrids)

entropy :: MultiSet a -> Double
entropy multiset = sum weights - sum ((\w -> w * log w) <$> weights) / sum weights
  where weights = fromIntegral . snd <$> M.toOccurList multiset

collapse :: (Ord coord, Eq a, Ord a) => MWC.GenST x -> Grid coord (MultiSet a) -> ST x (Grid coord (MultiSet a))
collapse gen = pick gen . eigenstates

pick :: GenST x -> [a] -> ST x a
pick gen xs = do
    let len = length xs
    i <- uniformRM (0, len-1) gen
    pure (xs !! i)

eigenstates :: (Ord coord, Eq a, Ord a) => Grid coord (MultiSet a) -> [Grid coord (MultiSet a)]
eigenstates grid@(Grid c _) = case M.toList (extract grid) of
    [] -> []
    [_] -> [grid]
    xs -> fmap (\x -> setAt c (M.singleton x) grid) xs

data Touched a = Touched a | Untouched a deriving Functor

getTouched :: Touched p -> p
getTouched (Touched a) = a
getTouched (Untouched a) = a

propagate :: (Ord coord, Eq a) => (Grid coord (Touched (MultiSet a)) -> Touched (MultiSet a)) -> Grid coord (MultiSet a) -> Grid coord (MultiSet a)
propagate projection = fmap getTouched . go . mapCurrent (Touched . getTouched) . fmap Untouched
  where
    go grid =
        let grid' = extend projection grid
        in if fmap getTouched grid' == fmap getTouched grid then grid else go grid'

data Stencil3x3 a = Stencil3x3
    { s11 :: !(Maybe a)
    , s12 :: !(Maybe a)
    , s13 :: !(Maybe a)
    , s21 :: !(Maybe a)
    , s22 :: !a
    , s23 :: !(Maybe a)
    , s31 :: !(Maybe a)
    , s32 :: !(Maybe a)
    , s33 :: !(Maybe a)
    } deriving (Eq, Ord, Show, Functor)

instance Foldable Stencil3x3 where
    foldr plus zero Stencil3x3{..} = foldr plus zero (catMaybes [s11, s12, s13, s21, Just s22, s23, s31, s32, s33])

instance DrawToSize a => DrawToSize (Stencil3x3 a) where
    drawToSize (w, h) = drawGrid (w, h) . stencilToGrid

stencil3x3 :: RectilinearGrid a -> Stencil3x3 a
stencil3x3 (Grid (x, y) xs) = Stencil3x3 a b c d e f g h i
  where
    a = el (x-1, y-1)
    b = el (x,   y-1)
    c = el (x+1, y-1)
    d = el (x-1, y)
    Just e = el (x, y)
    f = el (x+1, y)
    g = el (x-1, y+1)
    h = el (x,   y+1)
    i = el (x+1, y+1)
    el coord = Map.lookup coord xs


stencils3x3 :: RectilinearGrid a -> [Stencil3x3 a]
stencils3x3 = foldr (:) [] . extend stencil3x3

stencilToGrid :: Stencil3x3 a -> RectilinearGrid a
stencilToGrid (Stencil3x3 a b c d e f g h i) = Grid (1-xmin, 1-ymin) m
  where
    xs = mapMaybe (\(k, v) -> fmap (k,) v)
        [ ((0, 0), a), ((1, 0), b), ((2, 0), c)
        , ((0, 1), d), ((1, 1), Just e), ((2, 1), f)
        , ((0, 2), g), ((1, 2), h), ((2, 2), i)
        ]
    (xmin, ymin) = minimum (fst <$> xs)
    m = Map.fromList (fmap (\((x, y), v) -> ((x-xmin, y-ymin), v)) xs)

extractStencil :: Stencil3x3 a -> a
extractStencil (Stencil3x3 _ _ _ _ e _ _ _ _) = e

settingsFromGrid :: (Eq a, Ord a) => (Int, Int) -> RectilinearGrid a -> WfcSettings (Int, Int) (Stencil3x3 a)
settingsFromGrid (w, h) grid = WfcSettings{..}
  where
    wfcRange = [ (x, y) | x <- [0..w-1], y <- [0..h-1] ]
    wfcTiles = M.fromList (filter onlyFullStencils (stencils3x3 grid))
    onlyFullStencils = \case
        Stencil3x3 (Just _) (Just _) (Just _) (Just _) _ (Just _) (Just _) (Just _) (Just _) -> True
        _otherwise -> False
    wfcLocalProjection :: (Eq a, Ord a) => RectilinearGrid (Touched (MultiSet (Stencil3x3 a))) -> Touched (MultiSet (Stencil3x3 a))
    wfcLocalProjection = remainingEigenvalues

{-
Concept:
* Take the middle stencil
* Move in some direction, and take the stencils there
* Displace the middle stencil in the same direction
* Compare the shifted middle stencil to each of the neighbouring stencils
* If any of them is `weakEq`, then the middle stencil is still allowed.

If in one given direction none of the neighbouring stencils matches, then the middle stencil is discarded.

_ _ _   _ _ _   _ _ _
_ a b   a b c   b c _
_ d e   d e f   e f _

_ a b   a b c   b c _
_ d e   d e f   e f _
_ g h   g h i   h i _

_ d e   d e f   e f _
_ g h   g h i   h i _
_ _ _   _ _ _   _ _ _
-}
remainingEigenvalues :: (Eq a, Ord a) => RectilinearGrid (Touched (MultiSet (Stencil3x3 a))) -> Touched (MultiSet (Stencil3x3 a))
remainingEigenvalues grid
    | all isUntouched (fmap (fmap extract) (neighbouringDirections <*> [grid]))
    = Untouched oldState
    | oldState == newState
    = Untouched oldState
    | otherwise
    = Touched newState
  where
    oldState = getTouched (extract grid)
    newState = M.fromAscOccurList
        [ (this, n)
        | (this, n) <- M.toAscOccurList (getTouched (extract grid))
        , let isCompatible = and
                [ not (null compatibleOthers)
                | (there, back) <- zip neighbouringDirections neighbouringDirections
                , let others = maybeToList (there grid) >>= M.distinctElems . getTouched . extract
                , not (null others)
                , thisShifted <- maybeToList (back (stencilToGrid this))
                , let compatibleOthers = filter (weakEq (stencil3x3 thisShifted)) others
                ]
        , isCompatible
        ]
    isUntouched = \case
        Nothing -> True
        Just (Untouched _) -> True
        _otherwise -> False
    neighbouringDirections :: [RectilinearGrid b -> Maybe (RectilinearGrid b)]
    neighbouringDirections =
        [ up >=> left
        , up
        , up >=> right
        , left
        , right
        , down >=> left
        , down
        , down >=> right
        ]

-- Compares the overlapping parts of the two grids, i.e. comparing the points where both sides are defined.
weakEq :: Eq a => Stencil3x3 a -> Stencil3x3 a -> Bool
weakEq (Stencil3x3 a1 b1 c1 d1 e1 f1 g1 h1 i1) (Stencil3x3 a2 b2 c2 d2 e2 f2 g2 h2 i2)
    =  a1 =~ a2
    && b1 =~ b2
    && c1 =~ c2
    && d1 =~ d2
    && e1 == e2
    && f1 =~ f2
    && g1 =~ g2
    && h1 =~ h2
    && i1 =~ i2
  where
    Just x =~ Just y = x == y
    _      =~ _      = True
