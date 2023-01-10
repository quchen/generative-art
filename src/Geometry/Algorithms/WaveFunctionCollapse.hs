{-# LANGUAGE DeriveFunctor #-}
module Geometry.Algorithms.WaveFunctionCollapse (
      WfcSettings(..)
    , settingsFromGrid

    , wfc
    , wfcStep
    , initialGrid
    , collapse
    , propagate
    , eigenstates

    , Stencil3x3(..)
    , stencilToGrid
    , stencil3x3

    , module Data.Grid
) where



import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.ST
import Data.Function (on)
import Data.List (sortOn, find, groupBy)
import Data.Maybe (maybeToList, catMaybes, listToMaybe)
import System.Random.MWC as MWC

import Data.Grid
import Data.Zipper (Zipper(..))
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as M



data WfcSettings a = WfcSettings
    { wfcTiles :: MultiSet a
    , wfcLocalProjection :: Grid (MultiSet a) -> MultiSet a
    }

wfc :: (Eq a, Ord a) => WfcSettings a -> Int -> Int -> MWC.GenST x -> ST x [Grid (MultiSet a)]
wfc settings width height gen = go (initialGrid settings width height)
  where
    go grid = fmap (grid :) $ wfcStep settings gen grid >>= \case
        Just grid' -> go grid'
        Nothing -> pure []

wfcStep :: (Eq a, Ord a) => WfcSettings a -> MWC.GenST x -> Grid (MultiSet a) -> ST x (Maybe (Grid (MultiSet a)))
wfcStep WfcSettings{..} gen grid = pickMin gen grid >>= \case
    Just grid' -> Just . propagate wfcLocalProjection <$> collapse gen grid'
    Nothing -> pure Nothing

initialGrid :: WfcSettings a -> Int -> Int -> Grid (MultiSet a)
initialGrid WfcSettings{..} width height = fromList $ replicate height (replicate width wfcTiles)

findMin :: Grid (MultiSet a) -> Maybe (Grid (MultiSet a))
findMin = find isUnobserved . sortOn (length . extract) . foldr (:) [] . duplicate
  where
    isUnobserved grid = M.distinctSize (extract grid) > 1

pickMin :: MWC.GenST x -> Grid (MultiSet a) -> ST x (Maybe (Grid (MultiSet a)))
pickMin gen grid = case shortestGrids of
    Nothing -> pure Nothing
    Just xs -> Just <$> pick gen xs
  where
    lengthIndexed = fmap (\g -> (M.distinctSize (extract g), g)) (foldr (:) [] (duplicate grid))
    shortestGrids = case groupBy ((==) `on` fst) $ sortOn fst $ filter ((> 1) . fst) $ lengthIndexed of
        [] -> Nothing
        (lengthIndexedShortestGrids : _) -> Just (snd <$> lengthIndexedShortestGrids)

collapse :: (Eq a, Ord a) => MWC.GenST x -> Grid (MultiSet a) -> ST x (Grid (MultiSet a))
collapse gen = pick gen . eigenstates

pick :: GenST x -> [a] -> ST x a
pick gen xs = do
    let len = length xs
    i <- uniformRM (0, len-1) gen
    pure (xs !! i)

eigenstates :: (Eq a, Ord a) => Grid (MultiSet a) -> [Grid (MultiSet a)]
eigenstates grid = case M.toList (extract grid) of
    [] -> []
    [_] -> [grid]
    xs -> fmap (\x -> setCurrent (M.singleton x) grid) xs

propagate :: Eq a => (Grid (MultiSet a) -> MultiSet a) -> Grid (MultiSet a) -> Grid (MultiSet a)
propagate projection = go
  where
    go grid =
        let grid' = extend projection grid
        in if grid' == grid then grid else go grid'

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

instance Applicative Stencil3x3 where
    pure a = Stencil3x3 (Just a) (Just a) (Just a) (Just a) a (Just a) (Just a) (Just a) (Just a)
    liftA2 f (Stencil3x3 a1 b1 c1 d1 e1 f1 g1 h1 i1) (Stencil3x3 a2 b2 c2 d2 e2 f2 g2 h2 i2) =
        Stencil3x3 (liftA2 f a1 a2) (liftA2 f b1 b2) (liftA2 f c1 c2)
                   (liftA2 f d1 d2) (       f e1 e2) (liftA2 f f1 f2)
                   (liftA2 f g1 g2) (liftA2 f h1 h2) (liftA2 f i1 i2)

instance Foldable Stencil3x3 where
    foldr plus zero Stencil3x3{..} = foldr plus zero (catMaybes [s11, s12, s13, s21, Just s22, s23, s31, s32, s33])

stencil3x3 :: Grid a -> Stencil3x3 a
stencil3x3 (Grid (Zipper upper middle lower)) = Stencil3x3 a b c d e f g h i
  where
    (a, b, c) = case upper of
        [] -> (Nothing, Nothing, Nothing)
        (Zipper as b cs : _) -> (listToMaybe as, Just b, listToMaybe cs)
    (d, e, f) = case middle of
        Zipper ds e fs -> (listToMaybe ds, e, listToMaybe fs)
    (g, h, i) = case lower of
        [] -> (Nothing, Nothing, Nothing)
        (Zipper gs h is : _) -> (listToMaybe gs, Just h, listToMaybe is)

stencils3x3 :: Grid a -> [Stencil3x3 a]
stencils3x3 = foldr (:) [] . extend stencil3x3

stencilToGrid :: Stencil3x3 a -> Grid a
stencilToGrid (Stencil3x3 a b c d e f g h i) = Grid (Zipper upper middle lower)
  where
    upper = case b of
        Just b' -> [Zipper (maybeToList a) b' (maybeToList c)]
        Nothing -> []
    middle = Zipper (maybeToList d) e (maybeToList f)
    lower = case h of
        Just h' -> [Zipper (maybeToList g) h' (maybeToList i)]
        Nothing -> []

settingsFromGrid :: (Eq a, Ord a, Show a) => Grid a -> WfcSettings (Stencil3x3 a)
settingsFromGrid grid = WfcSettings{..}
  where
    wfcTiles = M.fromList (filter onlyFullStencils (stencils3x3 grid))
    onlyFullStencils = \case
        Stencil3x3 (Just _) (Just _) (Just _) (Just _) _ (Just _) (Just _) (Just _) (Just _) -> True
        _otherwise -> False
    wfcLocalProjection :: (Eq a, Ord a, Show a) => Grid (MultiSet (Stencil3x3 a)) -> MultiSet (Stencil3x3 a)
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
remainingEigenvalues :: (Eq a, Ord a, Show a) => Grid (MultiSet (Stencil3x3 a)) -> MultiSet (Stencil3x3 a)
remainingEigenvalues grid = M.fromAscOccurList
    [ (this, n)
    | (this, n) <- M.toAscOccurList (extract grid)
    , let isCompatible = and
            [ not (null compatibleOthers)
            | (there, back) <- zip neighbouringDirections neighbouringDirections
            , let others = maybeToList (there grid) >>= M.distinctElems . extract
            , not (null others)
            , thisShifted <- maybeToList (back (stencilToGrid this))
            , let compatibleOthers = filter (weakEq (stencil3x3 thisShifted)) others
            ]
    , isCompatible
    ]
  where
    neighbouringDirections :: [Grid b -> Maybe (Grid b)]
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
