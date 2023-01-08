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
)where



import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.ST
import Data.Function (on)
import Data.List.Extended (sortOn, find, nubOrd, groupBy)
import Data.Maybe (maybeToList, catMaybes)
import System.Random.MWC as MWC

import Data.Grid
import Data.Zipper (Zipper(..))



data WfcSettings a = WfcSettings
    { wfcTiles :: [a]
    , wfcLocalProjection :: Grid [a] -> [a]
    }

wfc :: Eq a => WfcSettings a -> Int -> Int -> MWC.GenST x -> ST x (Grid [a])
wfc settings width height gen = go (initialGrid settings width height)
  where
    go grid = wfcStep settings gen grid >>= \case
        Just grid' -> go grid'
        Nothing -> pure grid

wfcStep :: Eq a => WfcSettings a -> MWC.GenST x -> Grid [a] -> ST x (Maybe (Grid [a]))
wfcStep WfcSettings{..} gen grid = pickMin gen grid >>= \case
    Just grid' -> Just . propagate wfcLocalProjection <$> collapse gen grid'
    Nothing -> pure Nothing

initialGrid :: WfcSettings a -> Int -> Int -> Grid [a]
initialGrid WfcSettings{..} width height = fromList $ replicate height (replicate width wfcTiles)

findMin :: Grid [a] -> Maybe (Grid [a])
findMin = find isUnobserved . sortOn (length . extract) . foldr (:) [] . duplicate
  where
    isUnobserved grid = length (extract grid) > 1

pickMin :: MWC.GenST x -> Grid [a] -> ST x (Maybe (Grid [a]))
pickMin gen grid = case shortestGrids of
    Nothing -> pure Nothing
    Just xs -> Just <$> pick gen xs
  where
    lengthIndexed = fmap (\g -> (length (extract g), g)) (foldr (:) [] (duplicate grid))
    shortestGrids = case groupBy ((==) `on` fst) $ sortOn fst $ filter ((> 1) . fst) $ lengthIndexed of
        [] -> Nothing
        (lengthIndexedShortestGrids : _) -> Just (snd <$> lengthIndexedShortestGrids)

collapse :: Eq a => MWC.GenST x -> Grid [a] -> ST x (Grid [a])
collapse gen = pick gen . eigenstates

pick :: GenST x -> [a] -> ST x a
pick gen xs = do
    let len = length xs
    i <- uniformRM (0, len-1) gen
    pure (xs !! i)

eigenstates :: Eq a => Grid [a] -> [Grid [a]]
eigenstates grid = case extract grid of
    [] -> []
    [_] -> [grid]
    xs -> fmap (\x -> setCurrent [x] grid) xs

propagate :: Eq a => (Grid [a] -> [a]) -> Grid [a] -> Grid [a]
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
stencil3x3 grid = toStencil $ fmap (fmap extract)
    [ left =<< upGrid, upGrid, right =<< upGrid
    , left grid, Just grid, right grid
    , left =<< downGrid, downGrid, right =<< downGrid
    ]
  where
    upGrid = up grid
    downGrid = down grid
    toStencil [a, b, c, d, Just e, f, g, h, i] = Stencil3x3 a b c d e f g h i
    toStencil _ = undefined

stencils3x3 :: Grid a -> [Stencil3x3 a]
stencils3x3 = foldr (:) [] . extend stencil3x3

stencilToGrid :: Stencil3x3 a -> Grid a
stencilToGrid (Stencil3x3 a b c d e f g h i) = Grid (Zipper upper middle lower)
  where
    upper = case liftA3 (,,) a b c of
        Just (a', b', c') -> [Zipper [a'] b' [c']]
        Nothing           -> []
    middle = Zipper (maybeToList d) e (maybeToList f)
    lower = case liftA3 (,,) g h i of
        Just (g', h', i') -> [Zipper [g'] h' [i']]
        Nothing           -> []

settingsFromGrid :: (Eq a, Ord a, Show a) => Grid a -> WfcSettings (Stencil3x3 a)
settingsFromGrid grid = WfcSettings{..}
  where
    wfcTiles = filter onlyFullStencils (stencils3x3 grid)
    onlyFullStencils = \case
        Stencil3x3 (Just _) (Just _) (Just _) (Just _) _ (Just _) (Just _) (Just _) (Just _) -> True
        _otherwise -> False
    wfcLocalProjection :: (Eq a, Ord a, Show a) => Grid [Stencil3x3 a] -> [Stencil3x3 a]
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
remainingEigenvalues :: (Eq a, Ord a, Show a) => Grid [Stencil3x3 a] -> [Stencil3x3 a]
remainingEigenvalues grid = nubOrd
    [ this
    | this <- extract grid
    , let isCompatible = and
            [ not (null compatibleOthers)
            | (there, back) <- zip neighbouringDirections neighbouringDirections
            , let others = maybeToList (there grid) >>= extract
            , not (null others)
            , thisShifted <- maybeToList (back (stencilToGrid this))
            , let compatibleOthers = [other | other <- others, thisShifted `weakEq` stencilToGrid other]
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
weakEq :: Eq a => Grid a -> Grid a -> Bool
weakEq as bs = and (liftA2 (==) as bs)
