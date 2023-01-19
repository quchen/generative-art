{-# LANGUAGE DeriveFunctor #-}
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
import Data.Maybe (maybeToList, catMaybes)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as M
import qualified Data.Vector as V
import System.Random.MWC as MWC

import Data.Grid
import Draw.Grid



data WfcSettings a = WfcSettings
    { wfcTiles :: MultiSet a
    , wfcLocalProjection :: Grid (Touched (MultiSet a)) -> Touched (MultiSet a)
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
eigenstates grid@(Grid l _ _ u _) = case M.toList (extract grid) of
    [] -> []
    [_] -> [grid]
    xs -> fmap (\x -> setAt (l, u) (M.singleton x) grid) xs

data Touched a = Touched a | Untouched a deriving Functor

getTouched :: Touched p -> p
getTouched (Touched a) = a
getTouched (Untouched a) = a

propagate :: Eq a => (Grid (Touched (MultiSet a)) -> Touched (MultiSet a)) -> Grid (MultiSet a) -> Grid (MultiSet a)
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

stencil3x3 :: Grid a -> Stencil3x3 a
stencil3x3 (Grid l' d' r' u' xs) = Stencil3x3 a b c d e f g h i
  where
    a | l' > 0 && u' > 0 = el (l'-1) (u'-1)
      | otherwise = Nothing
    b | u' > 0 = el l' (u'-1)
      | otherwise = Nothing
    c | r' > 0 && u' > 0 = el (l'+1) (u'-1)
      | otherwise = Nothing
    d | l' > 0 = el (l'-1) u'
      | otherwise = Nothing
    Just e = el l' u'
    f | r' > 0 = el (l'+1) u'
      | otherwise = Nothing
    g | l' > 0 && d' > 0 = el (l'-1) (u'+1)
      | otherwise = Nothing
    h | d' > 0 = el l' (u'+1)
      | otherwise = Nothing
    i | r' > 0 && d' > 0 = el (l'+1) (u'+1)
      | otherwise = Nothing
    el x y = Just (xs V.! y V.! x)
    

stencils3x3 :: Grid a -> [Stencil3x3 a]
stencils3x3 = foldr (:) [] . extend stencil3x3

stencilToGrid :: Stencil3x3 a -> Grid a
stencilToGrid (Stencil3x3 a b c d e f g h i) = Grid l' d' r' u' xs
  where
    l'  | Just _ <- d = 1
        | otherwise = 0
    d'  | Just _ <- h = 1
        | otherwise = 0
    r'  | Just _ <- f = 1
        | otherwise = 0
    u'  | Just _ <- b = 1
        | otherwise = 0
    xs = V.catMaybes $ V.fromList
        [ case b of
              Just _ -> Just $ V.catMaybes $ V.fromList [a, b, c]
              Nothing -> Nothing,
          Just $ V.catMaybes $ V.fromList [d, Just e, f],
          case h of
              Just _ -> Just $ V.catMaybes $ V.fromList [g, h, i]
              Nothing -> Nothing
        ]

extractStencil :: Stencil3x3 a -> a 
extractStencil (Stencil3x3 _ _ _ _ e _ _ _ _) = e

settingsFromGrid :: (Eq a, Ord a) => Grid a -> WfcSettings (Stencil3x3 a)
settingsFromGrid grid = WfcSettings{..}
  where
    wfcTiles = M.fromList (filter onlyFullStencils (stencils3x3 grid))
    onlyFullStencils = \case
        Stencil3x3 (Just _) (Just _) (Just _) (Just _) _ (Just _) (Just _) (Just _) (Just _) -> True
        _otherwise -> False
    wfcLocalProjection :: (Eq a, Ord a) => Grid (Touched (MultiSet (Stencil3x3 a))) -> Touched (MultiSet (Stencil3x3 a))
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
remainingEigenvalues :: (Eq a, Ord a) => Grid (Touched (MultiSet (Stencil3x3 a))) -> Touched (MultiSet (Stencil3x3 a))
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
