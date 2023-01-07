{-# LANGUAGE DeriveFunctor #-}
module Geometry.Algorithms.WaveFunctionCollapse where

import System.Random.MWC as MWC
import Control.Monad ((>=>))
import Control.Monad.ST
import Control.Applicative
import Data.List.Extended (sortOn, find, nubOrd, groupBy)
import Data.Maybe (maybeToList, catMaybes)
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import Data.Function (on)

data Zipper a = Zipper ![a] !a ![a] deriving (Eq, Ord, Show, Functor)

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

newtype Grid a = Grid (Zipper (Zipper a)) deriving (Eq, Ord, Show, Functor)

fromListG :: [[a]] -> Grid a
fromListG xss = Grid (fromList (fmap fromList xss))

left, right, up, down :: Grid a -> Maybe (Grid a)
left  (Grid zipper) = Grid <$> traverse prev zipper
right (Grid zipper) = Grid <$> traverse next zipper
up    (Grid zipper) = Grid <$> prev zipper
down  (Grid zipper) = Grid <$> next zipper

instance Foldable Grid where
    foldr plus zero (Grid zz) = foldr plus zero (toList zz >>= toList)

instance Applicative Grid where
    pure a = Grid (pure (pure a))
    liftA2 f (Grid xs) (Grid ys) = Grid (liftA2 (liftA2 f) xs ys)

extract :: Grid a -> a
extract (Grid zipper) = extractZ (extractZ zipper)

extend :: (Grid a -> b) -> Grid a -> Grid b
extend f = fmap f . duplicate

duplicate :: Grid a -> Grid (Grid a)
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
initialGrid WfcSettings{..} width height = Grid $ fromList $ replicate height (fromList $ replicate width wfcTiles)

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




data XO = X | O deriving (Eq, Ord, Show)

example :: Grid XO
example = fromListG
    [ [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ O, O, O, O, O, O, O, O, O, O, O, O, O ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    ]

printGrid :: Show a => Grid a -> String
printGrid (Grid xs) = unlines (toList (fmap (concatMap show . toList) xs))

debugGrid :: Show a => Grid [Stencil3x3 a] -> IO ()
debugGrid (Grid xs) = putStrLn $ unlines $ toList $ fmap (concat . toList . fmap pp) xs
  where
    pp [] = "0"
    pp [a] = show (extractStencil a)
    pp as = show (length as)

debugStencil :: Show a => Stencil3x3 a -> String
debugStencil (Stencil3x3 a b c d e f g h i) = unlines
    [ concatMap (maybe "_" show) [a, b, c]
    , concatMap (maybe "_" show) [d, Just e, f]
    , concatMap (maybe "_" show) [g, h, i]
    ]

test :: IO ()
test = do
    let height = 2
        width = 4
        settings = settingsFromGrid example
        initial = initialGrid settings width height
    debugGrid initial
    traverse_ debugGrid $ runST $ do
        gen <- initialize (V.fromList [1])
        go' 1 settings gen initial
  where
    go :: (Show a, Eq a) => WfcSettings a -> GenST x -> Grid [a] -> ST x [Grid [a]]
    go settings gen grid = pickMin gen grid >>= \case 
        Just grid' -> do
            grid'' <- collapse gen grid'
            result' <- go' 1 settings gen grid''
            pure (grid' : grid'' : result')
        Nothing -> pure []
    go' :: (Show a, Eq a) => Int -> WfcSettings a -> GenST x -> Grid [a] -> ST x [Grid [a]]
    go' n settings gen grid =
        let grid' = extend (wfcLocalProjection settings) grid
        in if grid == grid'
            then go settings gen grid
            else do
                result <- go' (n-1) settings gen grid'
                pure (grid' : result)

extractStencil :: Stencil3x3 a -> a 
extractStencil (Stencil3x3 _ _ _ _ e _ _ _ _) = e
