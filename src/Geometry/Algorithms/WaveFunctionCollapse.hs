{-# LANGUAGE DeriveFunctor #-}
module WaveFunctionCollapse where

import System.Random.MWC as MWC
import Control.Monad.ST
import Control.Applicative

data Zipper a = Zipper [a] a [a] deriving (Functor)

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs

toList :: Zipper a -> [a]
toList (Zipper xs y zs) = reverse xs ++ [y] ++ zs

moveTo :: Int -> Zipper a -> Maybe (Zipper a)
moveTo i zipper@(Zipper xs y _) = case length xs of
    j | i == j    -> Just zipper
      | i < j     -> prev zipper >>= moveTo i
      | otherwise -> next zipper >>= moveTo i

extract :: Zipper a -> a
extract (Zipper _ y _) = y

(!) :: Zipper a -> Int -> Maybe a
(!) zipper i = extract <$> moveTo i zipper

instance Applicative Zipper where
    pure a = Zipper [] a []
    liftA2 f (Zipper as b cs) (Zipper xs y zs) = Zipper (zipWith f as xs) (f b y) (zipWith f cs zs)

instance Foldable Zipper where
    foldr plus zero = foldr plus zero . toList

instance Traversable Zipper where
    sequenceA (Zipper xs y zs) = liftA3 Zipper (sequenceA xs) y (sequenceA zs)

newtype Grid a = Grid (Zipper (Zipper a)) deriving (Functor)

prev, next :: Zipper a -> Maybe (Zipper a)
prev (Zipper xs y zs) = case xs of
    [] -> Nothing
    x:xs' -> Just (Zipper xs' x (y:zs))
next (Zipper xs y zs) = case zs of
    [] -> Nothing
    z:zs' -> Just (Zipper (y:xs) z zs')

left, right, up, down :: Grid a -> Maybe (Grid a)
left  (Grid zipper) = Grid <$> traverse prev zipper
right (Grid zipper) = Grid <$> traverse next zipper
up    (Grid zipper) = Grid <$> prev zipper
down  (Grid zipper) = Grid <$> next zipper

data WfcSettings a = WfcSettings
    { wfcWidth :: Int
    , wfcHeight :: Int
    , wfcTiles :: [a]
    , wfcLegalAdjacency :: Adjacency a -> Bool
    }

data Adjacency a
    = a `Above` a
    | a `LeftOf` a
    | a `Below` a
    | a `RightOf` a

type Coordinate = (Int, Int)

data WfState a = Unobserved [a] | Observed a | Contradiction

wfc :: Eq a => WfcSettings a -> MWC.GenST x -> ST x (Maybe (Grid a))
wfc WfcSettings{..} gen = go initialGrid
  where
    initialGrid = Grid $ fromList $ replicate wfcHeight (fromList $ replicate wfcWidth (Unobserved wfcTiles))
    go grid = case findMin grid of
        Just minimumElement -> collapse minimumElement grid gen >>= go . propagate minimumElement
        Nothing -> pure (Just (fmap (\(Observed a) -> a) grid))

findMin :: Grid (WfState a) -> Maybe Coordinate
findMin = undefined

collapse :: Coordinate -> Grid (WfState a) -> MWC.GenST x -> ST x (Grid (WfState a))
collapse (x, y) grid@(Grid zipper) gen = do
    let Just (Unobserved possibilities) = zipper ! y >>= (! x)
    observed <- pick possibilities gen
    pure (change (x, y) (const (Observed observed)) grid)

pick :: [a] -> GenST x -> ST x a
pick xs gen = do
    let len = length xs
    i <- uniformRM (0, len-1) gen
    pure (xs !! i)

change :: Coordinate -> (a -> a) -> Grid a -> Grid a
change (x, y) f (Grid zipper) = case traverse (moveTo x) zipper >>= moveTo y of
    Nothing -> Grid zipper
    Just (Zipper xs (Zipper as b cs) zs) -> Grid (Zipper xs (Zipper as (f b) cs) zs)

propagate :: Coordinate -> Grid (WfState a) -> Grid (WfState a)
propagate = undefined
