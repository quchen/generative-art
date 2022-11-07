{-# LANGUAGE DeriveFunctor #-}
module WaveFunctionCollapse where

import System.Random.MWC as MWC
import Control.Monad
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

extractZ :: Zipper a -> a
extractZ (Zipper _ y _) = y

(!) :: Zipper a -> Int -> Maybe a
(!) zipper i = extractZ <$> moveTo i zipper

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

extract :: Grid a -> a
extract (Grid zipper) = extractZ (extractZ zipper)

data WfcSettings a = WfcSettings
    { wfcWidth :: Int
    , wfcHeight :: Int
    , wfcTiles :: [a]
    , wfcLegalAdjacency :: a -> a -> Bool
    }

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
    Just zipper' -> mapCurrent f (Grid zipper')

mapCurrent :: (a -> a) -> Grid a -> Grid a
mapCurrent f (Grid (Zipper xs (Zipper as b cs) zs)) = Grid (Zipper xs (Zipper as (f b) cs) zs)

setCurrent :: a -> Grid a -> Grid a
setCurrent x = mapCurrent (const x)

propagate :: (a -> a -> Bool) -> Grid (WfState a) -> Grid (WfState a)
propagate isAllowedNextTo = go left right . go up down . go right left . go down up
  where
    go there back grid = case there grid of
        Nothing -> grid
        Just grid' -> case (peek (extract grid), peek (extract grid')) of
            (xs, ys) -> if and [ y `isAllowedNextTo` x | x <- xs, y <- ys]
                then grid
                else
                    let grid'' = case filter (\y -> or [ y `isAllowedNextTo` x | x <- xs ]) ys of
                            []  -> setCurrent Contradiction    grid'
                            [x] -> setCurrent (Observed x)     grid'
                            xs' -> setCurrent (Unobserved xs') grid'
                    in back (propagate isAllowedNextTo grid'')

    peek :: WfState a -> [a]
    peek (Unobserved xs) = xs
    peek (Observed x) = [x]
    peek Contradiction = []
