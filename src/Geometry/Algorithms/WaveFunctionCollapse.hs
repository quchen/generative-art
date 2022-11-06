module WaveFunctionCollapse where

import System.Random.MWC as MWC
import Control.Monad.ST

type Plane a = [[a]]

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

wfc :: Eq a => WfcSettings a -> MWC.GenST x -> ST x (Maybe (Plane a))
wfc WfcSettings{..} gen = go initialPlane
  where
    initialPlane = replicate wfcHeight (replicate wfcWidth (Unobserved wfcTiles))
    go plane = case findMin plane of
        Just minimumElement -> collapse minimumElement plane gen >>= go . propagate minimumElement
        Nothing -> pure (Just (fmap (fmap (\(Observed a) -> a)) plane))

findMin :: Plane (WfState a) -> Maybe Coordinate
findMin = undefined

collapse :: Coordinate -> Plane (WfState a) -> MWC.GenST x -> ST x (Plane (WfState a))
collapse (x, y) plane gen = do
    let Unobserved possibilities = plane !! y !! x
    observed <- pick possibilities gen
    pure (change (x, y) (const (Observed observed)) plane)

pick :: [a] -> GenST x -> ST x a
pick xs gen = do
    let len = length xs
    i <- uniformRM (0, len-1) gen
    pure (xs !! i)

change :: Coordinate -> (a -> a) -> Plane a -> Plane a
change coord f = fmap (\(y, row) -> fmap (\(x, cell) -> if (x, y) == coord then f cell else cell) (zip [0..] row)) . zip [0..]

propagate :: Coordinate -> Plane (WfState a) -> Plane (WfState a)
propagate = undefined
