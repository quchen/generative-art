module Geometry.Contour
where

import Data.Vector as V
import Data.Foldable
import qualified Data.Set as S
import Data.Set (Set)

import Geometry.Core
import Numerics.Interpolation

contours
    :: Grid
    -> (Vec2 -> Double) -- ^ Scalar field
    -> Double           -- ^ Contour threshold
    -> Set Line         -- ^ Contours of the field
contours grid f threshold  =
    let table = valueTable grid f
        tableThresholded = applyThreshold threshold table
        classified = classify tableThresholded
        edges = contourEdges classified

        tolerance = 1e-2
        contourLines = S.map (\iEdges -> optimizeDiscreteLine grid f iEdges tolerance) edges
    in contourLines

-- | Find the root of a scalar field along a line.
findRootOnLine :: (Vec2 -> Double) -> Line -> Double -> Vec2
findRootOnLine f line@(Line start end) tolerance
    | lineLength line <= tolerance = middle
    | signum fStart /= signum fMiddle = findRootOnLine f (Line start middle) tolerance
    | signum fMiddle /= signum fEnd = findRootOnLine f (Line middle end) tolerance
    | otherwise = error "This shouldn’t happen if we only have lines that change sign,\
                        \ picked by marching squares, but I’m sure we’ll be surprised.\
                        \ Might not be worth investigating though, simply abort the alg\
                        \ and have one wonky cell."
  where
    middle = (start +. end) /. 2
    fStart  = f start
    fMiddle = f middle
    fEnd    = f end

optimizeDiscreteLine
    :: Grid
    -> (Vec2 -> Double) -- ^ Scalar field
    -> (IEdge, IEdge)   -- ^ Edges of a discrete cell the contour passes through
    -> Double           -- ^ Tolerance
    -> Line             -- ^ Line between points on the discrete edges, which approximates the real contour
optimizeDiscreteLine grid f (IEdge ivec1start ivec1end, IEdge ivec2start ivec2end) tolerance =
    let edge1 = Line (fromGrid grid ivec1start) (fromGrid grid ivec1end)
        edge2 = Line (fromGrid grid ivec2start) (fromGrid grid ivec2end)
        start = findRootOnLine f edge1 tolerance
        end = findRootOnLine f edge2 tolerance
    in Line start end

-- | Specification of a discrete grid
data Grid = Grid
    { _range :: (Vec2, Vec2)  -- ^ Range of continuous coordinates
    , _numCells :: (Int, Int) -- ^ Number of steps in x/y directions
    }

-- | Convert a function on continuous space to a function on the grid
fToGrid
    :: Grid
    -> (Vec2 -> a) -- ^ Scalar field on continuous coordinates
    -> IVec2 -> a  -- ^ Scalar field on discrete coordinates
fToGrid grid f iVec = f (fromGrid grid iVec)

-- | Map a coordinate from the discrete grid to continuous space
fromGrid
    :: Grid
    -> IVec2 -- ^ Discrete coordinate
    -> Vec2  -- ^ Continuous coordinate
fromGrid (Grid (Vec2 xMin yMin, Vec2 xMax yMax) (xSteps, ySteps)) (IVec2 i j) =
    let x = linearInterpolate ((0, fromIntegral xSteps-1)) ((xMin, xMax)) (fromIntegral i)
        y = linearInterpolate ((0, fromIntegral ySteps-1)) ((yMin, yMax)) (fromIntegral j)
    in Vec2 x y

valueTable :: Grid -> (Vec2 -> a) -> Vector (Vector a)
valueTable grid@Grid{_numCells = (is, js)} f = generate is (\i -> generate js (\j -> fToGrid grid f (IVec2 i j)))

data XO = X | O
    deriving (Eq, Ord, Show)

data CellClassification = CellClassification !XO !XO !XO !XO
    deriving (Eq, Ord, Show)

applyThreshold :: Double -> Vector (Vector Double) -> Vector (Vector XO)
applyThreshold threshold = (fmap.fmap) xo
  where
    xo v | v <= threshold = X
         | otherwise      = O

ifor :: Vector a -> (Int -> a -> b) -> Vector b
ifor = flip imap

classify :: Vector (Vector XO) -> Vector (Vector CellClassification)
classify xos =
    -- Our squares extend from the top left to the bottom right, so we drop
    -- the last element or the bottom-right would run out of bounds of the Vec.
    ifor (V.init xos) $
        \i js -> ifor (V.init js) $
            \j xo ->
                let topLeft     = xo
                    topRight    = xos!(i+1)!j
                    bottomLeft  = xos!i!(j+1)
                    bottomRight = xos!(i+1)!(j+1)
                in CellClassification topLeft topRight bottomLeft bottomRight

data IVec2 = IVec2 !Int !Int
    deriving (Eq, Ord, Show)
data IEdge = IEdge !IVec2 !IVec2
    deriving (Eq, Ord, Show)

contourEdges :: Vector (Vector CellClassification) -> Set (IEdge, IEdge)
contourEdges classifiedCells = fold.fold $ ifor classifiedCells $ \i cy -> ifor cy $ \j classification ->
    let top    = IEdge (IVec2     i     j) (IVec2 (i+1)     j)
        right  = IEdge (IVec2 (i+1)     j) (IVec2 (i+1) (j+1))
        left   = IEdge (IVec2     i     j) (IVec2     i (j+1))
        bottom = IEdge (IVec2     i (j+1)) (IVec2 (i+1) (j+1))

        -- TODO: do this right: check which kind of saddle point we have. For
        -- now, we pick one of the choices arbitrarily.
        disambiguateSaddle this _that = this

    in case classification of
        CellClassification X X
                           X X -> S.empty

        CellClassification X X
                           X O -> S.singleton (bottom, right)

        CellClassification X X
                           O X -> S.singleton (bottom, left)

        CellClassification X X
                           O O -> S.singleton (left, right)

        CellClassification X O
                           X X -> S.singleton (top, right)

        CellClassification X O
                           X O -> S.singleton (top, bottom)

        CellClassification X O
                           O X -> S.fromList $ disambiguateSaddle
                                    [(left, top), (bottom, right)]
                                    [(left, bottom), (top, right)]

        CellClassification X O
                           O O -> S.singleton (left, top)

        CellClassification O X
                           X X -> S.singleton (left, top)

        CellClassification O X
                           X O -> S.fromList $ disambiguateSaddle
                                    [(left, top), (bottom, right)]
                                    [(left, bottom), (top, right)]

        CellClassification O X
                           O X -> S.singleton (top, bottom)

        CellClassification O X
                           O O -> S.singleton (top, right)

        CellClassification O O
                           X X -> S.singleton (left, right)

        CellClassification O O
                           X O -> S.singleton (left, bottom)

        CellClassification O O
                           O X -> S.singleton (right, bottom)

        CellClassification O O
                           O O -> S.empty
