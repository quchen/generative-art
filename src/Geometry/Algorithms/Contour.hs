module Geometry.Algorithms.Contour
where



import           Data.Foldable
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Vector   (Vector, (!))
import qualified Data.Vector   as V
import           Data.Ord
import           Prelude       hiding (lines)

import Geometry.Core
import Numerics.Interpolation
import Geometry.Trajectory



isoLines
    :: Grid
    -> (Vec2 -> Double) -- ^ Scalar field
    -> Double           -- ^ Contour threshold
    -> [[Vec2]]         -- ^ Contours of the field
isoLines grid f  =
    let table = valueTable grid f
    in \threshold ->
        let tableThresholded = applyThreshold threshold table
            classified = classifySquares tableThresholded
            edgeSemengts = classificationsToContourEdgeSegments classified

            reassembled = reassembleLines (\(LineBetweenEdges e1 e2) -> (e1, e2)) edgeSemengts
            tolerance = 1e-3
            sandedDown = (map.map) (\iEdge -> optimizeDiscreteLine grid f threshold iEdge tolerance) reassembled
        in sandedDown

-- | Find the root of a scalar field along a line.
binarySearchRoot :: (Vec2 -> Double) -> Line -> Double -> Vec2
binarySearchRoot f line@(Line start end) tolerance
    | lineLength line <= tolerance = middle
    | signum fStart /= signum fMiddle = binarySearchRoot f (Line start middle) tolerance
    | signum fMiddle /= signum fEnd = binarySearchRoot f (Line middle end) tolerance

    -- EMERGENCY UNCOMMENT IF THE ERROR BELOW COMES UP
    -- otherwise = middle
    | otherwise = bugError "This shouldn’t happen if we only have lines that change sign,\
                           \ picked by marching squares, but I’m sure we’ll be surprised.\
                           \ Might not be worth investigating though, simply abort the alg\
                           \ and have one wonky cell."
  where
    middle = (start +. end) /. 2
    fStart  = f start
    fMiddle = f middle
    fEnd    = f end

optimizeIsoIntersections
    :: Grid
    -> (Vec2 -> Double)    -- ^ Scalar field
    -> Double              -- ^ Contour threshold
    -> LineBetweenEdges -- ^ Edges of a discrete cell the contour passes through
    -> Double              -- ^ Tolerance
    -> Line                -- ^ Line between points on the discrete edges, which approximates the real contour
optimizeIsoIntersections grid f threshold (LineBetweenEdges iEdge1 iEdge2) tolerance =
    let start = optimizeDiscreteLine grid f threshold iEdge1 tolerance
        end   = optimizeDiscreteLine grid f threshold iEdge2 tolerance
    in Line start end

optimizeDiscreteLine :: Grid -> (Vec2 -> Double) -> Double -> IEdge -> Double -> Vec2
optimizeDiscreteLine grid f threshold (IEdge iStart iEnd) tolerance =
    let line = Line (fromGrid grid iStart) (fromGrid grid iEnd)
    in binarySearchRoot (\x -> f x - threshold) line tolerance
    -- in (fromGrid grid iStart +. fromGrid grid iEnd) /. 2

-- | Specification of a discrete grid
data Grid = Grid
    { _range :: (Vec2, Vec2)  -- ^ Range of continuous coordinates
    , _numCells :: (Int, Int) -- ^ Maximum of discrete coords; equivalent to the number of grid coordinates.
    } deriving (Eq, Ord, Show)

-- | Map a coordinate from the discrete grid to continuous space
fromGrid
    :: Grid
    -> IVec2 -- ^ Discrete coordinate
    -> Vec2  -- ^ Continuous coordinate
fromGrid (Grid (Vec2 xMin yMin, Vec2 xMax yMax) (iMax, jMax)) (IVec2 i j) =
    let x = linearInterpolate (0, fromIntegral iMax) (xMin, xMax) (fromIntegral i)
        y = linearInterpolate (0, fromIntegral jMax) (yMin, yMax) (fromIntegral j)
    in Vec2 x y

-- | We first index by i and then j, so that vec!i!j has the intuitive meaning of »go in i/x direction and then in j/y.
-- The drawback is that this makes the table look like downward columns of y
-- values, indexed by x. The more common picture for at least me is to have line
-- numbers and then rows in each line.
valueTable :: Grid -> (Vec2 -> a) -> Vector (Vector a)
valueTable grid@Grid{_numCells = (is, js)} f =
    V.generate is (\i -> -- »x« direction
        V.generate js (\j -> -- »y« direction
            f (fromGrid grid (IVec2 i j))))

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
ifor = flip V.imap

classifySquares :: Vector (Vector XO) -> Vector (Vector CellClassification)
classifySquares xos =
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

data LineBetweenEdges = LineBetweenEdges !IEdge !IEdge
    deriving (Eq, Ord, Show)

classificationsToContourEdgeSegments :: Vector (Vector CellClassification) -> Set LineBetweenEdges
classificationsToContourEdgeSegments classifiedCells = fold.fold $ ifor classifiedCells $ \i cy -> ifor cy $ \j classification ->
    let topLeft     = IVec2 i      j
        topRight    = IVec2 (i+1)  j
        bottomLeft  = IVec2 i      (j+1)
        bottomRight = IVec2 (i+1)  (j+1)

        top    = IEdge topLeft topRight
        right  = IEdge topRight bottomRight
        left   = IEdge topLeft bottomLeft
        bottom = IEdge bottomLeft bottomRight

        -- TODO: do this right: check which kind of saddle point we have. For
        -- now, we pick one of the choices arbitrarily.
        disambiguateSaddle this _that = this

    in case classification of
        CellClassification X X
                           X X -> S.empty

        CellClassification X X
                           X O -> S.singleton (LineBetweenEdges bottom right)

        CellClassification X X
                           O X -> S.singleton (LineBetweenEdges bottom left)

        CellClassification X X
                           O O -> S.singleton (LineBetweenEdges left right)

        CellClassification X O
                           X X -> S.singleton (LineBetweenEdges top right)

        CellClassification X O
                           X O -> S.singleton (LineBetweenEdges top bottom)

        CellClassification X O
                           O X -> S.fromList $ disambiguateSaddle
                                    [(LineBetweenEdges left top), (LineBetweenEdges bottom right)]
                                    [(LineBetweenEdges left bottom), (LineBetweenEdges top right)]

        CellClassification X O
                           O O -> S.singleton (LineBetweenEdges left top)

        CellClassification O X
                           X X -> S.singleton (LineBetweenEdges left top)

        CellClassification O X
                           X O -> S.fromList $ disambiguateSaddle
                                    [(LineBetweenEdges left top), (LineBetweenEdges bottom right)]
                                    [(LineBetweenEdges left bottom), (LineBetweenEdges top right)]

        CellClassification O X
                           O X -> S.singleton (LineBetweenEdges top bottom)

        CellClassification O X
                           O O -> S.singleton (LineBetweenEdges top right)

        CellClassification O O
                           X X -> S.singleton (LineBetweenEdges left right)

        CellClassification O O
                           X O -> S.singleton (LineBetweenEdges left bottom)

        CellClassification O O
                           O X -> S.singleton (LineBetweenEdges right bottom)

        CellClassification O O
                           O O -> S.empty
