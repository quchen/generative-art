-- | Hexagonal coordinate systems.
--
-- Nice article about the topic: https://www.redblobgames.com/grids/hexagons/
module Geometry.Coordinates.Hexagonal where

import Geometry.Core

data Cube = Cube !Int !Int !Int
    deriving (Eq, Ord, Show)

data Axial = Axial !Int !Int
    deriving (Eq, Ord, Show)

cubicalToAxial :: Cube -> Axial
cubicalToAxial (Cube q r _s) = Axial q r

axialToCubical :: Axial -> Cube
axialToCubical (Axial q r) = Cube q r (-q-r)

data Direction
    = R  -- ^ Right
    | UR -- ^ Up+right
    | UL -- ^ Up+left
    | L  -- ^ Left
    | DL -- ^ Down+left
    | DR -- ^ Down+right

class HexagonalCoordinate hex where
    -- ^ Move x steps in a direction
    move :: Direction -> Int -> hex -> hex

    -- ^ Convert a hexagonal coordinate’s center to an Euclidean 'Vec2'.
    toVec2
        :: Double -- ^ Size of a hex cell (radius, side length)
        -> hex
        -> Vec2

    -- ^ Convert a Euclidean 'Vec2' to the coordiante of the hexagon it is in.
    fromVec2
        :: Double -- ^ Size of a hex cell (radius, side length)
        -> Vec2
        -> hex

instance HexagonalCoordinate Cube where
    move dir x (Cube q r s) = case dir of
        R  -> Cube (q+x) (r  )  (s-x)
        UR -> Cube (q+x) (r-x)  (s  )
        UL -> Cube (q  ) (r-x)  (s+x)
        L  -> Cube (q-x) (r  )  (s+x)
        DL -> Cube (q-x) (r+x)  (s  )
        DR -> Cube (q  ) (r+x)  (s-x)

    toVec2 size = toVec2 size . cubicalToAxial

    fromVec2 size (Vec2 x y) =
        let q', r', s' :: Double
            q' = (sqrt(3)/3 * x - 1/3 * y) / size
            r' = (                2/3 * y) / size
            s' = -q'-r'

            q,r,s :: Int
            q = round q'
            r = round r'
            s = round s'

            -- Rounding all three might violate the invariant that
            -- q+r+s=0, so we calculate the discrepancy and discard
            -- the value that was rounded the most.
            qDiff, rDiff, sDiff :: Double
            qDiff = abs (fromIntegral q - q')
            rDiff = abs (fromIntegral r - r')
            sDiff = abs (fromIntegral s - s')
        in if
            -- q had highest diff
            | qDiff > rDiff && qDiff > sDiff -> Cube (-r-s) r s
            -- r had highest diff
            | rDiff > sDiff                  -> Cube q (-q-s) s
            -- s is the only one left
            | otherwise                      -> Cube q r (-q-r)

instance HexagonalCoordinate Axial where
    move dir x (Axial q r) = case dir of
        R  -> Axial (q+x) (r  )
        UR -> Axial (q+x) (r-x)
        UL -> Axial (q  ) (r-x)
        L  -> Axial (q-x) (r  )
        DL -> Axial (q-x) (r+x)
        DR -> Axial (q  ) (r+x)

    toVec2 size (Axial q r) =
        let q' = fromIntegral q
            r' = fromIntegral r
            x = size * (sqrt(3)*q' + sqrt(3)/2*r')
            y = size * (                   3/2*r')
        in Vec2 x y

    fromVec2 size vec = cubicalToAxial (fromVec2 size vec)
