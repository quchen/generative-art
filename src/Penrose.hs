{-# LANGUAGE RecordWildCards #-}
module Penrose
(
-- * Types
  Tile(..)
, TileType(..)
, twin
, flipTile

-- * Graphical representations
, asPolygon
, inscribedPentagons

-- * Recursive construction
, subdivide

-- * Penrose patterns

-- ** Base tiles
, thinTileBase
, thickTileBase

-- ** Preconfigured tilings
, star1
, star2
, decagonRose
, asymmetricDecagon
) where

import Prelude hiding (length)
import Geometry

-- | A rhombic Penrose tile. Strictly speaking, this is /half/ a tile,
-- because subdividing a full tile will result in half tiles protruding from
-- the original tile. Subdividing half a tile however will produce half tiles
-- that exactly cover the original tile. See 'subdivide'.
data Tile = Tile
    { tileType :: TileType
    , tileP0 :: Vec2
    , tileP1 :: Vec2
    , tileP2 :: Vec2 }
    deriving (Show)

-- | Convert the half tile to a polygon. Note that this should be rendered as
-- an open polygon (i.e., don't use 'Graphics.Rendering.Cairo.closePath').
asPolygon :: Tile -> Polygon
asPolygon Tile{..} = Polygon [tileP0, tileP1, tileP2]
instance Transform Tile where
    transform t f@Tile{..} = f
        { tileP0 = transform t tileP0
        , tileP1 = transform t tileP1
        , tileP2 = transform t tileP2 }

-- | There are two Penrose rhombs: A thick rhomb with an angle of 72°, and a
-- thin rhomb with an angle of 36°.
data TileType = Thin | Thick
    deriving (Eq, Show)

-- | Subdivide a Penrose tile into smaller tiles. While it's possible to
-- generate a Penrose tiling iteratively by adding more tiles to the side, the
-- possible configurations are non-local, i.e. adding a particular tile
-- somewhere can prevent adding tiles (while following the tiling rules)
-- somewhere else.
--
-- Constructing a Penrose tiling recursively via subdivision on the other hand
-- is much easier, and guarantees to have a tiling that adheres to the tiling
-- rules if you start with a correct tiling.
subdivide :: Tile -> [Tile]
subdivide Tile{..} = case tileType of
    Thin ->
        [ Tile
            { tileType = Thick
            , tileP0 = tileP0
            , tileP1 = newPoint
            , tileP2 = tileP1 }
        , Tile
            { tileType = Thin
            , tileP0 = tileP2
            , tileP1 = tileP0
            , tileP2 = newPoint } ]
      where
        newPoint = tileP2 +. (1/phi-1) *. v
        v = tileP2 -. tileP1
    Thick ->
        [ Tile
            { tileType = Thick
            , tileP0 = newPoint2
            , tileP1 = newPoint1
            , tileP2 = tileP0 }
        , Tile
            { tileType = Thin
            , tileP0 = tileP1
            , tileP1 = newPoint2
            , tileP2 = newPoint1 }
        , Tile
            { tileType = Thick
            , tileP0 = tileP2
            , tileP1 = newPoint2
            , tileP2 = tileP1 } ]
      where
        newPoint1 = tileP1 +. (1/phi-1) *. v1
        v1 = tileP1 -. tileP0
        newPoint2 = tileP2 +. (1/phi-1) *. v2
        v2 = tileP2 -. tileP0

-- | The other half of a half tile.
twin :: Tile -> Tile
twin f@Tile{..} = f { tileP1 = mirrorAlong (Line tileP0 tileP2) tileP1 }

-- | Flips a tile: Keeps the same shape, but reverses the orientation. Note
-- that flipping a tile in place will turn a legal configuration to an illegal
-- configuration.
flipTile :: [Tile] -> [Tile]
flipTile = fmap $ \f@Tile{..} -> f { tileP0 = tileP2, tileP2 = tileP0 }

-- | A different graphical representation of a tile. The first-generation
-- Penrose tiling (P1) uses four shapes: Pentagon, Star, Boat, and Diamond.
--
-- Note that like 'asPolygon', the resulting polygons are open, so don't use
-- 'Graphics.Rendering.Cairo.closePath'.
inscribedPentagons :: Tile -> [Polygon]
inscribedPentagons f@Tile{..} = case tileType of
    Thin -> [Polygon [p0, p1, p2, p3]]
      where
        center = tileP2 +. a *. (tileP1 -. tileP2)
        v0 = p0 -. center
        p0 = center +. 1/phi *. (tileP1 -. center)
        p1 = center +. rotate theta v0
        p2 = center +. rotate (2 *. theta) v0
        p3 = center +. 0.5 *. (rotate (2 *. theta) v0 +. rotate (3 *. theta) v0)

    Thick -> [pentagon1, pentagon2]
      where
        pentagon1 = Polygon [p0, p1, p2, p3]
          where
            center = tileP2 +. a *. (tileP0 -. tileP2)
            v1 = p1 -. center
            p0 = center +. 0.5 *. (v1 +. rotate (negateV theta) v1)
            p1 = tileP2 +. a *. (tileP1 -. tileP2)
            p2 = center +. rotate theta v1
            p3 = center +. rotate (2 *. theta) v1
        pentagon2 = Polygon [p0, p1, p2, p3]
          where
            center = tileP1 +. a *. (tileP0 -. tileP1)
            v0 = p0 -. center
            p0 = center +. 1/phi *. (tileP0 -. center)
            p1 = center +. rotate theta v0
            p2 = center +. rotate (2 *. theta) v0
            p3 = center +. 0.5 *. (rotate (2 *. theta) v0 +. rotate (3 *. theta) v0)
  where
    a = 1 - 1/phi
    theta = case polygonOrientation (asPolygon f) of
        PolygonPositive -> rad(2*pi/5)
        PolygonNegative -> rad(-2*pi/5)

phi :: Double
phi = (1+sqrt 5)/2

-- | A thin tile with edge length 1 (two half tiles)
thinTileBase :: [Tile]
thinTileBase = [baseTile, twin baseTile]
  where
    baseTile = Tile
        { tileType = Thin
        , tileP0 = rotate (rad (-pi/10)) $ Vec2 1 0
        , tileP1 = Vec2 0 0
        , tileP2 = rotate (rad (pi/10)) $ Vec2 1 0
        }

-- | A thick tile with edge length 1 (two half tiles)
thickTileBase :: [Tile]
thickTileBase = [baseTile, twin baseTile]
  where
    baseTile = Tile
        { tileType = Thick
        , tileP0 = Vec2 phi 0
        , tileP1 = rotate (rad (-pi/5)) $ Vec2 1 0
        , tileP2 = Vec2 0 0
        }

-- | There are two star configurations, 'star1' and 'star2'. Depending on the
-- orientation of the thick tiles, the pattern grows differently.
star1 :: Vec2 -> Double -> [Tile]
star1 center r =
    let inner = flipTile thickTileBase
    in  (rotateAround center . rad . (2*pi/5 *) <$> [0..4]) <*> scaleTo center r inner

-- | There are two star configurations, 'star1' and 'star2'. Depending on the
-- orientation of the thick tiles, the pattern grows differently.
--
-- 'star2' is the base for 'decagonRose'.
star2 :: Vec2 -> Double -> [Tile]
star2 center r = scaleTo center r ((rotate . rad . (2*pi/5 *) <$> [0..4]) <*> thickTileBase)

-- | A basic Penrose fragment consisting of a 'star2' and some thin tiles
-- around, forming a decagon.
decagonRose :: Vec2 -> Double -> [Tile]
decagonRose center r =
    let outer = translate (Vec2 phi 0) . rotate (rad (7*pi/10)) $ thinTileBase
    in  star2 center r ++ ((rotateAround center . rad . (2*pi/5 *) <$> [0..4]) <*> scaleTo center r outer)

-- | Another Penrose fragment.
asymmetricDecagon :: Vec2 -> Double -> [Tile]
asymmetricDecagon center r = scaleTo center r $ concat
    [ offAxisTiles, mirrorY offAxisTiles, onAxisTiles ]
  where
    origin = Vec2 (-phi) 0
    edge = Vec2 1 0
    f1 = translate origin $ rotate (rad (pi/5)) $ flipTile thickTileBase
    f2 = translate (origin +. edge) $ rotate (rad (3*pi/10)) $ flipTile thinTileBase
    f3 = mirrorAlong (angledLine origin (rad (pi/5)) (Distance 1)) f2
    f4 = translate (origin +. edge) $ flipTile thickTileBase
    f5 = translate edge $ rotate (rad (3*pi/5)) thickTileBase
    f6 = flipTile $ translate (negateV origin) $ rotate (rad (pi/2)) $ translate (rotate (rad (9*pi/10)) edge) thinTileBase
    offAxisTiles = concat [f1, f2, f3, f5]
    onAxisTiles = concat [f4, f6]

scaleTo :: Vec2 -> Double -> [Tile] -> [Tile]
scaleTo center size = transform (translate' center <> scale' (size/phi) (size/phi))
