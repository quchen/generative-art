module Main (main) where



import Control.Monad
import Data.Foldable
import Data.List
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import Text.Printf

import Draw
import Geometry
import Geometry.Chaotic
import qualified Util.RTree as RT



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 600

scaleFactor :: Double
scaleFactor = 1

resolution :: Int
resolution = 100

ball :: Double -> Vec3 -> Vec3 -> Double
ball radius center q = (radius^2 / normSquare (center -. q))**1.7

main :: IO ()
main = do
    let count = 100
        seed = 0 :: Int
    gen <- initializeMwc seed
    centers <- replicateM count (uniformRM (Vec3 (-300) (-300) (-300), Vec3 300 300 300) gen)
    radii <- replicateM count (uniformRM (25, 75) gen)

    let metaballField :: Vec3 -> Double
        metaballField q
            | norm q > 200 = 0
            | otherwise    = vsum (zipWith ball radii centers) q
        file = printf "out/marching_cubes_%03d.svg" seed
        scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)
        grid = Grid3 (negateV (Vec3 300 300 300), Vec3 300 300 300) (resolution, resolution, resolution)
        components = isoSurfaces grid metaballField 1
        normal = Vec3 1 1 1
        frontFacing = concatMap (cull normal) components
        culledByOcclusion = occlude normal frontFacing

    putStrLn $ "Components: " ++ show (length components)
    putStrLn $ "Total triangles: " ++ show (length culledByOcclusion)

    render file scaledWidth scaledHeight $ do
        C.scale scaleFactor scaleFactor
        C.translate 300 300
        cairoScope (setColor (magma 0.05) >> C.paint)

        for_ culledByOcclusion $ \tri -> cairoScope $ do
            sketch tri
            setColor (inferno 0.75)
            C.setLineWidth 0.5
            C.stroke
    writeSTL (printf "out/marching_cubes_%03d.stl" seed) components



-- | Remove the parts of each projected triangle that are covered by triangles
--   closer to the viewer.
--
--   The viewer looks along @normal@ (the projection direction used in
--   'projection'). A triangle is closer to the viewer the larger its
--   projection along @normal@ is, i.e. the larger @v \`dotProduct\` normal@ is
--   for its vertices.
--
--   We process triangles from front to back, and for each triangle we subtract
--   all triangles in front of it using 'differencePP'. This clips away the
--   parts of the triangle that are hidden behind closer geometry. The visible
--   fragments are then added to the set of occluders for subsequent triangles.
--
--   The occluders are stored in an 'RTree' keyed by their bounding box, so we
--   only need to run the expensive 'differencePP' against the occluders whose
--   bounding boxes actually overlap the current fragment.
occlude :: Vec3 -> [Triangle3] -> [Polygon]
occlude normal triangles = go frontToBack RT.empty
  where
    -- Sort by depth along the viewing direction, closest first. A triangle is
    -- closer to the viewer the larger the dot product of its vertices with the
    -- viewing direction is.
    frontToBack = sortOn (negate . triDepth) triangles

    -- Average dot product of the triangle’s vertices with the viewing
    -- direction. Used as the depth metric.
    triDepth (Triangle3 _ (v1, v2, v3)) =
        (dotProduct v1 normal + dotProduct v2 normal + dotProduct v3 normal) / 3

    -- Process triangles from front to back, accumulating the projected
    -- polygons of all already-drawn (front) triangles as occluders in an
    -- 'RTree' for bounding-box-accelerated overlap queries.
    go :: [Triangle3] -> RT.RTree Polygon -> [Polygon]
    go [] _occluders = []
    go (tri : rest) occluders =
        let projected = projection normal tri
            visible = subtractOccluders [projected] occluders
            occluders' = foldl' (flip RT.insert) occluders visible
        in visible ++ go rest occluders'

-- | Subtract the occluder polygons from each of the given fragments, returning
--   the visible (non-occluded) fragments. Each fragment is kept only if it is
--   an 'Island' produced by the difference; 'Hole' fragments describe holes
--   punched into the subject and are not drawable on their own.
--
--   Only occluders whose bounding box intersects the bounding box of a fragment
--   are passed to 'differencePP', since non-overlapping polygons cannot
--   subtract from each other. The candidate occluders are found via the
--   'RTree' for an efficient bounding-box-accelerated overlap query.
subtractOccluders :: [Polygon] -> RT.RTree Polygon -> [Polygon]
subtractOccluders fragments occluders = concatMap subtractOverlapping fragments
  where
    subtractOverlapping frag =
        let candidates = RT.intersect (boundingBox frag) occluders
        in foldl' (\frags occluder -> concatMap (subtractOne occluder) frags) [frag] candidates
    subtractOne occluder frag =
        [ p | (p, Island) <- differencePP frag occluder, not (isEmptyPolygon p) ]

-- | A polygon with fewer than three corners has no interior and can be
--   discarded.
isEmptyPolygon :: Polygon -> Bool
isEmptyPolygon (Polygon ps) = length ps < 3
