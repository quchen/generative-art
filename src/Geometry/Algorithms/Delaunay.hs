-- | Delaunay triangulation and Voronoi diagrams.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay.hs/delaunay_voronoi.svg>>
--
-- === __(image code)__
-- >>> import           Draw
-- >>> import           Geometry.Algorithms.Sampling
-- >>> import           Control.Monad.ST
-- >>> import           Numerics.Functions
-- >>> import           Geometry.Core                as G
-- >>> import           Graphics.Rendering.Cairo     as C
-- >>> import qualified Data.Vector                  as V
-- >>> import qualified System.Random.MWC            as MWC
-- >>>
-- >>> seed = [2]
-- >>> (width, height) = (600::Int, 600::Int)
-- >>> :{
-- >>> points = runST $ do
-- >>>    gen <- MWC.initialize (V.fromList (map fromIntegral seed))
-- >>>    let bb = boundingBox [zero, Vec2 (fromIntegral width) (fromIntegral height)]
-- >>>    points <- poissonDisc gen bb 16 5
-- >>>    let radius = fromIntegral (min width height) / 2.5
-- >>>    pure (filter (\p -> normSquare (p -. boundingBoxCenter bb) <= radius^2) points)
-- >>> :}
--
-- >>> delaunay = delaunayTriangulation points
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay.hs/delaunay_voronoi.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 1)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     let edgeCenter (Line start end) = (start +. end) /. 2
--         imageSize = fromIntegral (min width height)
--     for_ (delaunayEdges delaunay) $ \edge -> do
--         let startRamp = imageSize * 0.4
--             endRamp = imageSize * 0.8
--             opacity = 1 - smoothstep startRamp endRamp (let Vec2 _ y = edgeCenter edge in y)
--         setColor (mathematica97 3 `withOpacity` opacity)
--         sketch edge
--         stroke
--     for_ (clipEdgesToBox bb (voronoiEdges delaunay)) $ \edge -> do
--         let startRamp = imageSize * 0.2
--             endRamp = imageSize * 0.6
--             opacity = smoothstep startRamp endRamp (let Vec2 _ y = edgeCenter edge in y)
--         setColor (mathematica97 0 `withOpacity` opacity)
--         sketch edge
--         stroke
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay.hs/delaunay_voronoi.svg
module Geometry.Algorithms.Delaunay (
    -- * Core
      delaunayTriangulation
    , Api.DelaunayTriangulation

    -- * Accessors
    , delaunayTriangles
    , delaunayEdges
    , voronoiCorners
    , Api.Ray(..)
    , voronoiEdges
    , Api.VoronoiPolygon(..)
    , voronoiCells
    , delaunayHull
    , findClosestInputPoint

    -- * Convenience
    , clipEdgesToBox
    , clipCellsToBox
    , lloydRelaxation
) where



import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import qualified Geometry.Algorithms.Clipping as Clipping
import           Geometry.Core

import qualified Geometry.Algorithms.Delaunay.Internal.Delaunator.Api as Api



-- $setup
-- >>> import           Draw
-- >>> import           Control.Monad.ST
-- >>> import           Geometry.Algorithms.Sampling
-- >>> import           Geometry.Core                as G
-- >>> import           Graphics.Rendering.Cairo     as C
-- >>> import qualified Data.Vector                  as V
-- >>> import qualified System.Random.MWC            as MWC
-- >>>
-- >>> :{
-- >>> numPoints = 2^7
-- >>> seed = [2]
-- >>> (width, height) = (600::Int, 400::Int)
-- >>> points = runST $ do
-- >>>     gen <- MWC.initialize (V.fromList (map fromIntegral seed))
-- >>>     let margin = 100
-- >>>         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
-- >>>     uniformlyDistributedPoints gen bb numPoints
-- >>> delaunay = delaunayTriangulation points
-- >>> :}



-- | Create an (abstract) 'Api.DelaunayTriangulation' from a set of points. The
-- resulting data structure can be queried using various functions in this module.
--
-- Some of the accessor results are aligned, e.g. 'delaunayTriangles' and
-- 'voronoiCorners' have related \(i\)-th entries. Similarly it is possible to
-- annotate accessor results with arbitrary data by simply mapping over or zipping
-- them with the data source.
delaunayTriangulation :: Sequential vector => vector Vec2 -> Api.DelaunayTriangulation
delaunayTriangulation = Api.delaunayTriangulation . toVector



-- | All Delaunay triangles, ordered roughly from the center of the input points’
-- bounding box.
--
-- __Note:__ The circumcenter of the \(i\)-th triangle is the \(i\)-th entry of
-- 'voronoiCorners'. This can be used to 'V.zip' them.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/triangles.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/triangles.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     V.iforM_ (delaunayTriangles delaunay) $ \i triangle -> do
--         setColor (mathematica97 i)
--         sketch (growPolygon (-2) triangle)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/triangles.svg
delaunayTriangles :: Api.DelaunayTriangulation -> Vector Polygon
delaunayTriangles = Api._triangles



-- | Each (undirected) edge of the Delaunay triangulation.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/edges.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/edges.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     V.iforM_ (delaunayEdges delaunay) $ \i edge -> do
--         setColor (mathematica97 i)
--         sketch edge
--         stroke
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/edges.svg
delaunayEdges :: Api.DelaunayTriangulation -> Vector Line
delaunayEdges = Api._edges


-- | Corners of the Voronoi cells, useful for painting them in isolation.
--
-- __Note:__ The \(i\)-th corner is the circumcenter of the \(i\)-th entry of
-- 'delaunayTriangles'. This can be used to 'V.zip' them.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/voronoi_corners.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/voronoi_corners.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     for_ (clipEdgesToBox bb (voronoiEdges delaunay)) $ \edge -> do
--         setColor (mathematica97 0 `withOpacity` 0.2)
--         sketch edge
--         stroke
--     V.iforM_ (voronoiCorners delaunay) $ \i corner -> do
--         setColor (mathematica97 i)
--         sketch (Circle corner 2)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/voronoi_corners.svg
voronoiCorners :: Api.DelaunayTriangulation -> Vector Vec2
voronoiCorners = Api._voronoiCorners



-- | Each edge of the Voronoi diagram. The boundary edges extend to
-- infinity, and are provided as 'Api.Ray's.
--
-- 'clipEdgesToBox' conveniently handles the case of constraining
-- this to a rectangular viewport.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/voronoi_edges.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/voronoi_edges.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     V.iforM_ (clipEdgesToBox bb (voronoiEdges delaunay)) $ \i edge -> do
--         setColor (mathematica97 i)
--         sketch edge
--         stroke
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/voronoi_edges.svg
voronoiEdges :: Api.DelaunayTriangulation -> Vector (Either Line Api.Ray)
voronoiEdges = Api._voronoiEdges



-- | All Voronoi polygons. The polygons at the hull can be infinite.
--
-- 'clipCellsToBox' conveniently handles the case of constraining
-- this to a rectangular viewport.
--
-- __Note:__ The cell of the \(i\)-th input point is the \(i\)-th entry of
-- 'voronoiCells'. This can be used to 'V.zip' them.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/voronoi_cells.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/voronoi_cells.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     V.iforM_ (clipCellsToBox bb (voronoiCells delaunay)) $ \i polygon -> do
--         setColor (mathematica97 i)
--         sketch (growPolygon (-2) polygon)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/voronoi_cells.svg
voronoiCells :: Api.DelaunayTriangulation -> Vector Api.VoronoiPolygon
voronoiCells = Api._voronoiCells



-- | We get the convex hull for free out of the calculation. Equivalent to
-- calling 'convexHull' on the input points, but as a 'Vector'.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/convex_hull.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/convex_hull.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     sketch (Polygon (toList (delaunayHull delaunay)))
--     setColor (mathematica97 1)
--     stroke
--     for_ points $ \p -> do
--         sketch (Circle p 2)
--         setColor (mathematica97 3)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/convex_hull.svg
delaunayHull :: Api.DelaunayTriangulation -> Vector Vec2
delaunayHull = Api._convexHull



-- | Find the index of the closest input point.
--
-- @'findClosestInputPoint' needle start@ returns the index @i@ of the closest input
-- point to @needle@, starting the search at @start@. @start=0@ searches the
-- entire input. @points!i@ is the closest point’s position.
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/find_triangle.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/find_triangle.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--         findThesePoints = runST $ do
--             gen <- MWC.initialize (V.fromList (map (succ . fromIntegral) seed))
--             let margin' = 20
--                 bb' = boundingBox [Vec2 margin' margin', Vec2 (fromIntegral width - margin') (fromIntegral height - margin')]
--             poissonDisc gen bb' 50 4
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     for_ (delaunayEdges delaunay) $ \edge -> do
--         setColor (black `withOpacity` 0.5)
--         sketch edge
--         stroke
--     let foundTriangleIndices = [(p, findClosestInputPoint delaunay p 0) | p <- toList findThesePoints]
--     for_ (zip [0..] foundTriangleIndices) $ \(i, (needle, p)) -> do
--         let closest = points V.! p
--         cairoScope $ do
--             setColor (mathematica97 i)
--             sketch (Circle closest 3) >> fill
--             sketch (Circle needle 3) >> fill
--         cairoScope $ do
--             setColor black
--             sketch (Circle closest 3) >> stroke
--             sketch (Circle needle 3) >> stroke
--         cairoScope $ do
--             setColor (mathematica97 i)
--             sketch (Line needle closest) >> stroke
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/find_triangle.svg
findClosestInputPoint
    :: Api.DelaunayTriangulation
    -> Vec2 -- ^ Needle
    -> Int  -- ^ Start searching at the \(i\)-th input point of
            --   'delaunayTriangulate'. When doing many lookups for 'Vec2's close
            --   together, starting at the index of the previous find yields a
            --   significant speedup, because most of the time we’re already
            --   there.
    -> Int  -- ^ Input
findClosestInputPoint = Api._findClosestInputPoint



-- | Relax the input points by moving them to(wards) their cell’s centroid, leading
-- to a uniform distribution of points. Works well when applied multiple times.
--
-- The parameter \(\omega\) controls how far the Voronoi cell center moves towards
-- the centroid.
-- [See here for a cool live visualization.](https://observablehq.com/@mbostock/lloyds-algorithm)
--
--   * \(0\) does not move the points at all.
--   * \(1\) moves the cell’s centers to the cell’s centroid (standard Lloyd).
--   * \(\sim 2\) overshoots the move towards the cell’s center, leading to faster convergence.
--   * \(<0\) values yield wonky-but-interesting behavior! \(\ddot\smile\)
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/lloyd_relaxation.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/lloyd_relaxation.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     let points' = iterate (lloydRelaxation bb 1) points !! 5
--         delaunay' = delaunayTriangulation points'
--     V.iforM_ (clipCellsToBox bb (voronoiCells delaunay')) $ \i polygon -> do
--         setColor (mathematica97 i)
--         sketch (growPolygon (-2) polygon)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/lloyd_relaxation.svg
lloydRelaxation
    :: (HasBoundingBox boundingBox, Sequential vector)
    => boundingBox
    -> Double -- ^ Convergence factor \(\omega\).
    -> vector Vec2
    -> Vector Vec2
lloydRelaxation bb omega points = relax . voronoiCells . delaunayTriangulation $ pointsVec
  where
    pointsVec = toVector points
    newCenter old cell = old +. omega*.(polygonCentroid cell-.old)

    relax :: Vector Api.VoronoiPolygon -> Vector Vec2
    relax cells = V.zipWith
        (\center polygon -> newCenter center polygon)
        pointsVec
        (clipCellsToBox (boundingBox bb) cells)


-- | Create a stupidly long line out of a 'Ray' so that it definitely spans well
-- over the bounding box.
comicallyLengthen :: BoundingBox -> Api.Ray -> Line
comicallyLengthen bb (Api.Ray start dir) =
    let BoundingBox bbMin bbMax = bb
        boundingBoxDiagonalNormSquare = normSquare (bbMin -. bbMax)
        dirNormSquare = normSquare dir
        end = start +. (max 1 boundingBoxDiagonalNormSquare / max 1 dirNormSquare) *. dir
    in Line start end



-- | Convert a 'Ray' to a 'Line', cutting it off when it hits the 'BoundingBox'.
clipRay
    :: BoundingBox
    -> Api.Ray
    -> Maybe Line -- ^ Nothing if the ray does not hit the bounding box.
clipRay bb ray = Clipping.cohenSutherland bb (comicallyLengthen bb ray)



-- | Cut off all 'Ray's to end at the provided 'BoundingBox'. Convenient to take
-- the result of '_voronoiEdges' and clip it to a rectangular viewport.
clipEdgesToBox
    :: HasBoundingBox boundingBox
    => boundingBox
    -> Vector (Either Line Api.Ray)
    -> Vector Line
clipEdgesToBox bb' segments = do
    let bb = boundingBox bb'
    segment <- segments
    maybe mempty pure $ case segment of
        Left line -> Clipping.cohenSutherland bb line
        Right ray -> clipRay bb ray



-- | Cut off all infinite 'VoronoiCell's with the provided 'BoundingBox'. Convenient to take
-- the result of '_voronoiCells' and clip it to a rectangular viewport.
--
-- This function yields incorrect results when the angle between the directions is
-- too large, because it simply comically enlarges the »infinite« directions to
-- finite size, closes the then finite polygon, and clips the resulting polygon.
-- Since Voronoi cells don’t produce such wide angels for even small point sizes,
-- this is a worthwhile tradeoff. The issue can probably be hacked around by adding
-- another point for all corners enclosed by the direction vectors.
clipCellsToBox
    :: HasBoundingBox boundingBox
    => boundingBox
    -> Vector Api.VoronoiPolygon
    -> Vector Polygon
clipCellsToBox bb' = V.map $ \vPoly -> case vPoly of
    Api.VoronoiFinite polygon -> Clipping.sutherlandHodgman polygon  viewport
    Api.VoronoiInfinite dirIn vertices dirOut ->
        let comicallyLargePolygon = Polygon ([looongIn] ++ vertices ++ [looongOut])
            Line _ looongIn = comicallyLengthen bb (Api.Ray (head vertices) dirIn)
            Line _ looongOut = comicallyLengthen bb (Api.Ray (last vertices) dirOut)
        in Clipping.sutherlandHodgman comicallyLargePolygon viewport
  where
    bb = boundingBox bb'
    viewport = boundingBoxPolygon bb
