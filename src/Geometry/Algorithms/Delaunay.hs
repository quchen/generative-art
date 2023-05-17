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
-- >>>    points <- poissonDisc gen PoissonDiscParams
-- >>>       { _poissonShape  = bb
-- >>>       , _poissonRadius = 16
-- >>>       , _poissonK      = 5
-- >>>       }
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
--     for_ (_edges delaunay) $ \edge -> do
--         let startRamp = imageSize * 0.4
--             endRamp = imageSize * 0.8
--             opacity = 1 - smoothstep startRamp endRamp (let Vec2 _ y = edgeCenter edge in y)
--         setColor (mathematica97 3 `withOpacity` opacity)
--         sketch edge
--         stroke
--     for_ (clipEdgesToBox bb (_voronoiEdges delaunay)) $ \edge -> do
--         let startRamp = imageSize * 0.2
--             endRamp = imageSize * 0.6
--             opacity = smoothstep startRamp endRamp (let Vec2 _ y = edgeCenter edge in y)
--         setColor (mathematica97 0 `withOpacity` opacity)
--         sketch edge
--         stroke
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay.hs/delaunay_voronoi.svg
module Geometry.Algorithms.Delaunay (
      delaunayTriangulation
    , Triangulation(..)
    , VoronoiCell(..)
    , Ray(..)
    , D.TriangulationRaw
    , lloydRelaxation
    , clipEdgesToBox
    , clipCellsToBox
    , VoronoiPolygon(..)
) where



import Geometry.Algorithms.Delaunay.Internal.Delaunator.Api
import qualified Geometry.Algorithms.Delaunay.Internal.Delaunator.Raw as D
