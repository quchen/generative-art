-- | The Delaunay triangulation tries to maximize the smallest angle of a
-- triangulation of vertices.
--
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay.hs/delaunay.svg" 300 200 $ do
--     let (width, height) = (300, 200)
--     randomPoints <- liftIO $ do
--         gen <- MWC.create
--         poissonDisc gen PoissonDiscParams
--             { _poissonRadius = width * height / 1600
--             , _poissonK      = 4
--             , _poissonWidth  = round width
--             , _poissonHeight = round height
--             }
--     let delaunay = lloydRelaxation 3 (bowyerWatson (boundingBox [zero, Vec2 width height]) randomPoints)
--     setLineJoin LineJoinBevel
--     for_ (getPolygons delaunay) $ \polygon@(Polygon ps) -> cairoScope $ do
--         sketch polygon
--         setColor (mathematica97 0)
--         stroke
--         setColor (mathematica97 1)
--         for_ ps $ \p -> do
--             sketch (Circle p 3)
--             fill
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay.hs/delaunay.svg
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay.hs/delaunay.svg>>
module Geometry.Algorithms.Delaunay (
      DelaunayTriangulation()
    , getPolygons
    , bowyerWatson
    , bowyerWatsonStep

    , toVoronoi
    , lloydRelaxation
) where



import Geometry.Algorithms.Delaunay.Internal

-- $setup
-- >>> import           Draw
-- >>> import           Geometry.Algorithms.Sampling
-- >>> import           Geometry.Core                as G
-- >>> import           Graphics.Rendering.Cairo     as C
-- >>> import qualified System.Random.MWC            as MWC
