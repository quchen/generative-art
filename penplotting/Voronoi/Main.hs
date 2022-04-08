module Main (main) where



import qualified Data.Vector         as V
import           Prelude             hiding ((**))
import           System.Random.MWC

import           Draw
import           Draw.Plotting
import           Geometry                     as G
import           Geometry.Algorithms.Delaunay
import           Geometry.Algorithms.Sampling
import           Geometry.Algorithms.Voronoi
import           Graphics.Rendering.Cairo     as C



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 720

main :: IO ()
main = do
    gen <- initialize (V.fromList [124])
    let count = 100
        -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox [zero, Vec2 picWidth picHeight]
            , _poissonRadius = adaptiveRadius
            , _poissonK = 4
            }
    points <- poissonDisc gen samplingProps
    print (length points)
    let delaunay = lloydRelaxation 10 $ bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) points
        voronoi = toVoronoi delaunay

    render "out/voronoi.png" picWidth picHeight $ do
        setColor grey
        C.paint
        for_ (getPolygons delaunay) (drawPoly black)
        for_ (_voronoiCells voronoi) drawCell

drawCell :: VoronoiCell () -> Render ()
drawCell VoronoiCell{..} = drawPoly white _voronoiRegion

drawPoly :: Color Double -> Polygon -> Render ()
drawPoly _ (Polygon []) = pure ()
drawPoly color poly = do
    sketch poly
    setColor color
    setLineWidth 1
    stroke

grey :: Color Double
grey = hsv 0 0 0.5
