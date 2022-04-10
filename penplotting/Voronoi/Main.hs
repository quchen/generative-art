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
picHeight = 1000

main :: IO ()
main = do
    gen <- initialize (V.fromList [1234])
    let center = Vec2 (picWidth / 2) (picHeight / 2)
        count = 200
        -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox (Vec2 100 100, Vec2 (picWidth - 100) (picHeight - 100))
            , _poissonRadius = adaptiveRadius
            , _poissonK = 4
            }
    points <- poissonDisc gen samplingProps
    print (length points)
    let delaunay = lloydRelaxation 8 $ bowyerWatson (boundingBox (Vec2 0 0, Vec2 picWidth picHeight)) points
        voronoi = toVoronoi delaunay
        polygonInRange (Polygon xs) = all (\p -> norm (center -. p) < 400) xs

    render "out/voronoi.png" picWidth picHeight $ do
        setColor grey
        C.paint
        for_ (filter polygonInRange $ getPolygons delaunay) (drawPoly black)
        for_ (filter polygonInRange $ _voronoiRegion <$> _voronoiCells voronoi) (drawPoly white)

drawPoly :: Color Double -> Polygon -> Render ()
drawPoly _ (Polygon []) = pure ()
drawPoly color poly = do
    sketch poly
    setColor color
    setLineWidth 1
    stroke

grey :: Color Double
grey = hsv 0 0 0.5
