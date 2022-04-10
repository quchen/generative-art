module Main (main) where



import qualified Data.Vector         as V
import qualified Data.Text.Lazy.IO   as TL
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
picWidth = 500
picHeight = 500

main :: IO ()
main = do
    gen <- initialize (V.fromList [1234])
    let center = Vec2 (picWidth / 2) (picHeight / 2)
        count = 200
        -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox (Vec2 50 50, Vec2 (picWidth - 50) (picHeight - 50))
            , _poissonRadius = adaptiveRadius
            , _poissonK = 4
            }
    points <- poissonDisc gen samplingProps
    print (length points)
    let delaunay = lloydRelaxation 8 $ bowyerWatson (boundingBox (Vec2 0 0, Vec2 picWidth picHeight)) points
        voronoi = toVoronoi delaunay
        polygonInRange (Polygon xs) = all (\p -> norm (center -. p) < 200) xs

    render "out/voronoi-delaunay.png" picWidth picHeight $ do
        setColor grey
        C.paint
        for_ (filter polygonInRange $ getPolygons delaunay) (drawPoly black)
        for_ (filter polygonInRange $ _voronoiRegion <$> _voronoiCells voronoi) (drawPoly white)

    let settings = def
            { _feedrate = Just 6000
            , _zTravelHeight = 5
            , _zDrawingHeight = -2
            , _canvasBoundingBox = Just $ boundingBox (Vec2 0 0, Vec2 400 400)
            }
        removeMargin = G.transform (G.translate (Vec2 (-50) (-50)))
    TL.writeFile "voronoi-delaunay.g" $ runPlot settings $ do
        comment "To be plotted on 50cmx50cm grey paper, with a margin of 5cm."
        comment "Place the origin on the inside of the margin, i.e. at X50 Y50 from the paper corner."
        comment "Start with a black pen."
        for_ (removeMargin $ filter polygonInRange $ getPolygons delaunay) plot
        repositionTo zero
        withDrawingHeight 0 penDown
        pause PauseUserConfirm
        comment "Now change to a white pen."
        penUp
        withFeedrate 3000 $ do -- Lower feedrate for white pen, and draw twice
            for_ (removeMargin $ filter polygonInRange $ _voronoiRegion <$> _voronoiCells voronoi) plot
            for_ (removeMargin $ filter polygonInRange $ _voronoiRegion <$> _voronoiCells voronoi) plot

drawPoly :: Color Double -> Polygon -> Render ()
drawPoly _ (Polygon []) = pure ()
drawPoly color poly = do
    sketch poly
    setColor color
    setLineWidth 1
    stroke

grey :: Color Double
grey = hsv 0 0 0.5
