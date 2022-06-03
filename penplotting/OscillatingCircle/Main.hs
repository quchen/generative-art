module Main (main) where



import           Control.Monad.ST
import           Data.Traversable
import qualified System.Random.MWC as MWC

import Draw                             as D
import Draw.Plotting
import Geometry                         as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.SimplexNoise
import Geometry.Algorithms.Voronoi
import Numerics.Interpolation



main :: IO ()
main = do
    let w = 297
        h = 210
        margin = 20
        paperBB = boundingBox [zero, Vec2 w h]
        drawInsideBB = boundingBox [zero +. Vec2 margin margin, Vec2 w h -. Vec2 margin margin]

    let plotSettings = def
            { _previewDrawnShapesBoundingBox = True
            , _canvasBoundingBox = Just paperBB
            , _previewPenTravelColor = Nothing
            , _previewPenWidth = 1
            }
        plotCells = runPlot plotSettings { _previewPenColor = mathematica97 3, _feedrate = 1000 } $ do
            let (circles, rays) = geometry drawInsideBB
            plot circles
            plot rays

    writeGCodeFile "out/oscillating-circle.g" plotCells
    renderPreview "out/oscillating-circle.svg" plotCells

geometry bb = runST $ do
    gen <- MWC.create

    circles <- for [1..8] $ \_ -> do
        noise <- simplex2
            def { _simplexOctaves = 1
                , _simplexFrequency = let (w,h) = boundingBoxSize bb in 5/min w h }
            gen
        pure $ Polygon $ do
            angle <- fmap deg (takeWhile (< 360) [0,1..])
            let radius = 50
            let cleanPoint = boundingBoxCenter bb +. polar angle radius
                -- noiseOffset = 10 *. (sin (getRad angle/2 + getRad (deg 20))^2 + 0.1) *. polar angle (noise p)
                noiseOffset = 3 *. polar angle (noise cleanPoint)
            pure (cleanPoint +. noiseOffset)

    rays <- for (takeWhile (< 360) [0, 360/100 ..]) $ \rawAngle -> do
        let angle = deg rawAngle
        noise <- simplex2
            def { _simplexOctaves = 1
                , _simplexFrequency = let (w,h) = boundingBoxSize bb in 6/min w h }
            gen
        let radius = 60
        let cleanPoint = boundingBoxCenter bb +. polar angle radius
            noiseOffset = polar angle (3 + 20*abs (noise cleanPoint))

        pure (Line cleanPoint (cleanPoint +. noiseOffset))

    pure (circles, rays)
