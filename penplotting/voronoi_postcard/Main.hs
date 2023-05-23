module Main (main) where



import qualified Data.Set                     as S
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import           Draw
import           Draw.Plotting
import           Geometry                     as G
import           Geometry.Algorithms.Delaunay
import           Geometry.Algorithms.Sampling
import qualified System.Random.MWC            as MWC



main :: IO ()
main = do
    let width = 228
        height = 150

        outfileG = "out/postcard-delaunay.g"
        outfilePreview = "out/postcard.svg"

        margin = 10
        paperBB = boundingBox [zero, Vec2 width height]
        drawBB = shrinkBoundingBox margin paperBB

    points <- do
        gen <- MWC.initialize (V.fromList [])
        ps <- poissonDisc gen PoissonDiscParams
            { _poissonShape = drawBB
            , _poissonRadius = 5
            , _poissonK = 10
            }
        let ps' = filter (\p -> norm (boundingBoxCenter drawBB -. p) < 59) ps
        pure (lloydRelaxation drawBB 0.1 ps')

    let delaunay = delaunayTriangulation points

        optimize :: Vector Line -> [Polyline Vector]
        optimize lines1 =
            let foo = S.fromList $ toList $ V.map (\(Line start end) -> V.fromList [start, end]) lines1
            in fmap Polyline (minimizePenHovering foo)

        plottingSettings = def { _zTravelHeight = 5 }
        plotResult = runPlot plottingSettings $ do
            for_ (optimize (delaunayEdges delaunay)) plot

    writeGCodeFile outfileG plotResult
    renderPreview outfilePreview 3 plotResult
    pure ()
