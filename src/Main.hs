module Main (main) where



import           Control.Monad.ST
import           Data.Foldable
import qualified Data.Map                             as M
import qualified Data.Vector                          as V
import           Draw
import           Draw.Plotting
import           Draw.Plotting.PaperSize
import           Geometry
import           Geometry.Processes.PoissonDiscForest
import           Prelude                              hiding (lines)
import qualified System.Random.MWC                    as MWC



width, height :: Double
(width, height) = (paper_a5_long_mm, paper_a5_short_mm)

paperBB :: BoundingBox
paperBB = boundingBox [zero, Vec2 width height]

drawCircle :: Circle
drawCircle = Circle (boundingBoxCenter paperBB) (min width height / 2 - 15)

circleRadius :: Double
circleRadius = 1.5

main :: IO ()
main = do
    let plotSettings = def
            { _previewPenTravelColor = Nothing
            , _canvasBoundingBox = Just paperBB
            }
    let plotResult = runPlot plotSettings $ do
            let (circles, lines) = geometry
            for_ circles plot
            for_ lines plot
    writeGCodeFile "out/poisson_forest_a5.g" plotResult
    renderPreview "out/poisson_forest_a5.png" 3 plotResult

geometry :: ([Circle], [Line])
geometry = (allCircles, allEdges)
  where
    startingPoints = [boundingBoxCenter paperBB]
    forest = runST $ do
        gen <- MWC.initialize (V.fromList [])
        poissonDiscForest gen paperBB 5 32 startingPoints

    allCircles = concat [ circles p | p <- startingPoints ]
    circles parent
        | not (inCircle drawCircle parent) = []
    circles parent = Circle parent circleRadius : case M.lookup parent forest of
        Nothing -> []
        Just children -> concat [ circles child | child <- toList children ]

    allEdges = map (resizeLineSymmetric (\len -> len - 2*circleRadius)) $ concat [ edges p | p <- startingPoints ]
    edges parent
        | not (inCircle drawCircle parent) = []
    edges parent = case M.lookup parent forest of
        Nothing -> []
        Just children -> [Line parent child | child <- toList children, inCircle drawCircle child] ++ concat [ edges child | child <- toList children ]

    inCircle (Circle center radius) p = normSquare (center -. p) <= radius^2
