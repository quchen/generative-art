module Test.Geometry.Algorithms.Sampling (tests) where



import Control.Monad.IO.Class
import Graphics.Rendering.Cairo as C
import System.Random.MWC
import Text.Printf

import Draw
import Geometry
import Geometry.Algorithms.Sampling

import Test.TastyAll



tests :: TestTree
tests = testVisual "Poisson disc sampling" 400 132 "docs/sampling/poisson-disc" $ \_ -> do
    gen <- liftIO create
    let width, height :: Num a => a
        width = 80
        height = 80

    setLineWidth 1
    let renderPoisson radius = cairoScope $ do
            rectangle 0 0 width height
            setColor (mathematica97 1)
            stroke
            points <- liftIO $ poissonDisc gen PoissonDiscParams
                { _poissonK      = 4
                , _poissonShape = boundingBox [zero, Vec2 width height]
                , _poissonRadius = radius
                }
            drawPoints points
            C.translate 0 82 >> drawExplanation "Poisson disc"
            C.translate 0 10 >> drawExplanation (printf "r = %.f" radius)
            C.translate 0 10 >> drawExplanation (printf "%d points" (length points))

        renderUniform numPoints = cairoScope $ do
            rectangle 0 0 width height
            setColor (mathematica97 1)
            stroke
            points <- liftIO (uniformlyDistributedPoints gen width height numPoints)
            drawPoints points
            C.translate 0 82 >> drawExplanation "Uniformly random"
            C.translate 0 10
            C.translate 0 10 >> drawExplanation (printf "%d points" numPoints)

    cairoScope $ do
        C.translate 10 10
        renderPoisson 16
        C.translate 100 0
        renderPoisson 8
        C.translate 100 0
        renderPoisson 4
        C.translate 100 0
        renderUniform 300

drawPoints :: Foldable f => f Vec2 -> Render ()
drawPoints points = cairoScope $ do
    setColor (mathematica97 0)
    for_ points $ \point -> do
        sketch (Circle point 1)
        fill

drawExplanation :: String -> Render ()
drawExplanation s = cairoScope $ do
    setColor black
    moveTo 40 4
    setFontSize 8
    showTextAligned HCenter VTop s
