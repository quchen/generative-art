{-# LANGUAGE RecordWildCards #-}

module Test.Geometry.Algorithms.Sampling (tests) where


import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create)

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

    Cairo.translate 10 10
    cairoScope $ do
        Cairo.rectangle 0 0 width height
        setColor $ mathematica97 1
        Cairo.stroke
        let radius = 16
        points <- liftIO $ poissonDisc PoissonDisc { k = 4, ..  }
        drawPoints points
        Cairo.translate 0 82 >> drawExplanation "Poisson disc"
        Cairo.translate 0 10 >> drawExplanation ("r = " ++ show radius)
        Cairo.translate 0 10 >> drawExplanation ("n = " ++ show (length points))

    Cairo.translate 100 0
    cairoScope $ do
        Cairo.rectangle 0 0 width height
        setColor $ mathematica97 1
        Cairo.stroke
        let radius = 8
        points <- liftIO $ poissonDisc PoissonDisc { k = 4, ..  }
        drawPoints points
        Cairo.translate 0 82 >> drawExplanation "Poisson disc"
        Cairo.translate 0 10 >> drawExplanation ("r = " ++ show radius)
        Cairo.translate 0 10 >> drawExplanation ("n = " ++ show (length points))

    Cairo.translate 100 0
    cairoScope $ do
        Cairo.rectangle 0 0 width height
        setColor $ mathematica97 1
        Cairo.stroke
        let radius = 4
        points <- liftIO $ poissonDisc PoissonDisc { k = 4, ..  }
        drawPoints points
        Cairo.translate 0 82 >> drawExplanation "Poisson disc"
        Cairo.translate 0 10 >> drawExplanation ("r = " ++ show radius)
        Cairo.translate 0 10 >> drawExplanation ("n = " ++ show (length points))

    Cairo.translate 100 0
    cairoScope $ do
        Cairo.rectangle 0 0 width height
        setColor $ mathematica97 1
        Cairo.stroke
        points <- liftIO $ uniformlyDistributedPoints gen width height 300
        drawPoints points
        Cairo.translate 0 82 >> drawExplanation "Random points"
        Cairo.translate 0 10 >> drawExplanation "Uniform distribution"
        Cairo.translate 0 10 >> drawExplanation ("n = " ++ show (length points))


drawPoints :: [Vec2] -> Cairo.Render ()
drawPoints points = cairoScope $ do
    setColor $ mathematica97 0
    for_ points $ \point -> do
        circleSketch point 2
        Cairo.fill

drawExplanation :: String -> Cairo.Render ()
drawExplanation s = cairoScope $ do
    setColor black
    Cairo.moveTo 40 4
    Cairo.setFontSize 8
    showTextAligned HCenter VTop s
