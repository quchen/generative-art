{-# LANGUAGE RecordWildCards #-}

module Test.Geometry.Algorithms.Sampling (tests) where


import Control.Monad.IO.Class
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC

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

    C.translate 10 10
    cairoScope $ do
        C.rectangle 0 0 width height
        setColor $ mathematica97 1
        C.stroke
        let radius = 16
        points <- liftIO $ poissonDisc PoissonDisc { k = 4, ..  }
        drawPoints points
        C.translate 0 82 >> drawExplanation "Poisson disc"
        C.translate 0 10 >> drawExplanation ("r = " ++ show radius)
        C.translate 0 10 >> drawExplanation ("k = " ++ show (length points))

    C.translate 100 0
    cairoScope $ do
        C.rectangle 0 0 width height
        setColor $ mathematica97 1
        C.stroke
        let radius = 8
        points <- liftIO $ poissonDisc PoissonDisc { k = 4, ..  }
        drawPoints points
        C.translate 0 82 >> drawExplanation "Poisson disc"
        C.translate 0 10 >> drawExplanation ("r = " ++ show radius)
        C.translate 0 10 >> drawExplanation ("k = " ++ show (length points))

    C.translate 100 0
    cairoScope $ do
        C.rectangle 0 0 width height
        setColor $ mathematica97 1
        C.stroke
        let radius = 4
        points <- liftIO $ poissonDisc PoissonDisc { k = 4, ..  }
        drawPoints points
        C.translate 0 82 >> drawExplanation "Poisson disc"
        C.translate 0 10 >> drawExplanation ("r = " ++ show radius)
        C.translate 0 10 >> drawExplanation ("k = " ++ show (length points))

    C.translate 100 0
    cairoScope $ do
        C.rectangle 0 0 width height
        setColor $ mathematica97 1
        C.stroke
        points <- liftIO $ uniformlyDistributedPoints gen width height 300
        drawPoints points
        C.translate 0 82 >> drawExplanation "Random points"
        C.translate 0 10 >> drawExplanation "Uniform distribution"
        C.translate 0 10 >> drawExplanation ("k = " ++ show (length points))


drawPoints :: Foldable f => f Vec2 -> C.Render ()
drawPoints points = cairoScope $ do
    setColor $ mathematica97 0
    for_ points $ \point -> do
        circleSketch point 2
        C.fill

drawExplanation :: String -> C.Render ()
drawExplanation s = cairoScope $ do
    setColor black
    C.moveTo 40 4
    C.setFontSize 8
    showTextAligned HCenter VTop s
