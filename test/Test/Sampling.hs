{-# LANGUAGE RecordWildCards #-}

module Test.Sampling (tests) where


import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create, uniformRM)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Draw
import Geometry
import Sampling
import Test.Common



tests :: TestTree
tests = testCase "Poisson disc sampling" test
  where
    test = renderAllFormats 400 132 "docs/sampling/poisson-disc" $ do
        gen <- liftIO create
        let width, height :: Num a => a
            width = 80
            height = 80

        Cairo.translate 10 10
        cairoScope $ do
            Cairo.rectangle 0 0 width height
            mmaColor 1 1
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
            mmaColor 1 1
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
            mmaColor 1 1
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
            mmaColor 1 1
            Cairo.stroke
            points <- liftIO $ replicateM 300 $ do
                x <- uniformRM (0, width) gen
                y <- uniformRM (0, height) gen
                pure (Vec2 x y)
            drawPoints points
            Cairo.translate 0 82 >> drawExplanation "Random points"
            Cairo.translate 0 10 >> drawExplanation "Uniform distribution"
            Cairo.translate 0 10 >> drawExplanation ("n = " ++ show (length points))


drawPoints :: [Vec2] -> Cairo.Render ()
drawPoints points = cairoScope $ do
    mmaColor 0 1
    for_ points $ \point -> do
        circleSketch point (Distance 2)
        Cairo.fill

drawExplanation :: String -> Cairo.Render ()
drawExplanation s = cairoScope $ do
    hsva 0 0 0 1
    Cairo.moveTo 40 4
    Cairo.setFontSize 8
    showTextAligned HCenter VTop s

drawGrid :: Double -> Int -> Int -> Cairo.Render ()
drawGrid poissonDiscRadius width height = cairoScope $ do
    mmaColor 1 0.2
    for_ [0, poissonDiscRadius / sqrt 2 .. fromIntegral width] $ \x -> do
        Cairo.moveTo x 0
        Cairo.lineTo x (fromIntegral height)
        Cairo.stroke

    for_ [0, poissonDiscRadius / sqrt 2 .. fromIntegral height] $ \y -> do
        Cairo.moveTo 0 y
        Cairo.lineTo (fromIntegral width) y
        Cairo.stroke
