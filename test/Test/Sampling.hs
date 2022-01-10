{-# LANGUAGE RecordWildCards #-}
module Test.Sampling (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Draw
import Geometry
import Sampling
import Test.Common

tests :: TestTree
tests = testCase "Poisson-Disc sampling" test
  where
    test = renderAllFormats 400 400 "docs/sampling/poisson-disc" $ do
        Cairo.setSourceRGB 1 1 1 >> Cairo.paint
        gen <- liftIO create
        let radius = 20
            width = 400
            height = 400
        points <- liftIO $ poissonDisc PoissonDisc
            { k = 4
            , ..
            }
        for_ [0, radius / sqrt 2 .. fromIntegral width] $ \x -> do
            Cairo.moveTo x 0
            Cairo.lineTo x (fromIntegral height)
            mmaColor 1 0.2
            Cairo.stroke

        for_ [0, radius / sqrt 2 .. fromIntegral height] $ \y -> do
            Cairo.moveTo 0 y
            Cairo.lineTo (fromIntegral width) y
            mmaColor 1 0.2
            Cairo.stroke

        for_ points $ \point -> do
            circleSketch point (Distance 2)
            mmaColor 0 1
            Cairo.fill
