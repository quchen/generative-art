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
        points <- liftIO $ poissonDisc PoissonDisc
            { radius = 30
            , k = 10
            , width = 400
            , height = 400
            , ..
            }
        for_ points $ \point -> do
            circleSketch point (Distance 5)
            mmaColor 1 1
            Cairo.fill
