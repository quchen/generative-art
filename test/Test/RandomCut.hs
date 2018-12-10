{-# LANGUAGE ScopedTypeVariables #-}

module Test.RandomCut (tests) where



import           Control.Exception
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.List
import qualified Data.Set                  as S
import           Graphics.Rendering.Cairo  hiding (x, y)
import           System.Random

import Comparison
import Draw
import Geometry
import Geometry.Processes.RandomCut

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Random Cut"
    [ testSquare
    , testHaskellLogo
    -- , hackyFindSeed
    ]

testSquare :: TestTree
testSquare = testCase "Square" test
  where
    cutResult
      = let gen = mkStdGen 0
            initialPolygon = Polygon [Vec2 0 0, Vec2 200 0, Vec2 200 200, Vec2 0 200]
            recurse polygon = minMaxAreaRatio [polygon, initialPolygon] >= 1/30
            accept polys = minMaxAreaRatio polys >= 1/3
            (cutResult', _gen') = randomCutProcess recurse accept initialPolygon gen
        in cutResult'
    test = renderAllFormats 220 220 "test/out/cut_random/1_square" $ do
        setLineWidth 1
        translate 10 10
        let setColors = map mmaColor [0..]
        restoreStateAfter $ for_ (zip setColors cutResult) $ \(setColor, polygon) -> do
            polygonSketch polygon
            setColor 0.5
            fill
        restoreStateAfter $ do
            for_ (S.fromList (cutResult >>= polygonEdges)) $ \edge -> lineSketch edge
            hsva 0 0 0 1
            setLineWidth 0.5
            stroke

testHaskellLogo :: TestTree
testHaskellLogo = testCase "Haskell logo" test
  where
    cutResult
      = let gen = mkStdGen 6
            recurse polygon = minMaxAreaRatio (polygon : haskellLogo) >= 1/64
            accept polys = minMaxAreaRatio polys >= 1/3
        in evalState (fmap concat (traverse (\polygon -> state (randomCutProcess recurse accept polygon)) haskellLogo)) gen
    test = renderAllFormats 500 360 "test/out/cut_random/2_haskell_logo" $ do
        setLineWidth 1
        translate 10 10
        let setColors = zipWith4 hsva
                (repeat 0)
                (repeat 0)
                (randomRs (0.6, 0.9) (mkStdGen 13))
                (repeat 1)
        restoreStateAfter $ for_ (zip setColors cutResult) $ \(setColor, polygon) -> do
            polygonSketch polygon
            setColor
            fill
        restoreStateAfter $ do
            for_ (S.fromList (cutResult >>= polygonEdges)) $ \edge -> lineSketch edge
            hsva 0 0 0 1
            setLineWidth 0.5
            stroke

-- Since the cutting algorithm isn’t very robust yet, this test can be abused to
-- brute-force a seed that makes the polygon cutting process not crash.
hackyFindSeed :: TestTree
hackyFindSeed = testCase "Find seed" (test 0)
  where
    cutResult seed
      = let gen = mkStdGen seed
            recurse polygon = minMaxAreaRatio (polygon : haskellLogo) >= 1/64
            accept polys = minMaxAreaRatio polys >= 1/3
        in evalState (fmap concat (traverse (\polygon -> state (randomCutProcess recurse accept polygon)) haskellLogo)) gen
    attempt seed = length (show (cutResult seed)) `seq` pure ()

    test seed = catch (attempt seed >> assertFailure ("Seed: " ++ show seed))
                      (\(_ :: ErrorCall) -> test (seed+1))
