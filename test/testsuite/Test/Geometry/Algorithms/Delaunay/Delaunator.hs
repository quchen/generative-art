module Test.Geometry.Algorithms.Delaunay.Delaunator (tests) where



import           Control.Monad.ST
import qualified Data.Vector                  as V
import           Draw
import           Geometry
import           Geometry.Algorithms.Delaunay
import           Geometry.Algorithms.Sampling
import           Graphics.Rendering.Cairo
import qualified System.Random.MWC            as MWC

import Test.TastyAll

import qualified Test.Geometry.Algorithms.Delaunay.Internal.Delaunator.Api as Api
import qualified Test.Geometry.Algorithms.Delaunay.Internal.Delaunator.Raw as Raw



tests :: TestTree
tests = testGroup "Delaunator"
    [ Raw.tests
    , Api.tests
    , test_stipple
    ]

test_stipple :: TestTree
test_stipple = testCase "Stipple" $ haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/stipple.png" width height $ do
    setLineWidth 1
    let margin = 10
        bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
        stippleSteps = 10
        omega = 1.8
        f x y = let center = boundingBoxCenter bb
                    p = Vec2 (fromIntegral x) (fromIntegral y)
                    r = norm (p -. center)
                in (sin (r / 10) + 1.1)
        points' = stipple omega width height f points stippleSteps
    cairoScope $ do
        setColor (mathematica97 0)
        setDash [5,5] 0
        sketch (boundingBoxPolygon bb)
        stroke
    for_ points' $ \p -> do
        setColor black
        sketch (Circle p 1)
        fill

  where
    numPoints = 2^8
    seed = [2]
    (width, height) = (300::Int, 200::Int)
    points = runST $ do
        gen <- MWC.initialize (V.fromList (map fromIntegral seed))
        let margin = 20
            bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
        uniformlyDistributedPoints gen bb numPoints
