module Test.Geometry.Algorithms.Contour (tests) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import qualified Data.Map.Strict          as Map
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C
import qualified System.Random.MWC        as MWC

import Draw
import Geometry as G

import Test.TastyAll



tests :: TestTree
tests = testGroup "Contour finding"
    [ optimizeDiscreteLineTests
    , applyThresholdTests
    , classifyTests
    , contourEdgesTests
    , closedIsoIsClosedTest
    , marchingCubesTests
    , visualTests
    ]

optimizeDiscreteLineTests :: TestTree
optimizeDiscreteLineTests = testGroup "Optimize discrete line" []

applyThresholdTests :: TestTree
applyThresholdTests = testGroup "Apply threshold" []

classifyTests :: TestTree
classifyTests = testGroup "Classify" []

contourEdgesTests :: TestTree
contourEdgesTests = testGroup "Contour edges" []

closedIsoIsClosedTest :: TestTree
closedIsoIsClosedTest = testCase "Closed iso line results in closed trajectory" $ do
    let iso = head (isoLines (Grid (Vec2 (-2) (-2), Vec2 2 2) (10, 10)) normSquare 1)
    assertBool "First and last entries are not the same" (head iso == last iso)

marchingCubesTests :: TestTree
marchingCubesTests = testGroup "Marching cubes"
    [ sphereProducesTriangles
    , emptyFieldNoTriangles
    , singleComponentSphere
    , sphereIsClosedManifold
    ]

sphereProducesTriangles :: TestTree
sphereProducesTriangles = testCase "Sphere iso surface produces triangles" $ do
    let grid = Grid3 (Vec3 (-2) (-2) (-2), Vec3 2 2 2) (10, 10, 10)
        f (Vec3 x y z) = x*x + y*y + z*z
        components = isoSurfaces grid f 1
        totalTris = sum (map length components)
    assertBool "Expected some triangles for sphere" (totalTris > 0)

emptyFieldNoTriangles :: TestTree
emptyFieldNoTriangles = testCase "Uniform field produces no triangles" $ do
    let grid = Grid3 (Vec3 (-2) (-2) (-2), Vec3 2 2 2) (10, 10, 10)
        f _ = 5
        components = isoSurfaces grid f 1
        totalTris = sum (map length components)
    assertEqual "Expected no triangles for uniform field above threshold" (Expected 0) (Actual totalTris)

singleComponentSphere :: TestTree
singleComponentSphere = testCase "Sphere iso surface is a single component" $ do
    let grid = Grid3 (Vec3 (-2) (-2) (-2), Vec3 2 2 2) (15, 15, 15)
        f (Vec3 x y z) = x*x + y*y + z*z
        components = isoSurfaces grid f 1
    assertEqual "Expected single connected component for sphere" (Expected 1) (Actual (length components))

sphereIsClosedManifold :: TestTree
sphereIsClosedManifold = testCase "Sphere iso surface is a closed manifold (every edge appears exactly twice)" $ do
    let grid = Grid3 (Vec3 (-2) (-2) (-2), Vec3 2 2 2) (15, 15, 15)
        f (Vec3 x y z) = x*x + y*y + z*z
        components = isoSurfaces grid f 1
        allTris = concat components
        edgeCounts = Map.fromListWith (+)
            [ (edge, 1)
            | Triangle3 _ (a, b, c) <- allTris
            , edge <- [canonicalEdge a b, canonicalEdge b c, canonicalEdge c a]
            ]
        badEdges = Map.filter (/= 2) edgeCounts
    assertEqual "Expected all edges to appear exactly twice" (Expected 0) (Actual (Map.size badEdges))

canonicalEdge :: Vec3 -> Vec3 -> (Double, Double, Double, Double, Double, Double)
canonicalEdge a b
    | a <= b    = toTuple a b
    | otherwise = toTuple b a
  where
    toTuple (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
        (roundTo 6 x1, roundTo 6 y1, roundTo 6 z1, roundTo 6 x2, roundTo 6 y2, roundTo 6 z2)
    roundTo n x = fromIntegral (round (x * 10^n) :: Int) / 10^n

visualTests :: TestTree
visualTests = testGroup "Visual"
    [ testVisual "Single parabola" 100 100 "docs/iso_lines/parabola" $ \(w, h) -> do
        let gridDimension = (Vec2 (-10) (-10), Vec2 10 10)
            isos = Polyline <$> isoLines (Grid gridDimension (10, 10)) (\(Vec2 x y) -> y-0.1*x*x) 0
            fitToBox :: (HasBoundingBox geo, Transform geo) => geo -> geo
            fitToBox =
                G.transform (G.transformBoundingBox gridDimension (Vec2 (0+10) (0+10), Vec2 (w-10) (h-10)) def)
        cairoScope $ do
            setLineWidth 1
            for_ (fitToBox isos) sketch
            setColor (mma 0)
            stroke

    ,  testVisual "Concentric circles" 100 100 "docs/iso_lines/circles" $ \(w, h) -> do
        cartesianCoordinateSystem def
        for_ (zip [1..] [1,2..20]) $ \(colorIndex, r) -> do
            let gridDimension = (Vec2 (-10) (-10), Vec2 10 10)
                gridResolution = (32, 32)
                isos = Polyline <$> isoLines (Grid gridDimension gridResolution) (\(Vec2 x y) -> x*x+y*y) (r*r)
                fitToBox :: (HasBoundingBox geo, Transform geo) => geo -> geo
                fitToBox =
                    G.transform (G.transformBoundingBox gridDimension (Vec2 0 0, Vec2 w h) def)
            cairoScope $ do
                setLineWidth 1
                for_ (fitToBox isos) sketch
                setColor (mma colorIndex)
                stroke

    , testVisual "Bubble iso lines" 320 220 "docs/iso_lines/potentials" $ \_ -> do
        let geometry =
                let coulombPotential q center = \v -> q / (0.001 + normSquare (v -. center))
                    randomCharges = runST $ do
                        gen <- MWC.initialize (V.fromList [2])
                        fs <- replicateM 16 $ do
                            x <- MWC.uniformRM (70, 330) gen
                            y <- MWC.uniformRM (70, 230) gen
                            let q = 1000
                                center = Vec2 x y
                            pure (coulombPotential q center)
                        pure (\v -> sum [f v | f <- fs])
                in randomCharges

            gridDimension = (Vec2 (-400) (-300), Vec2 400 300)
            resolutionFactor = 80
            gridResolution = (4*resolutionFactor, 3*resolutionFactor)
            grid = Grid gridDimension gridResolution

            isoLinesAtThreshold = isoLines grid geometry

        C.translate (-20) (-50)
        for_ (zip [0..] [2**fromIntegral n | n <- [1..7]]) $ \(colorIx, threshold) -> do
            let isos = isoLinesAtThreshold threshold
            cairoScope $ do
                setLineWidth 1
                setColor (mma colorIx `withOpacity` threshold)
                for_ isos $ \path -> sketch (Polyline (toList (simplifyTrajectoryRdp 0.4 (V.fromList path))))
                stroke
    ]
