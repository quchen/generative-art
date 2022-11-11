module Test.Uncategorized.Trajectory (tests) where



import           Data.Foldable
import           Data.List.Extended
import           Data.Maybe
import qualified Data.Set                 as S
import           Data.Vector.Extended     (Vector)
import qualified Data.Vector.Extended     as V
import           Graphics.Rendering.Cairo as Cairo hiding (x, y)
import qualified System.Random.MWC        as MWC

import Draw
import Geometry               as G
import Geometry.Shapes        as G
import Numerics.Interpolation

import Test.TastyAll



tests :: TestTree
tests = testGroup "Trajectories"
    [ simplifyPathTests
    , reassembleLinesTest
    ]

simplifyPathTests :: TestTree
simplifyPathTests = testGroup "Simplify path"
    [ testGroup "Ramer-Douglas-Peucker"
        [ testVisual "Simplify function graph" 400 300 "docs/interpolation/3_simplify_path_rdp"
            (simplifyFunctionGraphTest reds simplifyTrajectoryRdp [ 2**(-e) | e <- [10,9..1]])
        , simplifyKeepsEndPoints "End points are always kept" 1000 simplifyTrajectoryRdp
        , simplifyPathRegressionTest
        ]
    , testGroup "Visvalingam-Whyatt"
        [ testGroup "Smoke tests"
            [ simplifyThreePointsSmokeTest
            , simplifyRegularPolygonSmokeTest
            , simplifyRegularPolygonSmokeTest2
            , simplifyClosedTrajectoryTestSmallParameter
            , simplifyClosedTrajectoryTestHuteParameter
            ]
        , testVisual "Simplify function graph" 400 300 "docs/interpolation/3_simplify_path_vw"
            (simplifyFunctionGraphTest blues simplifyTrajectoryVW [ 2**(-e) | e <- [10,9..1]])
        , simplifyKeepsEndPoints "End points are always kept" 1000 simplifyTrajectoryVW
        ]
    , testGroup "Radial"
        [ testVisual "Simplify function graph" 400 300 "docs/interpolation/3_simplify_path_radial"
            (simplifyFunctionGraphTest greens (\param vec -> V.fromList (simplifyTrajectoryRadial param vec)) [ 2**(-e/2) | e <- [10,9..1]])
        , simplifyKeepsEndPoints "End points are always kept" 1000
            (\param vec -> V.fromList (simplifyTrajectoryRadial param vec))
        ]
    ]

simplifyFunctionGraphTest
    :: (Double -> Color Double)
    -> (param -> Vector Vec2 -> Vector Vec2) -- ^ Algorithm
    -> [param]                               -- ^ Parameters for algorithm, gernerate one graph per entry
    -> (Double, Double)                      -- ^ Width, height
    -> Render ()
simplifyFunctionGraphTest colorScheme f parameters = \(w,h) -> do
    let graph = [Vec2 x (sin (2*x) / (0.5*x)) | x <- [0.1, 0.2 .. 16]]
        graphBB = boundingBox graph
        fitToBox :: Transform geo => geo -> geo
        fitToBox = G.transform
            (transformBoundingBox
                graphBB
                (boundingBox (Vec2 10 10, Vec2 (w-10) (h/3-10)))
                (TransformBBSettings FitWidthHeight IgnoreAspect FitAlignCenter))

    let plotPath points = cairoScope $ do
            setLineWidth 1
            newPath
            sketch (Polyline (toList points))
            stroke
        plotPoints points = for_ points $ \p -> cairoScope $ do
            sketch (Circle p 1.2)
            fillPreserve
            setSourceRGBA 0 0 0 1
            setColor (darken 0.8 (colorScheme 0.9))
            setLineWidth 0.5
            stroke

    cairoScope $ do
        setColor (colorScheme 0.9)
        plotPath (fitToBox graph)
        plotPoints (fitToBox graph)

    cairoScope $
        for_ (zip [1..] parameters) $ \(i, parameter) -> do
            Cairo.translate 0 20
            let simplified = f parameter (V.fromList graph)
            cairoScope $ do
                setColor (black `withOpacity` 0.2)
                plotPath (fitToBox graph)
            cairoScope $ do
                setColor (colorScheme (lerp (1, fromIntegral (length parameters)) (0.8, 0.5) i))
                plotPath (fitToBox simplified)
                plotPoints (fitToBox simplified)

simplifyPathRegressionTest :: TestTree
simplifyPathRegressionTest = localOption (Timeout (10^5) "100ms") $ testCase "Can handle cyclic paths" $ do
    let cyclicPath = V.fromList [Vec2 1 0, Vec2 1 1, Vec2 1 0]
        simplified = simplifyTrajectoryRdp 100 cyclicPath
    assertBool "The algorithm did not terminate" (simplified `seq` True)

simplifyThreePointsSmokeTest :: TestTree
simplifyThreePointsSmokeTest = testCase "Three points (giant threshold → nothing simplified)" $ do
    let !_ = simplifyTrajectoryVW 10 (V.fromList [Vec2 0 0, Vec2 1 1, Vec2 2 0])
    pure ()

simplifyRegularPolygonSmokeTest :: TestTree
simplifyRegularPolygonSmokeTest = testCase "A regular n-gon (giant threshold → nothing simplified)" $ do
    let !_ = simplifyTrajectoryVW 10 (V.fromList (let Polygon corners = regularPolygon 5 in corners))
    pure ()

simplifyRegularPolygonSmokeTest2 :: TestTree
simplifyRegularPolygonSmokeTest2 = testCase "A regular n-gon (small threshold → simplification)" $ do
    let numPoints = 10
        trajectory = V.fromList (let Polygon corners = regularPolygon numPoints in corners)
        simplified = simplifyTrajectoryVW 10 trajectory
    assertBool "Length of the result is wrong" (V.length simplified < numPoints)

simplifyClosedTrajectoryTestSmallParameter :: TestTree
simplifyClosedTrajectoryTestSmallParameter = testCase "Closed trajectory, small parameter" $ do
    let !_ = simplifyTrajectoryVW 0.001 (V.fromList [Vec2 0 0, Vec2 1 0, Vec2 1 1, Vec2 0 1, Vec2 0 0])
    pure ()

simplifyClosedTrajectoryTestHuteParameter :: TestTree
simplifyClosedTrajectoryTestHuteParameter = testCase "Closed trajectory, huge parameter" $ do
    let !_ = simplifyTrajectoryVW 1000 (V.fromList [Vec2 0 0, Vec2 1 0, Vec2 1 1, Vec2 0 1, Vec2 0 0])
    pure ()

simplifyKeepsEndPoints :: TestName -> param -> (param -> Vector Vec2 -> Vector Vec2) -> TestTree
simplifyKeepsEndPoints testName param simplify = testProperty testName $
    let gen = do
            n <- choose (3, 100)
            vecs <- replicateM n $ do
                Gaussian v <- arbitrary
                pure v
            pure (V.fromList (nubOrd vecs))
        shrinker vec = [V.take n vec | n <- [3..V.length vec-1]]
    in forAllShrink gen shrinker $ \vecs ->
        let simplified = toList (simplify param vecs)
        in (head simplified, last simplified) === (V.head vecs, V.last vecs)

fisherYatesList :: [a] -> [a]
fisherYatesList = V.toList . V.modify (\vec -> MWC.create >>= \gen -> V.fisherYatesShuffle gen vec) . V.fromList

reassembleLinesTest :: TestTree
reassembleLinesTest =
    let pairUp xs = zipWith Line xs (tail xs)

        points0 = [Vec2 x 0 | x <- [0..1]]
        points1 = [Vec2 x 1 | x <- [-1..5]]
        points2 = [Vec2 x 2 | x <- [-1..10]]
        points3 = [G.transform (G.rotate angle) (Vec2 0.5 0) | angle <- map deg (takeWhile (<360) [0, 12..])]
        points4 = [polar angle radius | n <- [1..1000], let angle = deg (9*n), let radius = n]
        points5 = fisherYatesList [polar angle radius | n <- [1..1000], let angle = deg (9*n), let radius = 1.1*n]

        allMangledUp = S.fromList (concatMap pairUp [points0, points1, points2, points3, points4, points5])
        reassembled = reassembleLines (\(Line a b) -> (a,b)) allMangledUp

        -- Reverse the list if the first element is larger than the last.
        -- We don’t have to care about whether a line was recognized in reverse this way.
        canonicalize [] = []
        canonicalize [x] = [x]
        canonicalize xs
            | head xs > last xs = reverse xs
            | otherwise = xs

        assertPointsConserved points = do
            let canonicalized = canonicalize points
                indent = ("    " ++)
            assertBool
                (unlines
                    ["One result should have the points"
                    , indent (show canonicalized)
                    , "but none has!"
                    , "Reassembled:"
                    , unlines (map (indent . show . canonicalize . toList) reassembled)])
                (isJust (find (\reassembledLine -> canonicalize (toList reassembledLine) == canonicalized) reassembled))
    in testGroup "Reassembling lines"
        [ testGroup "Example trajectories are assembled correctly"
            [ testCase "Two points on a line"                  (assertPointsConserved points0)
            , testCase "Many points on one line"               (assertPointsConserved points1)
            , testCase "Many points on another line"           (assertPointsConserved points2)
            , testCase "Circular points"                       (assertPointsConserved points3)
            , testCase "Archimedian spiral"                    (assertPointsConserved points4)
            , testCase "Archimedian spiral, randomly permuted" (assertPointsConserved points5)
            ]
        ]
