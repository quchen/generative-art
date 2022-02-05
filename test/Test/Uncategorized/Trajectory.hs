module Test.Uncategorized.Trajectory (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (x, y)
import qualified Data.Set as S
import Control.Monad.ST
import qualified System.Random.MWC as MWC
import Data.Maybe
import qualified Data.Vector as V

import Draw
import Geometry as G
import Why

import Test.TastyAll



tests :: TestTree
tests = testGroup "Trajactories"
    [ simplifyPathTests
    , reassembleLinesTest
    ]

simplifyPathTests :: TestTree
simplifyPathTests = testGroup "Simplify path"
    [ simplifyFunctionGraphTest
    , simplifyPathRegressionTest
    ]

simplifyFunctionGraphTest :: TestTree
simplifyFunctionGraphTest = testVisual "Simplify function graph" 400 300 "docs/interpolation/3_simplify_path" $ \_ -> do
    let graph = [Vec2 x (sin x / (0.5 * x)) | x <- [0.1, 0.2 .. 16]]
        graphBB = boundingBox graph
        fitToBox :: Transform geo => geo -> geo
        fitToBox = G.transform (transformBoundingBox graphBB (boundingBox (Vec2 10 10, Vec2 (400-10) (100-10))) FitAllIgnoreAspect)

    let plotPath points = cairoScope $ do
            setLineWidth 1
            newPath
            pathSketch points
            stroke
        plotPoints points = for_ points $ \p -> do
            circleSketch p 1
            fillPreserve
            cairoScope $ do
                setSourceRGB 0 0 0
                setLineWidth 0.5
                stroke
        plotBezier segments = cairoScope $ do
            setLineWidth 1
            bezierCurveSketch segments
            stroke
        epsilons = [ 2**(-e) | e <- [10,9..1]]

    cairoScope $ do
        setColor $ mathematica97 0
        plotPath (fitToBox graph)
        plotPoints (fitToBox graph)

    cairoScope $ do
        for_ epsilons $ \epsilon -> do
            Cairo.translate 0 20
            let simplified = simplifyTrajectory epsilon graph
                interpolatedAgain = bezierSmoothen simplified
            setColor $ mathematica97 1
            plotBezier (fitToBox interpolatedAgain)
            plotPoints (fitToBox simplified)

simplifyPathRegressionTest :: TestTree
simplifyPathRegressionTest = localOption (Timeout (10^5) "100ms") $ testCase "Can handle cyclic paths" $ do
    let cyclicPath = [Vec2 1 0, Vec2 1 1, Vec2 1 0]
        simplified = simplifyTrajectory 100 cyclicPath
    assertBool "The algorithm did not terminate" (simplified `seq` True)

fisherYatesList :: [a] -> [a]
fisherYatesList xs = runST $ do
    gen <- MWC.initialize (V.fromList [])
    fmap V.toList (fisherYatesShuffle gen (V.fromList xs))

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
