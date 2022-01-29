module Test.Trajectory (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (x, y)
import qualified Data.Set as S
import Data.Maybe

import Draw
import Geometry as G

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Trajactories"
    [ simplifyPathTest
    , reassembleLinesTest
    ]

simplifyPathTest :: TestTree
simplifyPathTest = testCase "Simplify path" (renderAllFormats 400 300 "docs/interpolation/3_simplify_path" simplifyPathTestRender)

simplifyPathTestRender :: Render ()
simplifyPathTestRender = do
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
        setColor $ mmaColor 0 1
        plotPath (fitToBox graph)
        plotPoints (fitToBox graph)

    cairoScope $ do
        for_ epsilons $ \epsilon -> do
            Cairo.translate 0 20
            let simplified = simplifyTrajectory epsilon graph
                interpolatedAgain = bezierSmoothen simplified
            setColor $ mmaColor 1 1
            plotBezier (fitToBox interpolatedAgain)
            plotPoints (fitToBox simplified)

reassembleLinesTest :: TestTree
reassembleLinesTest =
    let pairUp xs = zipWith Line xs (tail xs)

        points0 = [Vec2 x 0 | x <- [-1..0]]
        splitLine0 = pairUp points0

        points1 = [Vec2 x 1 | x <- [-1..5]]
        splitLine1 = pairUp points1

        points2 = [Vec2 x 2 | x <- [-1..10]]
        splitLine2 = pairUp points2

        points3 = [G.transform (G.rotate (deg (n*10))) (Vec2 0.5 0) | n <- [0..11]]
        splitLine3 = pairUp points3

        allMangledUp = S.fromList (concat [splitLine0, splitLine1, splitLine2, splitLine3])
        reassembled = reassembleLines allMangledUp

        assertion points = assertBool
            (unlines
                ["One result should have the points"
                , show points
                , "but none has!"
                , "Reassembled:"
                , unlines (map show reassembled)])
            (isJust (find (\reassembledLine -> S.fromList (toList reassembledLine) == S.fromList points) reassembled))
    in testGroup "Reassemble lines"
        [ testCase "points0" $ assertion points0
        , testCase "points1" $ assertion points1
        , testCase "points2" $ assertion points2
        , testCase "points3" $ assertion points3
        ]
