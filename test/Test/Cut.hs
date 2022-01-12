module Test.Cut (tests) where



import           Control.Monad
import           Data.Foldable
import           Data.List
import qualified Data.Map                 as M
import           Graphics.Rendering.Cairo as Cairo hiding (x, y)

import Draw
import Geometry
import Geometry.Cut.Internal
import Geometry.Shapes

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Helpers



tests :: TestTree
tests = testGroup "Cutting things"
    [ testGroup "Internal Helper functions"
        [ rebuildSimpleEdgeGraphTest
        , reconstructConvexPolygonTest
        , sideOfScissorsTest
        , drawCutEdgeGraphTest
        ]
    , testGroup "Public API"
        [ testCase "Cut my line into pieces" lineTest
        , testCase "Convex polygon" cutSquareTest
        , testCase "Concave polygon" complicatedPolygonTest
        , testCase "Cut misses polygon" cutMissesPolygonTest
        , cornerCasesTests
        ]
    ]

simpleCutEdgeGraph :: CutEdgeGraph
simpleCutEdgeGraph = foldl' (\db f -> f db) (CutEdgeGraph mempty)
    [ Vec2 0   0  --> Vec2 1   0  -- +---------+
    , Vec2 1   0  --> Vec2 1   1  -- |         |
    , Vec2 1   1  --> Vec2 0   1  -- |         |
    , Vec2 0   1  --> Vec2 0   0  -- |         |
                                  -- +---------+
    , Vec2 1   0  --> Vec2 0   0  -- |         |
    , Vec2 0   0  --> Vec2 0 (-1) -- |         |
    , Vec2 0 (-1) --> Vec2 1 (-1) -- |         |
    , Vec2 1 (-1) --> Vec2 1   0  -- +---------+
    ]

rebuildSimpleEdgeGraphTest :: TestTree
rebuildSimpleEdgeGraphTest = testCase "Rebuild simple edge graph" $
    let actual = sort (reconstructPolygons PolygonPositive simpleCutEdgeGraph)
        expected = sort [ Polygon [Vec2 0 (-1), Vec2 1 (-1), Vec2 1 0, Vec2 0 0]
                        , Polygon [Vec2 0 0,    Vec2 1 0,    Vec2 1 1, Vec2 0 1] ]
    in assertEqual "" expected actual

reconstructConvexPolygonTest :: TestTree
reconstructConvexPolygonTest = testProperty "Rebuild convex polygon" $
    \(LotsOfGaussianPoints points) ->
        let convexPolygon = convexHull points
            orientation = polygonOrientation convexPolygon
            maxY = maximum (map (\(Vec2 _ y) -> y) points)
            scissorsThatMiss = angledLine (Vec2 0 (maxY + 1)) (Angle 0) (Distance 1)
            cuts = cutAll scissorsThatMiss (polygonEdges convexPolygon)
            actual = reconstructPolygons orientation (buildGraph (polygonEdgeGraph cuts))
            expected = [convexPolygon]
        in actual === expected

lineTest :: IO ()
lineTest = renderAllFormats 220 100 "docs/geometry/cut/1_line" (do
    Cairo.translate 3 32
    let paper = angledLine (Vec2 0 0) (deg 20) (Distance 100)
        scissors = perpendicularBisector paper
        Cut paperStart p paperEnd = cutLine scissors paper
    liftIO $ print $ cutLine scissors scissors

    setLineWidth 1
    hsva 0 0 0 1
    setDash [2,4] 0
    lineSketch scissors
    stroke
    setDash [] 0

    setLineWidth 3
    mmaColor 0 1
    lineSketch (Line paperStart p)
    stroke
    mmaColor 3 1
    lineSketch (Line p paperEnd)
    stroke

    mmaColor 1 1
    setFontSize 12
    moveTo 60 10
    showText "Cut my line in two pieces"
    )

polyCutDraw :: Polygon -> Line -> [Polygon] -> Render ()
polyCutDraw initialPolygon scissors cutResults = do
    drawCutArrow
    drawPolygon 0 initialPolygon
    for_ (zip [0..] cutResults) (\(i, poly) -> drawPolygon i poly)
  where
    drawCutArrow = do
        setLineWidth 1
        hsva 0 0 0 1
        setDash [2,4] 0
        lineSketch scissors
        stroke
        setDash [] 0
        arrowSketch scissors def{arrowheadSize = Distance 5, arrowDrawBody = False}
        stroke
    drawPolygon i polygon = grouped paint $ do
        mmaColor i 1
        for_ (polygonEdges polygon) $ \edge -> do
            arrowSketch edge def
                { arrowheadRelPos   = 0.45
                , arrowheadSize     = Distance 6
                , arrowheadDrawLeft = False
                }
            stroke
        polygonSketch polygon
        strokePreserve
        mmaColor i 0.1
        fill

cutSquareTest :: IO ()
cutSquareTest = do
    let polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        scissors = centerLine (angledLine (Vec2 25 25) (deg 20) (Distance 100))
        cutResult = cutPolygon scissors polygon

    renderAllFormats 170 90 "docs/geometry/cut/2_square" $ do
        polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 10 10)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 90 10)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 90 10)) cutResult)

        mmaColor 1 1
        setFontSize 12
        moveTo 90 80
        showText (show (length cutResult) ++ " polygons")

    assertEqual "Number of resulting polygons" 2 (length cutResult)
    liftIO (assertAreaConserved polygon cutResult)

complicatedPolygonTest :: IO ()
complicatedPolygonTest = do
    let polygon = spiralPolygon 9 20
        scissors = centerLine (angledLine (Vec2 (-5) (-5)) (deg 140) (Distance 220))
        cutResult = cutPolygon scissors polygon

    renderAllFormats 400 190 "docs/geometry/cut/3_complicated" $ do
        polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 90 100)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 290 100)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 290 100)) cutResult)

        mmaColor 1 1
        setFontSize 12
        moveTo 250 15
        showText (show (length cutResult) ++ " polygons")
    assertEqual "Number of resulting polygons" 5 (length cutResult)
    liftIO (assertAreaConserved polygon cutResult)

cutMissesPolygonTest :: IO ()
cutMissesPolygonTest = do
    let scissors = Line (Vec2 0 70) (Vec2 50 60)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    renderAllFormats 130 90 "docs/geometry/cut/4_miss"
        (polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 10 10)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 70 10)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 70 10)) cutResult))

    assertEqual "Number of resulting polygons" 1 (length cutResult)
    liftIO (assertAreaConserved polygon cutResult)

-- These corner cases are terrible. Maybe I’ll rework the alg one day, until then I
-- don’t want to delete them, but I also do not want unused function warnings.
cornerCasesTests :: TestTree
cornerCasesTests = testGroup "Corner cases [IGNORED]" (zigzagTest : cutThroughCornerTest : pathologicalCornerCutsTests)
  where
    ignore = const []

zigzagTest :: TestTree
zigzagTest = testCase "Zigzag" $ do
    let scissors = Line (Vec2 0 25) (Vec2 50 25)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 25 0, Vec2 25 50, Vec2 0 0]
        cutResult = cutPolygon scissors polygon

    renderAllFormats 150 90 "docs/geometry/cut/5_zigzag"
        (polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 10 20)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 80 20)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 80 20)) cutResult))


cutThroughCornerTest :: TestTree
cutThroughCornerTest = testCase "Cut through corner" $ do
    let scissors = Line (Vec2 (-15) (-15)) (Vec2 65 65)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    renderAllFormats 150 90 "docs/geometry/cut/5_through_corner"
        (polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 10 20)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 80 20)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 80 20)) cutResult))

    assertEqual "Number of resulting polygons" 2 (length cutResult)
    liftIO (assertAreaConserved polygon cutResult)

pathologicalCornerCutsTests :: [TestTree]
pathologicalCornerCutsTests = do
    -- Taken from https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/
    (name, filenameSuffix, polygon, expectedNumPolys) <- [ooo, lol, ror, ool, oor, loo, roo]
    [ testCase name $ do
        renderAllFormats 380 100 ("docs/geometry/cut/6_corner_cases_" ++ filenameSuffix)
            (specialCaseTest name polygon)
        --assertEqual "Expected polygons" expectedNumPolys (length (cutPolygon scissors polygon))
        ]
  where
    scissors = Line (Vec2 (-60) 0) (Vec2 180 0)
    specialCaseTest name polygon = cairoScope $ do
        Cairo.translate 0 50
        let cutResult = cutPolygon scissors polygon
            placeOriginal, placeCut :: Transform a => a -> a
            placeOriginal = Geometry.transform (Geometry.translate (Vec2 70 0))
            placeCut = Geometry.transform (Geometry.translate (Vec2 190 0))
        grouped paint $ do
            polyCutDraw
                (placeOriginal polygon)
                (placeOriginal scissors)
                (placeCut cutResult)
            mmaColor 0 1
            for_ (let Polygon corners = polygon in corners) $ \corner -> do
                circleSketch (placeOriginal corner) (Distance 2.5)
                stroke
            for_ cutResult $ \cutPoly ->
                for_ (let Polygon corners = cutPoly in corners) $ \corner -> do
                    circleSketch (placeCut corner) (Distance 2.5)
                    stroke
        let renderDescription = do
                mmaColor 1 1
                let Vec2 x y = placeCut (Vec2 0 0) in moveTo x y >> relMoveTo 70 0
                setFontSize 12
                extents <- textExtents name
                relMoveTo 0 (textExtentsHeight extents / 2)
                showText name
        renderDescription

        --liftIO (assertAreaConserved polygon cutResult)

    ooo = let colinearPoints = [Vec2 (-40) 0, Vec2 0 0, Vec2 40 0]
          in ( "on → on → on"
             , "ooo"
             , Polygon (Vec2 0 40 : colinearPoints)
             , 1 )
    lol = ( "left → on → left"
          , "lol"
          , Polygon [Vec2 0 0, Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40)]
          , 3 )
    ror = ( "right → on → right"
          , "ror"
          , Polygon [Vec2 40 40, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 40, Vec2 0 0]
          , 3 )
    ool = ( "on → on → left"
          , "ool"
          , Polygon [Vec2 0 0, Vec2 0 (-40), Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) 0]
          , 2 )
    oor = ( "on → on → right"
          , "oor"
          , Polygon [Vec2 0 40, Vec2 40 40, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 0, Vec2 0 0]
          , 2 )
    loo = ( "left → on → on"
          , "loo"
          , Polygon [Vec2 0 0, Vec2 40 0, Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40), Vec2 0 (-40)]
          , 2 )
    roo = ( "right → on → on"
          , "roo"
          , Polygon [Vec2 40 0, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 40, Vec2 0 40, Vec2 0 0]
          , 2 )

assertAreaConserved :: Polygon -> [Polygon] -> Assertion
assertAreaConserved polygon cutResult = do
    let Area originalArea = signedPolygonArea polygon
        sumOfCutAreas = sum [ a | Area a <- map signedPolygonArea cutResult ]
    unless (originalArea ~== sumOfCutAreas) (assertFailure (unlines
        ("Total area is not conserved!"
        : map ("    " ++)
            [ "original = " ++ show originalArea
            , "Σ cuts   = " ++ show sumOfCutAreas
            , "|Δ|      = " ++ show (abs (originalArea - sumOfCutAreas)) ])))

drawCutEdgeGraphTest :: TestTree
drawCutEdgeGraphTest = testGroup "Draw cut edge graphs"
    [ testCase "Simple handcrafted graph" $
        renderAllFormats 120 220 "docs/geometry/cut/7_1_handcrafted_edge_graph" $ do
            let cutEdgeGraph = transformAllVecs (100 *.) simpleCutEdgeGraph
                transformAllVecs f (CutEdgeGraph xs) = (CutEdgeGraph . M.fromList . map modify . M.toList) xs
                  where
                    modify (k, [v]) = (f k, [f v])
                    modify (k, [v, v']) = (f k, [f v, f v'])
            Cairo.translate 10 110
            drawCutEdgeGraph PolygonPositive cutEdgeGraph
    , testCase "Simple calculated graph" $
        renderAllFormats 120 120  "docs/geometry/cut/7_2_calculated_edge_graph" $ do
            let polygon = Geometry.transform (Geometry.scale 50) (Polygon [Vec2 1 1, Vec2 (-1) 1, Vec2 (-1) (-1), Vec2 1 (-1)])
                scissors = angledLine (Vec2 0 0) (deg 20) (Distance 1)
                cutEdgeGraph = createEdgeGraph scissors (polygonOrientation polygon) (cutAll scissors (polygonEdges polygon))
            Cairo.translate 60 60
            drawCutEdgeGraph (polygonOrientation polygon) cutEdgeGraph
    ]
  where
    moveRight (Distance d) line = Geometry.transform (Geometry.translate (d *. direction (perpendicularBisector line))) line
    nudge = moveRight (Distance 2.5) . resizeLineSymmetric (\(Distance d) -> Distance (0.85*d))
    arrowSpec = def{arrowheadSize = Distance 7, arrowheadRelPos = 0.5, arrowheadDrawLeft = False}

    drawCutEdgeGraph orientation ceg@(CutEdgeGraph graph) = do
        let reconstructedPolygons = reconstructPolygons orientation ceg
        setLineWidth 1
        for_ (zip [1..] (M.toList graph)) $ \(i, (start, ends)) -> do
            mmaColor 0 1
            circleSketch start (Distance 3)
            strokePreserve
            mmaColor 0 0.3
            fill

            mmaColor i 1
            for_ ends $ \end -> do
                arrowSketch (nudge (Line start end)) arrowSpec
                stroke
        for_ (zip [1..] reconstructedPolygons) $ \(i, polygon) -> do
            mmaColor i 1
            cairoScope $ do
                polygonSketch polygon
                setDash [2,2] 0
                strokePreserve
                mmaColor i 0.1
                fill

            let Vec2 midX midY = polygonAverage polygon
            moveTo midX midY
            showTextAligned HCenter VCenter (show i)

sideOfScissorsTest :: TestTree
sideOfScissorsTest = testProperty "Side of scissors" $
    \(GaussianVec vec@(Vec2 _ y)) ->
        let scissors = Line (Vec2 0 0) (Vec2 1 0)
            actual = sideOfScissors scissors vec
            expected = case compare y 0 of
                LT -> RightOfLine
                EQ -> DirectlyOnLine
                GT -> LeftOfLine
        in actual === expected
