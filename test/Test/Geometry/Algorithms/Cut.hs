module Test.Geometry.Algorithms.Cut (tests) where



import           Control.Monad
import           Data.Foldable
import           Data.List
import qualified Data.Map                 as M
import           Graphics.Rendering.Cairo as Cairo hiding (x, y)

import Draw
import Geometry
import Geometry.Algorithms.Cut.Internal
import Geometry.Shapes

import Test.TastyAll



tests :: TestTree
tests = testGroup "Cutting things"
    [ testGroup "Internal Helper functions"
        [ rebuildSimpleEdgeGraphTest
        , reconstructConvexPolygonTest
        , sideOfScissorsTest
        , drawCutEdgeGraphTest
        ]
    , testGroup "Public API"
        [ lineTest
        , cutSquareTest
        , complicatedPolygonTest
        , cutMissesPolygonTest
        , cornerCasesTests
        ]
    ]

simpleCutEdgeGraph :: CutEdgeGraph
simpleCutEdgeGraph = foldl' (\db f -> f db) (CutEdgeGraph mempty)
    [ Vec2 0   0  --> Vec2 1   0  -- > +---------+
    , Vec2 1   0  --> Vec2 1   1  -- > |         |
    , Vec2 1   1  --> Vec2 0   1  -- > |         |
    , Vec2 0   1  --> Vec2 0   0  -- > |         |
                                  -- > +---------+
    , Vec2 1   0  --> Vec2 0   0  -- > |         |
    , Vec2 0   0  --> Vec2 0 (-1) -- > |         |
    , Vec2 0 (-1) --> Vec2 1 (-1) -- > |         |
    , Vec2 1 (-1) --> Vec2 1   0  -- > +---------+
    ]

rebuildSimpleEdgeGraphTest :: TestTree
rebuildSimpleEdgeGraphTest = testCase "Rebuild simple edge graph" $
    let actual = sort (reconstructPolygons PolygonPositive simpleCutEdgeGraph)
        expected = sort [ Polygon [Vec2 0 (-1), Vec2 1 (-1), Vec2 1 0, Vec2 0 0]
                        , Polygon [Vec2 0 0,    Vec2 1 0,    Vec2 1 1, Vec2 0 1] ]
    in assertEqual "" (Expected expected) (Actual actual)

reconstructConvexPolygonTest :: TestTree
reconstructConvexPolygonTest = testProperty "Rebuild convex polygon" $
    let gen = do
            n <- choose (10, 100)
            replicateM n $ do
                Gaussian v <- arbitrary
                pure v
    in forAll gen $ \points ->
        let convexPolygon = convexHull points
            orientation = polygonOrientation convexPolygon
            maxY = maximum (map (\(Vec2 _ y) -> y) points)
            scissorsThatMiss = angledLine (Vec2 0 (maxY + 1)) (deg 0) 1
            cuts = cutAll scissorsThatMiss (polygonEdges convexPolygon)
            actual = reconstructPolygons orientation (buildGraph (polygonEdgeGraph cuts))
            expected = [convexPolygon]
        in actual === expected

lineTest :: TestTree
lineTest = testVisual "Cut my line into pieces" 220 100 "docs/geometry/cut/1_line" $ \_ -> do
    Cairo.translate 3 32
    let paper = angledLine (Vec2 0 0) (deg 20) 100
        scissors = perpendicularBisector paper
        Cut paperStart p paperEnd = cutLine scissors paper

    setLineWidth 1
    setColor black
    setDash [2,4] 0
    sketch scissors
    stroke
    setDash [] 0

    setLineWidth 3
    setColor $ mathematica97 0
    sketch (Line paperStart p)
    stroke
    setColor $ mathematica97 3
    sketch (Line p paperEnd)
    stroke

    setColor $ mathematica97 1
    setFontSize 12
    moveTo 60 10
    showText "Cut my line in two pieces"


polyCutDraw :: Polygon -> Line -> [Polygon] -> Render ()
polyCutDraw initialPolygon scissors cutResults = do
    drawCutArrow
    drawPolygon 0 initialPolygon
    for_ (zip [0..] cutResults) (\(i, poly) -> drawPolygon i poly)
  where
    drawCutArrow = do
        setLineWidth 1
        setColor black
        setDash [2,4] 0
        sketch scissors
        stroke
        setDash [] 0
        sketch (Arrow scissors def{arrowheadSize = 5, arrowDrawBody = False})
        stroke
    drawPolygon i polygon = grouped paint $ do
        setColor $ mathematica97 i
        for_ (polygonEdges polygon) $ \edge -> do
            sketch (Arrow edge def
                { arrowheadRelPos   = 0.45
                , arrowheadSize     = 6
                , arrowheadDrawLeft = False
                })
            stroke
        sketch polygon
        strokePreserve
        setColor $ mathematica97 i `withOpacity` 0.1
        fill

cutSquareTest :: TestTree
cutSquareTest = do
    let polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        scissors = centerLine (angledLine (Vec2 25 25) (deg 20) 100)
        cutResult = cutPolygon scissors polygon

    testVisual "Convex polygon" 170 90 "docs/geometry/cut/2_square" $ \_ -> do
        polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 10 10)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 90 10)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 90 10)) cutResult)

        setColor $ mathematica97 1
        setFontSize 12
        moveTo 90 80
        showText (show (length cutResult) ++ " polygons")

        liftIO $ do
            assertEqual "Number of resulting polygons" (Expected 2) (Actual (length cutResult))
            assertAreaConserved polygon cutResult

complicatedPolygonTest :: TestTree
complicatedPolygonTest = do
    let polygon = spiralPolygon 9 20
        scissors = centerLine (angledLine (Vec2 (-5) (-5)) (deg 140) 220)
        cutResult = cutPolygon scissors polygon

    testVisual "Concave polygon" 400 190 "docs/geometry/cut/3_complicated" $ \_ -> do
        polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 90 100)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 290 100)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 290 100)) cutResult)

        setColor $ mathematica97 1
        setFontSize 12
        moveTo 250 15
        showText (show (length cutResult) ++ " polygons")

        liftIO $ do
            assertEqual "Number of resulting polygons" (Expected 5) (Actual (length cutResult))
            assertAreaConserved polygon cutResult

cutMissesPolygonTest :: TestTree
cutMissesPolygonTest = do
    let scissors = Line (Vec2 0 70) (Vec2 50 60)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    testVisual "Cut misses polygon" 130 90 "docs/geometry/cut/4_miss" $ \_ -> do
        polyCutDraw
            (Geometry.transform (Geometry.translate (Vec2 10 10)) polygon)
            (Geometry.transform (Geometry.translate (Vec2 70 10)) scissors)
            (Geometry.transform (Geometry.translate (Vec2 70 10)) cutResult)

        liftIO $ do
            assertEqual "Number of resulting polygons" (Expected 1) (Actual (length cutResult))
            assertAreaConserved polygon cutResult

-- These corner cases are terrible. Maybe I’ll rework the alg one day, until then I
-- don’t want to delete them, but I also do not want unused function warnings.
cornerCasesTests :: TestTree
cornerCasesTests = testGroup "Corner cases" (zigzagTest : cutThroughCornerTest : pathologicalCornerCutsTests)

zigzagTest :: TestTree
zigzagTest = testVisual "Zigzag" 150 90 "docs/geometry/cut/5_zigzag" $ \_ -> do
    let scissors = Line (Vec2 0 25) (Vec2 50 25)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 25 10, Vec2 25 50, Vec2 0 0]
        cutResult = cutPolygon scissors polygon

    (polyCutDraw
        (Geometry.transform (Geometry.translate (Vec2 10 20)) polygon)
        (Geometry.transform (Geometry.translate (Vec2 80 20)) scissors)
        (Geometry.transform (Geometry.translate (Vec2 80 20)) cutResult))

cutThroughCornerTest :: TestTree
cutThroughCornerTest = testVisual "Cut through corner" 150 90 "docs/geometry/cut/5_through_corner" $ \_ -> do
    let scissors = Line (Vec2 (-15) (-15)) (Vec2 65 65)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    (polyCutDraw
        (Geometry.transform (Geometry.translate (Vec2 10 20)) polygon)
        (Geometry.transform (Geometry.translate (Vec2 80 20)) scissors)
        (Geometry.transform (Geometry.translate (Vec2 80 20)) cutResult))

    liftIO $ do
        assertEqual "Number of resulting polygons" (Expected 2) (Actual (length cutResult))
        assertAreaConserved polygon cutResult

pathologicalCornerCutsTests :: [TestTree]
pathologicalCornerCutsTests = do
    -- Taken from https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/
    (name, filenameSuffix, polygon, expectedNumPolys) <- [ooo, lol, ror, ool, oor, loo, roo]
    [ testVisual name 380 100 ("docs/geometry/cut/6_corner_cases_" ++ filenameSuffix) $ \_ -> do
            specialCaseTest name polygon
            liftIO $ assertEqual "Expected polygons" (Expected expectedNumPolys) (Actual (length (cutPolygon scissors polygon)))
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
            setColor $ mathematica97 0
            for_ (let Polygon corners = polygon in corners) $ \corner -> do
                circleSketch (placeOriginal corner) 2.5
                stroke
            for_ cutResult $ \cutPoly ->
                for_ (let Polygon corners = cutPoly in corners) $ \corner -> do
                    circleSketch (placeCut corner) 2.5
                    stroke
        let renderDescription = do
                setColor $ mathematica97 1
                let Vec2 x y = placeCut (Vec2 0 0) in moveTo x y >> relMoveTo 70 0
                setFontSize 12
                extents <- textExtents name
                relMoveTo 0 (textExtentsHeight extents / 2)
                showText name
        renderDescription

        liftIO (assertAreaConserved polygon cutResult)

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
    let originalArea = signedPolygonArea polygon
        sumOfCutAreas = sum (map signedPolygonArea cutResult)
    unless (originalArea ~== sumOfCutAreas) (assertFailure (unlines
        ("Total area is not conserved!"
        : map ("    " ++)
            [ "original = " ++ show originalArea
            , "Σ cuts   = " ++ show sumOfCutAreas
            , "|Δ|      = " ++ show (abs (originalArea - sumOfCutAreas)) ])))

drawCutEdgeGraphTest :: TestTree
drawCutEdgeGraphTest = testGroup "Draw cut edge graphs"
    [ testVisual "Simple handcrafted graph" 120 220 "docs/geometry/cut/7_1_handcrafted_edge_graph" $ \_ -> do
        let cutEdgeGraph = transformAllVecs (100 *.) simpleCutEdgeGraph
            transformAllVecs f (CutEdgeGraph xs) = (CutEdgeGraph . M.fromList . map modify . M.toList) xs
                where
                modify (k, vs) = (f k, f <$> vs)
        Cairo.translate 10 110
        drawCutEdgeGraph PolygonPositive cutEdgeGraph
    , testVisual "Simple calculated graph" 120 120  "docs/geometry/cut/7_2_calculated_edge_graph" $ \_ -> do
        let polygon = Geometry.transform (Geometry.scale 50) (Polygon [Vec2 1 1, Vec2 (-1) 1, Vec2 (-1) (-1), Vec2 1 (-1)])
            scissors = angledLine (Vec2 0 0) (deg 20) 1
            cutEdgeGraph = createEdgeGraph scissors (polygonOrientation polygon) (cutAll scissors (polygonEdges polygon))
        Cairo.translate 60 60
        drawCutEdgeGraph (polygonOrientation polygon) cutEdgeGraph
    ]
  where
    moveRight d line = Geometry.transform (Geometry.translate (d *. direction (perpendicularBisector line))) line
    nudge = moveRight 2.5 . resizeLineSymmetric (\d -> 0.85*d)
    arrowSpec = def{arrowheadSize = 7, arrowheadRelPos = 0.5, arrowheadDrawLeft = False}

    drawCutEdgeGraph orientation ceg@(CutEdgeGraph graph) = do
        let reconstructedPolygons = reconstructPolygons orientation ceg
        setLineWidth 1
        for_ (zip [1..] (M.toList graph)) $ \(i, (start, ends)) -> do
            setColor $ mathematica97 0
            circleSketch start 3
            strokePreserve
            setColor $ mathematica97 0 `withOpacity` 0.3
            fill

            setColor $ mathematica97 i
            for_ ends $ \end -> do
                sketch (Arrow (nudge (Line start end)) arrowSpec)
                stroke
        for_ (zip [1..] reconstructedPolygons) $ \(i, polygon) -> do
            setColor $ mathematica97 i
            cairoScope $ do
                sketch polygon
                setDash [2,2] 0
                strokePreserve
                setColor $ mathematica97 i `withOpacity` 0.1
                fill

            let Vec2 midX midY = polygonAverage polygon
            moveTo midX midY
            showTextAligned HCenter VCenter (show i)

sideOfScissorsTest :: TestTree
sideOfScissorsTest = testProperty "Side of scissors" $
    \(Gaussian vec@(Vec2 _ y)) ->
        let scissors = Line (Vec2 0 0) (Vec2 1 0)
            actual = sideOfScissors scissors vec
            expected = case compare y 0 of
                LT -> RightOfLine
                EQ -> DirectlyOnLine
                GT -> LeftOfLine
        in actual === expected
