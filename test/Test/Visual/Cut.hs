module Test.Visual.Cut (tests) where



import           Data.Foldable
import           Data.List
import qualified Data.Map                 as M
import           Graphics.Rendering.Cairo hiding (rotate, x, y)

import Draw
import Geometry
import Geometry.Cut.Internal

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Visual.Common

import Test.Helpers



tests :: TestTree
tests = testGroup "Cutting things"
    [ testGroup "Internal Helper functions"
        [ rebuildSimpleEdgeGraphTest
        , reconstructConvexPolygonTest
        , classifyCutTest
        , sideOfScissorsTest
        , drawSimpleCutEdgeGraphTest
        ]
    , testGroup "Public API"
        [ testCase "Cut my line into pieces" lineTest
        , testCase "Convex polygon" cutSquareTest
        , testCase "Concave polygon" complicatedPolygonTest
        , testCase "Cut misses polygon" cutMissesPolygonTest
        , testCase "Cut through corner" cutThroughCornerTest
        , testCase "Edge cases" cornerCasesTest
        ]
    ]

simpleCutEdgeGraph :: CutEdgeGraph
simpleCutEdgeGraph = foldl' (\db f -> f db) mempty
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
    let actual = sort (reconstructPolygons simpleCutEdgeGraph)
        expected = sort [ Polygon [Vec2 0 (-1), Vec2 1 (-1), Vec2 1 0, Vec2 0 0]
                        , Polygon [Vec2 0 0,    Vec2 1 0,    Vec2 1 1, Vec2 0 1] ]
    in assertEqual "" expected actual

reconstructConvexPolygonTest :: TestTree
reconstructConvexPolygonTest = testProperty "Rebuild convex polygon" $
    \(LotsOfGaussianPoints points) ->
        let convexPolygon = convexHull points
            maxY = maximum (map (\(Vec2 _ y) -> y) points)
            scissorsThatMiss = angledLine (Vec2 0 (maxY + 1)) (Angle 0) (Distance 1)
            cuts = cutAll scissorsThatMiss (polygonEdges convexPolygon)
            actual = reconstructPolygons (polygonEdgeGraph cuts mempty)
            expected = [convexPolygon]
        in actual === expected

lineTest :: IO ()
lineTest = renderAllFormats 220 100 "test/out/cut/1_line" (do
    translate 3 32
    let paper = angledLine (Vec2 0 0) (deg 20) (Distance 100)
        scissors = perpendicularBisector paper
        Cut paperStart p paperEnd = cutLine scissors paper

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
    for_ (zip [0..] cutResults) (\(color, poly) -> drawPolygon color poly)
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
    drawPolygon color poly = grouped paint $ do
        mmaColor color 1
        for_ (polygonEdges poly) $ \edge -> do
            arrowSketch edge def
                { arrowheadRelPos = Distance 0.45
                , arrowheadSize   = Distance 4
                }
            stroke
        polygonSketch poly
        strokePreserve
        mmaColor color 0.1
        fill

cutSquareTest :: IO ()
cutSquareTest = do
    let square = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        scissors = centerLine (angledLine (Vec2 25 25) (deg 20) (Distance 100))
        cutResult = cutPolygon scissors square

    renderAllFormats 170 90 "test/out/cut/2_square" $ do
        polyCutDraw
            (move (Vec2 10 10) square)
            (move (Vec2 90 10) scissors)
            (move (Vec2 90 10) cutResult)

        mmaColor 1 1
        setFontSize 12
        moveTo 90 80
        showText (show (length cutResult) ++ " polygons")

    assertEqual "Number of resulting polygons" 2 (length cutResult)

complicatedPolygonTest :: IO ()
complicatedPolygonTest = do
    let polygon = spiral 9
        spiral n = Polygon (scanl (+.) (Vec2 0 0) relativeSpiral)
          where
            instructions = concat [ zip [1..n] (repeat turnLeft)
                                  , [(1, turnLeft)]
                                  , [(n-1, turnRight)]
                                  , zip [n-3, n-4 .. 1] (repeat turnRight)
                                  ]
            relativeSpiral = go instructions (Vec2 1 0)
              where
                go [] _dir = []
                go ((len, rotate) : rest) dir = 10*len *. dir : go rest (rotate dir)
            turnLeft  (Vec2 x y) = Vec2   y  (-x)
            turnRight (Vec2 x y) = Vec2 (-y)   x
        scissors = centerLine (angledLine (Vec2 (-5) (-5)) (deg 140) (Distance 150))
        cutResult = cutPolygon scissors polygon

    renderAllFormats 240 110 "test/out/cut/3_complicated" $ do
        polyCutDraw
            (move (Vec2 50 60) polygon)
            (move (Vec2 180 60) scissors)
            (move (Vec2 180 60) cutResult)

        mmaColor 1 1
        setFontSize 12
        moveTo 140 15
        showText (show (length cutResult) ++ " polygons")
    assertEqual "Number of resulting polygons" 5 (length cutResult)

cutMissesPolygonTest :: IO ()
cutMissesPolygonTest = do
    let scissors = Line (Vec2 0 70) (Vec2 50 60)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    renderAllFormats 130 90 "test/out/cut/4_miss"
        (polyCutDraw
            (move (Vec2 10 10) polygon)
            (move (Vec2 70 10) scissors)
            (move (Vec2 70 10) cutResult))

    assertEqual "Number of resulting polygons" 1 (length cutResult)

cutThroughCornerTest :: IO ()
cutThroughCornerTest = do
    let scissors = Line (Vec2 (-15) (-15)) (Vec2 65 65)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    renderAllFormats 150 90 "test/out/cut/5_through_corner"
        (polyCutDraw
            (move (Vec2 10 20) polygon)
            (move (Vec2 80 20) scissors)
            (move (Vec2 80 20) cutResult))

    assertEqual "Number of resulting polygons" 2 (length cutResult)

-- Taken from https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/
cornerCasesTest :: IO ()
cornerCasesTest = renderAllFormats 380 660 "test/out/cut/6_corner_cases"
    (sequence_ (intersperse (translate 0 100) [
        ooo,
        lol,
        ror,
        ool,
        oor,
        loo,
        roo
        ]))
  where
    scissors = Line (Vec2 (-60) 0) (Vec2 180 0)
    specialCase polygon description = withSavedState $ do
        translate 0 10
        let cutResult = cutPolygon scissors polygon
            placeOriginal, placeCut :: Move a => a -> a
            placeOriginal = move (Vec2 70 0)
            placeCut = move (Vec2 190 0)
        grouped paint $ do
            polyCutDraw
                (placeOriginal polygon)
                (placeOriginal scissors)
                (placeCut cutResult)
            for_ cutResult $ \poly ->
                for_ (let Polygon corners = poly in corners) $ \corner -> do
                    mmaColor 0 1
                    circleSketch (placeOriginal corner) (Distance 2.5)
                    stroke
                    circleSketch (placeCut corner) (Distance 2.5)
                    stroke
        let renderDescription = do
                mmaColor 1 1
                let Vec2 x y = placeCut (Vec2 0 0) in moveTo x y >> relMoveTo 70 0
                setFontSize 12
                extents <- textExtents description
                relMoveTo 0 (textExtentsHeight extents / 2)
                showText description
        renderDescription

    ooo = let colinearPoints = [Vec2 (-40) 0, Vec2 0 0, Vec2 40 0]
          in specialCase (Polygon (Vec2 0 40 : colinearPoints))
                         "on -> on -> on"
    lol = specialCase (Polygon [Vec2 0 0, Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40)])
                      "left → on → left"
    ror = specialCase (Polygon [Vec2 40 40, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 40, Vec2 0 0])
                      "right → on → right"
    ool = specialCase (Polygon [Vec2 0 0, Vec2 0 (-40), Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) 0])
                      "on → on → left"
    oor = specialCase (Polygon [Vec2 0 40, Vec2 40 40, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 0, Vec2 0 0])
                      "on → on → right"
    loo = specialCase (Polygon [Vec2 0 0, Vec2 40 0, Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40), Vec2 0 (-40)])
                      "left → on → on"
    roo = specialCase (Polygon [Vec2 40 0, Vec2 40 (-40), Vec2 (-40) (-40), Vec2 (-40) 40, Vec2 0 40, Vec2 0 0])
                      "right → on → on"

drawSimpleCutEdgeGraphTest :: TestTree
drawSimpleCutEdgeGraphTest = testCase "Draw simple cut edge graph" $
    renderAllFormats 120 220 "test/out/cut/7_edge_graph" $ do
        translate 10 110

        let CutEdgeGraph graph = transformAllVecs (100 *.) simpleCutEdgeGraph
            reconstructedPolygons = map (\(Polygon ps) -> Polygon (map (100 *.) ps))
                                        (sort (reconstructPolygons simpleCutEdgeGraph))
            moveRight (Distance d) line
              = move (d *. direction (perpendicularBisector line)) line
            nudge = moveRight (Distance 3.5) . resizeLineSymmetric (\(Distance d) -> Distance (0.85*d))
            arrowSpec = def{arrowheadSize = Distance 7, arrowheadRelPos = Distance 0.5}
        setLineWidth 1
        for_ (zip [1..] (M.toList graph)) $ \(i, (start, ends)) -> do
            mmaColor 0 1
            circleSketch start (Distance 3)
            strokePreserve
            mmaColor 0 0.3
            fill

            mmaColor i 1
            case ends of
                One end -> do
                    arrowSketch (nudge (Line start end)) arrowSpec
                    stroke
                Two end1 end2 -> do
                    arrowSketch (nudge (Line start end1)) arrowSpec
                    stroke
                    arrowSketch (nudge (Line start end2)) arrowSpec
                    stroke
        for_ (zip [1..] reconstructedPolygons) $ \(i, polygon) -> do
            withSavedState $ do
                polygonSketch polygon
                setDash [2,2] 0
                mmaColor i 1
                strokePreserve
                mmaColor i 0.1
                fill



transformAllVecs :: (Vec2 -> Vec2) -> CutEdgeGraph -> CutEdgeGraph
transformAllVecs f (CutEdgeGraph xs) = (CutEdgeGraph . M.fromList . map modify . M.toList) xs
  where
    modify (k, One v) = (f k, One (f v))
    modify (k, Two v v') = (f k, Two (f v) (f v'))




classifyCutTest :: TestTree
classifyCutTest
  = let scissors = Line (Vec2 0 0) (Vec2 1 0)
        test name spec expected
          = let actual = classifyCut scissors spec
            in testCase name (assertEqual "" expected actual)
    in (testGroup "Classify cuts" . map (\(name, expected, spec) -> test name spec expected))
        [ ("on    → on → on",    OOO, ((True,  Vec2 0   0),  (True,  Vec2 1   0)))
        , ("left  → on → left",  LOL, ((False, Vec2 0   1),  (False, Vec2 1   1)))
        , ("right → on → right", ROR, ((False, Vec2 0 (-1)), (False, Vec2 1 (-1))))
        , ("on    → on → left",  OOL, ((True,  Vec2 0   0),  (False, Vec2 1   1)))
        , ("on    → on → right", OOR, ((True,  Vec2 0   0),  (False, Vec2 1 (-1))))
        , ("left  → on → on",    LOO, ((False, Vec2 0   1),  (True,  Vec2 1   0)))
        , ("right → on → on",    ROO, ((False, Vec2 0 (-1)), (True,  Vec2 1   0)))
        ]

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
