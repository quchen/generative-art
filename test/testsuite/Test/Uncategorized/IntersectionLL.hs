module Test.Uncategorized.IntersectionLL (tests) where



import Graphics.Rendering.Cairo as Cairo hiding (transform, translate, x, y)
import Text.Printf

import Draw
import Geometry

import Test.TastyAll



tests :: TestTree
tests = testGroup "Line-line intersection"
    [ intersectionReal
    , intersectionHalfVirtual
    , intersectionVirtual
    , intersectionParallel
    , intersectionAlmostParallel
    ]

intersectionReal :: TestTree
intersectionReal = testVisual "Real intersection" 580 200 "docs/geometry/intersection/real" $ \_ -> do
    testReal1
    testReal2
  where
    testReal1 = testDraw
        (Line (Vec2 10  10) (Vec2 220 190))
        (Line (Vec2 270 50) (Vec2  30 160))
    testReal2 = testDraw
        (transform (translate (Vec2 320 10)) (Line (Vec2 0   0) (Vec2 120 120)))
        (transform (translate (Vec2 320 10)) (Line (Vec2 120 0) (Vec2 0   120)))

intersectionHalfVirtual :: TestTree
intersectionHalfVirtual = testVisual "Half-virtual intersection" 580 120 "docs/geometry/intersection/half_virtual" $ \_ -> do
    testVirtualInL
    testVirtualR
  where
    testVirtualInL = testDraw
        (angledLine (Vec2 10 10) (rad      0) 100)
        (angledLine (Vec2 60 30) (rad (pi/2)) 50)
    testVirtualR = testDraw
        (angledLine (Vec2 370 60) (rad      0) 50)
        (angledLine (Vec2 350 10) (rad (pi/2)) 100)

intersectionVirtual :: TestTree
intersectionVirtual = testVisual "Virtual intersection" 580 120 "docs/geometry/intersection/virtual" $ \_ -> do
    testVirtual1
    testVirtual2
  where
    testVirtual1 = testDraw
        (angledLine (Vec2 50 0) (rad ( pi/6)) 100)
        (angledLine (Vec2 50 120) (rad (-pi/6)) 100)
    testVirtual2 = testDraw
        (transform (translate (Vec2 300 60 +. polar (rad (-pi/6)) 20)) (angledLine (Vec2 0 0) (rad (-pi/6)) 100))
        (transform (translate (Vec2 300 60 +. polar (rad ( pi/6)) 20)) (angledLine (Vec2 0 0) (rad ( pi/6)) 100))

intersectionParallel :: TestTree
intersectionParallel = testVisual "Parallel" 580 120 "docs/geometry/intersection/parallel" $ \_ -> do
    testParallel1
    testParallel2
  where
    testParallel1 = testDraw
        (Line (Vec2 50 40) (Vec2 150 40))
        (Line (Vec2 50 80) (Vec2 150 80))
    testParallel2 = testDraw
        (Line (Vec2 300 60) (Vec2 400 60))
        (Line (Vec2 420 60) (Vec2 520 60))

intersectionAlmostParallel :: TestTree
intersectionAlmostParallel = testVisual "Parallel lines that fail to be recognized as parallel" 580 120 "docs/geometry/intersection/almost_parallel" $ \_ -> testDraw
    (angledLine (Vec2 50 0) (rad (pi/6)) 80)
    (angledLine (Vec2 50 20) (rad (pi/6)) 80)

testDraw :: Line -> Line -> Render ()
testDraw line1@(Line start _) line2 = do
    setLineWidth 1

    cairoScope $ do
        setColor $ mathematica97 0
        sketch (Arrow line1 def{_arrowheadSize = 8})
        stroke

    cairoScope $ do
        setColor $ mathematica97 1
        sketch (Arrow line2 def{_arrowheadSize = 8})
        stroke

    let ty = intersectionLL line1 line2

    case intersectionPoint ty of
        Nothing -> cairoScope $ do
            let Vec2 x y = start +. Vec2 15 15
            setColor black
            moveTo x y
            setFontSize 10
            showText $ case ty of
                Parallel    -> "Parallel"
                Collinear _ -> "Collinear"
                _           -> error "Unknown intersection type"

        Just point -> do

            cairoScope $ do
                setColor $ mathematica97 3
                sketch (Circle point 3)
                fill

            cairoScope $ do
                setColor $ mathematica97 3
                angleSketch point (angleOfLine line1) (angleOfLine line2)
                stroke

            cairoScope $ do
                let Vec2 x y = point +. Vec2 15 15
                    angleDeg = printf "%2.f" (getDeg (normalizeAngle (rad 0) (angleBetween line1 line2)))
                    tyStr = case ty of
                        IntersectionVirtual _        -> "Virtual"
                        IntersectionVirtualInsideL _ -> "Virtual (but inside left argument)"
                        IntersectionVirtualInsideR _ -> "Virtual (but inside right argument)"
                        IntersectionReal _           -> "Intersection"
                        _                            -> error "Unknown intersection type"

                setColor black
                moveTo x y
                setFontSize 10
                showText (tyStr ++ ", " ++ angleDeg ++ "°")

angleSketch :: Vec2 -> Angle -> Angle -> Render ()
angleSketch point angle1 angle2 = do
    let radius = 10
    arcSketch point radius angle1 angle2
    let arcEnd = transform (translate (polar angle2 radius)) point
        arrowAngleTweak = rad (-0.2)
        tangentStart = transform (translate (polar (angle2 -. rad (pi/2) +. arrowAngleTweak) radius)) arcEnd
        tangent = Line tangentStart arcEnd
    sketch (Arrow tangent def { _arrowheadSize = 6, _arrowDrawBody = False })
