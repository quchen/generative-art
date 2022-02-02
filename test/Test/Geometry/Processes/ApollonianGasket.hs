module Test.Geometry.Processes.ApollonianGasket (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C

import Draw
import Geometry as G
import Geometry.Processes.ApollonianGasket

import Test.TastyAll



tests :: TestTree
tests = testGroup "Apollonian gasket"
    [ threeCircles
    , manyCircles
    ]

threeCircles :: TestTree
threeCircles = testCase "Manually defined" $ renderAllFormats 300 300 "out/manually_defined" $ do
    let startL = toApoCircle $ Circle (Vec2 100 100) 50
        startR = toApoCircle $ Circle (Vec2 200 100) 50
        startB = toApoCircle $ Circle (G.transform (rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 50

        large = newCircle (-) (-) startL startR startB
        middle = newCircle (+) (+) startL startR startB

        top = newCircle (+) (+) large startL startR
        bl = newCircle (+) (-) large startL startB
        br = newCircle (+) (+) large startR startB

        topL = newCircle (+) (+) top startL large
        topR = newCircle (+) (+) top startR large
        topB = newCircle (+) (+) top startR startL

        rightT = newCircle (+) (+) large startR br
        rightL = newCircle (+) (+) startB startR br
        rightB = newCircle (+) (+) large startB br
        leftT = newCircle (+) (+) large startL bl
        leftR = newCircle (+) (+) bl startB startL
        leftB = newCircle (+) (+) large bl startB

        apoCircleSketch c =
            let Circle center radius = toCircle c
            in circleSketch center (abs radius)

        apoCircles = []
            ++ [startL, startR, startB]
            ++ [large, middle]
            ++ [top, bl, br]
            ++ [topL, topR, topB]
            ++ [rightT, rightL, rightB]
            ++ [leftT, leftR, leftB]

    for_ (zip [0..] apoCircles) $ \(i, apo) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i)
        apoCircleSketch apo
        stroke

manyCircles :: TestTree
manyCircles = testCase "Automatically built" $ renderAllFormats 300 300 "out/automatically_built" $ do
    let startL = toApoCircle $ Circle (Vec2 100 100) 50
        startR = toApoCircle $ Circle (Vec2 200 100) 50
        startB = toApoCircle $ Circle (G.transform (rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 50

        large = newCircle (-) (-) startL startR startB

        top = newCircle (+) (+) large startL startR
        bl = newCircle (+) (-) large startL startB
        br = newCircle (+) (+) large startR startB

        recurse [] = []
        recurse ((c1, c2, c3) : rest) =
            let new@(ApoCircle _ k) = newCircle (+) (+) c1 c2 c3
            in if k > 10
                then recurse rest
                else new : recurse ((c1, c2, new) : (c1, c3, new) : (c2, c3, new) : rest)


        apoCircleSketch c =
            let Circle center radius = toCircle c
            in circleSketch center (abs radius)

        apoCircles = large : top : bl : br : recurse [(large, top, bl), (large, top, br), (top, bl, br), (large, bl, br)]

    for_ (zip [0..] apoCircles) $ \(i, apo) -> cairoScope $ do
        setLineWidth 1
        setColor (mathematica97 i)
        apoCircleSketch apo
        stroke
