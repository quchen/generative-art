module Test.Uncategorized.Reflection (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry

import Test.TastyAll



tests :: TestTree
tests = testVisual "Reflection of rays on a mirror" 520 300 "docs/geometry/reflection" $ \_ -> do

    let mirrorSurface = angledLine (Vec2 10 100) (deg 10) 510

    setLineWidth 1

    cairoScope $ do
        setLineWidth 2
        setColor (black `withOpacity` 0.5)
        sketch mirrorSurface
        stroke

    cairoScope $ do
        let rayOrigin = Vec2 180 250
        setColor (hsv 0 1 0.7)
        sketch (Circle rayOrigin 5)
        stroke
        for_ (zip [-135,-120.. -10] [0,6..]) (\(angleDeg, colorDeg) -> do
            let rayRaw = angledLine rayOrigin (deg angleDeg) 100
                Just (Line _ reflectedRayEnd, iPoint, _) = reflection rayRaw mirrorSurface
                ray = Line rayOrigin iPoint
                ray' = Line iPoint reflectedRayEnd
            setColor (hsva colorDeg 1 0.7 0.7)
            sketch ray
            sketch ray'
            stroke )
    cairoScope $ do
        let rayOrigin = Vec2 350 30
        setColor (hsva 180 1 0.7 1)
        sketch (Circle rayOrigin 5)
        stroke
        for_ (zip [-135,-120.. -10] [180,180+6..]) (\(angleDeg, colorDeg) -> do
            let rayRaw = angledLine rayOrigin (deg angleDeg) 100
                Just (Line _ reflectedRayEnd, iPoint, _) = reflection rayRaw mirrorSurface
                ray = Line rayOrigin iPoint
                ray' = Line iPoint reflectedRayEnd
            setColor (hsva colorDeg 1 0.7 0.7)
            sketch ray
            sketch ray'
            stroke )
