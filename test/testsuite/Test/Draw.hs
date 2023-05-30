module Test.Draw (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C

import Draw
import Geometry               as G
import Numerics.Interpolation

import Test.TastyAll



tests :: TestTree
tests = testGroup "Draw"
    [ testProperty "fromCairoMatrix/toCairoMatrix are inverses" $ \trafo ->
        (fromCairoMatrix . toCairoMatrix) trafo ~=== trafo
    , testGroup "Ellipses"
        [ scaleEllipseTest
        ]
    ]

scaleEllipseTest :: TestTree
scaleEllipseTest = testVisual "Scale" 300 300 "docs/geometry/ellipses" $ \(w,h) -> do
    let center = zero
        radius = w/6*0.9
        ellipse = toEllipse (Circle center radius)

        grid i j = C.translate (fromIntegral i*w/3 + w/6) (fromIntegral j*h/3 + w/6)

    setLineWidth 1

    let actions =
            [ for_ (take 10 [0..]) $ \i -> cairoScope $ do
                let scaleFactor = lerpID (0,9) (0.1, 1) i
                grid 0 0
                sketch (G.transform (G.scaleAround center scaleFactor) ellipse)
                stroke

            , do
                grid 1 0
                for_ (take 10 [0..]) $ \i -> cairoScope $ do
                    let scaleFactor = lerpID (0,9) (0.1, 1) i
                    sketch (G.transform (G.scaleAround' center scaleFactor 1) ellipse)
                    stroke

            , do
                grid 2 0
                for_ [0..9] $ \i -> cairoScope $ do
                    let scaleFactor1 = lerp (0, 9) (1, 0.1) (fromIntegral i)
                        scaleFactor2 = lerp (0, 9) (0.1, 1) (fromIntegral i)
                    sketch (G.transform (G.scaleAround' center scaleFactor1 scaleFactor2) ellipse)
                    stroke

            , do
                grid 0 1
                for_ (take 10 [0..]) $ \i -> do
                    let scaleX = scaleAround' center (lerpID (0,9) (0.5,1) (fromIntegral i)) 1
                        scaleY = scaleAround' center 1 (lerpID (0,9) (0.1,1) (fromIntegral i))
                    sketch (G.transform (scaleX <> scaleY) ellipse)
                    stroke

            , do
                grid 1 1
                for_ (take 10 [0..]) $ \i -> cairoScope $ do
                    let angle = deg (lerpID (0,9) (0, 90) i)
                    sketch (G.transform (G.rotateAround center angle <> scaleAround' center 1 0.5) ellipse)
                    stroke

            , do
                grid 2 1
                for_ (take 19 [0..]) $ \i -> cairoScope $ do
                    let angle = deg (lerpID (0,19) (0, 180) i)
                    sketch (G.transform (G.rotateAround center angle <> scaleAround' center 1 0.5) ellipse)
                    stroke

            , do
                grid 0 2
                for_ (take 9 [0..]) $ \i -> cairoScope $ do
                    let scaleFactor = lerpID (0,9) (1,0.1) i
                        angle = deg (lerpID (0,9) (90,0) i)
                    sketch (G.transform (G.rotateAround center angle <> G.scaleAround' center 1 scaleFactor) ellipse)
                    stroke

            , do
                grid 1 2
                for_ (take 9 [0..]) $ \i -> do
                    let scaleFactor = lerpID (0,9) (1,0.1) i
                        angle = deg (lerpID (0,9) (0,90) i)
                    sketch (G.transform (scaleAround center scaleFactor <> G.rotateAround center angle <> scaleAround' center 1 scaleFactor) ellipse)
                    stroke

            , do
                grid 2 2
                for_ (take 9 [0..]) $ \i -> do
                    let BoundingBox topLeft _ = boundingBox ellipse
                        scaleFactor = lerpID (0,9) (1,0.1) i
                        angle = deg (lerpID (0,9) (0,90) i)
                    sketch (G.transform (rotateAround center angle <> scaleAround (0.5 *. (topLeft -. center)) scaleFactor) ellipse)
                    stroke
            ]

    for_ (zip [0..] actions) $ \(i, action) -> do
        setColor (mathematica97 i)
        cairoScope action
