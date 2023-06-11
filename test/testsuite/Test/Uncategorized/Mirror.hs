module Test.Uncategorized.Mirror (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform)

import Draw
import Geometry

import Test.TastyAll



tests :: TestTree
tests = testVisual "Mirror along an axis" 550 550 "docs/geometry/mirror" $ \_ -> do
    Cairo.translate 10 20
    mirror1
    Cairo.translate 0 230
    mirror2

originalC, mirroredC :: Double -> Render ()
originalC o = setColor (withOpacity (mma 0) o)
mirroredC o = setColor (withOpacity (mma 1) o)

setMirrorStyle :: Render ()
setMirrorStyle = setColor (black `withOpacity` 0.5) >> setDash [5,5] 0 >> setLineWidth 1

mirror1 :: Render ()
mirror1 = do
    testDraw
        (angledLine (Vec2 10 100) (deg 0) 100)
        [ Vec2 40 10
        , Vec2 20 20
        , Vec2 60 60
        , Vec2 80 160
        ]
    testDraw
        (angledLine (Vec2 150 100) (deg (-30)) 100)
        [ Vec2 160 10
        , Vec2 140 20
        , Vec2 180 60
        , Vec2 230 100
        ]
  where
    testDraw :: Line -> [Vec2] -> Render ()
    testDraw mirror ps = do
        setLineWidth 1

        cairoScope $ do
            setMirrorStyle
            sketch mirror
            stroke

        for_ ps (\p -> do
            let p' = transform (mirrorAlong mirror) p

            mirroredC 1
            sketch (Cross p 5)
            stroke
            originalC 1
            sketch (Circle p' 5)
            stroke

            setColor (black `withOpacity` 0.5)
            sketch (Arrow (Line p p') def)
            stroke )

mirror2 :: Render ()
mirror2 = do
    let mirror = angledLine (Vec2 10 100) (deg 10) 510

    cairoScope $ do
        setMirrorStyle
        sketch mirror
        stroke

    setFontSize 12
    originalC 1 >> moveTo 180 30 >> showText "Original"
    mirroredC 1 >> moveTo 180 45 >> showText "Mirrored"

    setLineWidth 1
    let mirrorLineTest line = do
            let mirrored = transform (mirrorAlong mirror) line
            originalC 1 >> sketch (Arrow line def) >> stroke
            mirroredC 1 >> sketch (Arrow mirrored def) >> stroke
    mirrorLineTest (angledLine (Vec2 50 10) (deg 20) 100)
    mirrorLineTest (angledLine (Vec2 150 10) (deg 90) 100)
    mirrorLineTest (angledLine (Vec2 160 10) (deg 90) 150)
    mirrorLineTest (angledLine (Vec2 300 10) (deg 120) 180)
    mirrorLineTest (angledLine (Vec2 250 160) (deg 0) 200)
    mirrorLineTest (angledLine (Vec2 120 110) (deg 180) 100)

    let mirrorPolygonTest poly = do
            let mirrored = transform (mirrorAlong mirror) poly
            originalC 1 >> sketch poly >> strokePreserve >> originalC 0.1 >> fill
            mirroredC 1 >> sketch mirrored >> strokePreserve >> mirroredC 0.1 >> fill
    mirrorPolygonTest (Polygon [Vec2 350 200, Vec2 400 220, Vec2 380 240, Vec2 420 220, Vec2 420 200])
    mirrorPolygonTest (Polygon [Vec2 339 56, Vec2 310 110, Vec2 370 82, Vec2 300 70, Vec2 348 118])
