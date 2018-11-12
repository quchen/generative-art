{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable
import Data.Semigroup
import Data.Time.Clock.POSIX
import Graphics.Rendering.Cairo
import System.Random

import Draw
import Geometry



main :: IO ()
main = do
    testIntersectionLL
    testMirror
    testReflection
    testReflection2
    -- testBillard

testBillard :: IO ()
testBillard = renderAllFormats 320 240 "test/out/billard" (do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]

    billard table (Vec2 100 100) (deg $ -40)

    )
  where
    billard table startPoint startAngle = do
        let startVec = angledLine startPoint startAngle (Distance 100)
            billard = startPoint : take 16 (billardProcess table startVec)

        setLineWidth 2
        hsva 0 0 0 0.5
        polygonSketch table
        stroke

        setLineWidth 1
        hsva 0 1 1 1
        liftIO (traverse print billard)
        for_ billard (\point -> do
            circleSketch point (Distance 3)
            fill )
        hsva 180 1 0.7 1
        let billardArrows = zipWith Line billard (tail billard)
        for_ billardArrows (\arr -> do
            arrowSketch arr
            stroke )

testReflection2 :: IO ()
testReflection2 = renderAllFormats 520 300 "test/out/mirror2" (do
    let mirror = angledLine (Vec2 10 100) (deg 10) (Distance 510)

    setLineWidth 2
    hsva 0 0 0 0.5
    lineSketch mirror
    stroke

    setLineWidth 1
    let mirrorLineTest line = do
            let mirrored = mirrorAlong mirror line
            hsva 120 1 0.7 1 >> arrowSketch line >> stroke
            hsva 0 1 0.7 1 >> arrowSketch mirrored >> stroke
    mirrorLineTest (angledLine (Vec2 50 10) (deg (20)) (Distance 100))
    mirrorLineTest (angledLine (Vec2 150 10) (deg 90) (Distance 100))
    mirrorLineTest (angledLine (Vec2 160 10) (deg 90) (Distance 150))
    mirrorLineTest (angledLine (Vec2 300 10) (deg 120) (Distance 180))
    mirrorLineTest (angledLine (Vec2 250 160) (deg 0) (Distance 200))
    mirrorLineTest (angledLine (Vec2 120 110) (deg 180) (Distance 100))

    let mirrorPolygonTest poly = do
            let mirrored = mirrorAlong mirror poly
            hsva 120 1 0.7 1 >> polygonSketch poly >> strokePreserve >> hsva 120 1 0.7 0.1 >> fill
            hsva 0 1 0.7 1 >> polygonSketch mirrored >> strokePreserve >> hsva 0 1 0.7 0.1 >> fill
    mirrorPolygonTest (Polygon [Vec2 350 200, Vec2 400 220, Vec2 380 240, Vec2 420 220, Vec2 420 200])
    mirrorPolygonTest (Polygon [Vec2 339 56, Vec2 310 110, Vec2 370 82, Vec2 300 70, Vec2 348 118])
    )

testReflection :: IO ()
testReflection = renderAllFormats 520 300 "test/out/reflection" (do

    let mirror = angledLine (Vec2 10 100) (deg 10) (Distance 510)

    setLineWidth 2
    hsva 0 0 0 0.5
    lineSketch mirror
    stroke

    do
        let rayOrigin = Vec2 180 250
        setLineWidth 1
        hsva 0 1 0.7 1
        circleSketch rayOrigin (Distance 5)
        stroke
        for_ (zip [-135,-120.. -10] [0,6..]) (\(angleDeg, colorDeg) -> do
            let rayRaw = angledLine rayOrigin (deg angleDeg) (Distance 100)
                (Line _ reflectedRayEnd, iPoint, _, _) = reflection rayRaw mirror
                ray = Line rayOrigin iPoint
                ray' = Line iPoint reflectedRayEnd
            hsva colorDeg 1 0.7 0.7
            lineSketch ray
            lineSketch ray'
            stroke )
    do
        let rayOrigin = Vec2 350 30
        setLineWidth 1
        hsva 180 1 0.7 1
        circleSketch rayOrigin (Distance 5)
        stroke
        for_ (zip [-135,-120.. -10] [180,180+6..]) (\(angleDeg, colorDeg) -> do
            let rayRaw = angledLine rayOrigin (deg angleDeg) (Distance 100)
                (Line _ reflectedRayEnd, iPoint, _, _) = reflection rayRaw mirror
                ray = Line rayOrigin iPoint
                ray' = Line iPoint reflectedRayEnd
            hsva colorDeg 1 0.7 0.7
            lineSketch ray
            lineSketch ray'
            stroke )

    )

testMirror :: IO ()
testMirror = renderAllFormats 250 200 "test/out/mirror" (do
    testMirror
        (angledLine (Vec2 10 100) (Angle 0) (Distance 100))
        [ Vec2 40 10
        , Vec2 20 20
        , Vec2 60 60
        , Vec2 80 160
        ]
    testMirror
        (angledLine (Vec2 150 100) (deg (-30)) (Distance 100))
        [ Vec2 160 10
        , Vec2 140 20
        , Vec2 180 60
        , Vec2 230 100
        ])
  where

    testMirror mirror ps = do
        setLineWidth 2
        hsva 0 0 0 1
        lineSketch mirror
        stroke

        for_ ps (\p -> do
            let p' = mirrorAlong mirror p

            setLineWidth 1
            hsva 0 1 0.7 1
            crossSketch p (Distance 5)
            stroke
            hsva 120 1 0.7 1
            circleSketch p' (Distance 5)
            stroke

            hsva 120 0 0 0.5
            arrowSketch (Line p p')
            stroke )

testIntersectionLL :: IO ()
testIntersectionLL = renderAllFormats 580 480 "test/out/intersection" (do
    testVirtual1
    testVirtual2
    testVirtualInL
    testVirtualR
    testReal1
    testReal2 )
  where
    testVirtual1 = testDraw
        (angledLine (Vec2 50 190) (Angle ( pi/6)) (Distance 100))
        (angledLine (Vec2 50 300) (Angle (-pi/6)) (Distance 100))
    testVirtual2 = testDraw
        (move (Vec2 30 380) (moveRad (Angle (-pi/6)) (Distance 20) (angledLine (Vec2 0 0) (Angle (-pi/6)) (Distance 100))))
        (move (Vec2 30 380) (moveRad (Angle ( pi/6)) (Distance 20) (angledLine (Vec2 0 0) (Angle ( pi/6)) (Distance 100))))
    testVirtualInL = testDraw
        (angledLine (Vec2 300 180) (Angle      0) (Distance 100))
        (angledLine (Vec2 350 200) (Angle (pi/2)) (Distance  50))
    testVirtualR = testDraw
        (angledLine (Vec2 370 330) (Angle      0) (Distance  50))
        (angledLine (Vec2 350 280) (Angle (pi/2)) (Distance 100))
    testReal1 = testDraw
        (Line (Vec2 10  10) (Vec2 220 190))
        (Line (Vec2 270 50) (Vec2  30 160))
    testReal2 = testDraw
        (move (Vec2 320 10) (Line (Vec2 0   0) (Vec2 120 120)))
        (move (Vec2 320 10) (Line (Vec2 120 0) (Vec2 0   120)))

    testDraw line1 line2 = do
        let (point, angle, ty) = intersectionLL line1 line2

        setLineWidth 1
        hsva 0 1 0.7 1
        arrowSketch line1
        stroke
        hsva 60 1 0.7 1
        arrowSketch line2
        stroke

        hsva 120 1 0.7 1
        circleSketch point (Distance 3)
        fill

        hsva 180 1 0.7 1
        arcSketch point (Distance 10) (angleOfLine line1) (angleOfLine line2)
        stroke

        do let fontSize = 10
           let Vec2 x y = point `addVec2` Vec2 15 15
           hsva 0 0 0 1
           moveTo x y
           setFontSize fontSize
           let Angle alpha = angle
               angleDeg = round (alpha / (2 * pi) * 360) :: Int
           showText (show ty ++ ", " ++ show angleDeg ++ "°")

        closePath

arrowSketch :: Line -> Render ()
arrowSketch line = do
    lineSketch line
    let Line start end = line
        Angle rawAngle = angleOfLine line
    lineSketch (angledLine end (Angle (rawAngle + pi - 0.5)) (Distance 10))
    lineSketch (angledLine end (Angle (rawAngle + pi + 0.5)) (Distance 10))

renderPng :: Int -> Int -> FilePath -> Render () -> IO ()
renderPng picWidth picHeight filename drawing = do
    surface <- createImageSurface FormatARGB32 picWidth picHeight
    renderWith surface (do
        background picWidth picHeight
        drawing
        )
    surfaceWriteToPNG surface filename
  where
    background picWidth picHeight = do
        rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
        hsva 0 0 0 1
        setLineWidth 1
        stroke

renderSvg :: Int -> Int -> FilePath -> Render () -> IO ()
renderSvg picWidth picHeight filename drawing
  = withSVGSurface filename w h (\surface -> renderWith surface drawing)
  where
    w = fromIntegral picWidth
    h = fromIntegral picHeight

renderAllFormats :: Int -> Int -> FilePath -> Render () -> IO ()
renderAllFormats w h filename drawing = do
    renderPng w h (filename ++ ".png") drawing
    renderSvg w h (filename ++ ".svg") drawing


hsva h s v a = setSourceRGBA channelRed channelGreen channelBlue a
    where RGB{..} = hsv h s v
