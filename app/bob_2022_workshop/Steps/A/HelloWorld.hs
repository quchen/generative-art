module Steps.A.HelloWorld (hello) where

import Graphics.Rendering.Cairo hiding (x,y)

hello :: Int -> Int -> Render ()
hello _ _ = do
    translate 10 10
    letterH
    translate 40 0
    letterE
    translate 40 0
    letterL1
    translate 40 0
    letterL2
    translate 40 0
    letterO

    identityMatrix
    translate 10 100
    bobkonf2022

-- | Using absolute coordinates.
letterH :: Render ()
letterH = do
    newPath
    moveTo 0 0
    lineTo 10 0
    lineTo 10 20
    lineTo 20 20
    lineTo 20 0
    lineTo 30 0
    lineTo 30 50
    lineTo 20 50
    lineTo 20 30
    lineTo 10 30
    lineTo 10 50
    lineTo 0 50
    closePath
    stroke

-- | Using relative coordinates, also some styling
letterE :: Render ()
letterE = do
    newPath
    moveTo 0 0
    relLineTo 30 0
    relLineTo 0 10
    relLineTo (-20) 0
    relLineTo 0 10
    relLineTo 15 0
    relLineTo 0 10
    relLineTo (-15) 0
    relLineTo 0 10
    relLineTo 20 0
    relLineTo 0 10
    relLineTo (-30) 0
    closePath
    setSourceRGB 0.368417 0.506779 0.709798
    fillPreserve
    setSourceRGB 0 0 0
    strokePreserve

-- | Using more Haskell and more styles
letterL1 :: Render ()
letterL1 = do
    newPath
    moveTo 0 0
    sequence_ [ relLineTo dx dy | (dx,dy) <- [(10, 0), (0,40), (20,0), (0,10),(-30,0)] ]
    closePath
    setDash [3,5] 0
    setLineCap LineCapRound
    stroke

-- | Reuse the first L with different styles
letterL2 :: Render ()
letterL2 = do
    setLineWidth 3
    setSourceRGB 0 0 0
    letterL1

-- | Bezier curves and getting a bit fancier
letterO :: Render ()
letterO = do
    newPath
    setSourceRGB 0.880722 0.611041 0.142051
    setLineWidth 1
    setDash [] 0
    moveTo 0 20
    curveTo 0 0 40 0 40 20
    relLineTo 0 10
    curveTo 40 50 0 50 0 30
    closePath
    stroke
    prettyBezier 0 20 0 0 40 0 40 20
    prettyBezier 40 30 40 50 0 50 0 30
  where
    circle r = do
        (x,y) <- getCurrentPoint
        newPath
        arc x y r 0 (2*pi)
        closePath
    prettyBezier x1 y1 x2 y2 x3 y3 x4 y4 = do
        setSourceRGBA 0 0 0 0.2
        moveTo x1 y1 >> circle 2 >> fill
        moveTo x1 y1 >> lineTo x2 y2 >> stroke
        moveTo x2 y2 >> circle 2 >> fill
        moveTo x3 y3 >> circle 2 >> fill
        moveTo x3 y3 >> lineTo x4 y4 >> stroke
        moveTo x4 y4 >> circle 2 >> fill

-- | Text rendering
bobkonf2022 :: Render ()
bobkonf2022 = do
    setFontSize 40
    setSourceRGB 0.368417 0.506779 0.709798
    showText "BobKonf 2022"
