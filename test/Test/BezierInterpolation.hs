module Test.BezierInterpolation (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (x,y)

import Draw
import Geometry

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Bezier interpolation"
    [ testGroup "Visual"
        [ somePoints
        , picassoSquirrel
        , simplifyPathTest
        ]
    ]

somePoints :: TestTree
somePoints = testCase "Open curve" (renderAllFormats 400 300 "test/out/bezier_interpolation/1_bezier_open" somePointsRender)

somePointsRender :: Render ()
somePointsRender = paintBezierOpenPicture points smoothed
  where
    points =
        Geometry.translate (Vec2 50 25) $ Geometry.scale 0.5 [ Vec2 100 500
        , Vec2 200 200
        , Vec2 500 500
        , Vec2 600 100
        , Vec2 400 50
        , Vec2 50 100
        , Vec2 50 50
        , Vec2 250 100
        , Vec2 200 500
        ]
    smoothed = bezierSmoothenOpen points

paintBezierOpenPicture :: [Vec2] -> [Bezier Vec2] -> Render ()
paintBezierOpenPicture points smoothed = do
    setLineWidth 1

    let circle r = restoreStateAfter $ do
            (x,y) <- getCurrentPoint
            newPath
            circleSketch (Vec2 x y) (Distance r)
            closePath
        prettyBezier (Bezier (Vec2 x0 y0) (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) ) = do
            do -- Paint actual curve
                setSourceRGB 0 0 0
                moveTo x0 y0
                curveTo x1 y1 x2 y2 x3 y3
                stroke
            do -- paint bezier helper
                do -- forward
                    setSourceRGB 0.368417 0.506779 0.709798
                    moveTo x1 y1 >> circle 4 >> fill
                    moveTo x0 y0 >> lineTo x1 y1 >> stroke
                do -- backward
                    setSourceRGB 0.880722 0.611041 0.142051
                    moveTo x2 y2 >> circle 4 >> fill
                    moveTo x3 y3 >> lineTo x2 y2 >> stroke

        prettyPoint (Vec2 x y) = do
            save
            moveTo x y
            circle 5
            setSourceRGBA 0.922526 0.385626 0.209179 0.8
            fillPreserve
            setSourceRGB 0 0 0
            stroke
            restore

    for_ smoothed prettyBezier
    for_ points prettyPoint

picassoSquirrel :: TestTree
picassoSquirrel = testCase "Picasso squirrel" (renderAllFormats 320 270 "test/out/bezier_interpolation/2_picasso_squirrel" picassoSquirrelRender)

picassoSquirrelRender :: Render ()
picassoSquirrelRender = do
    let beziers = [bezierFace, bezierEar, bezierBack, bezierTail1, bezierTail2]
    restoreStateAfter $ do -- Paint squirrel outline
        moveToVec (head face)
        setSourceRGB 0 0 0
        for_ beziers $ \bezier -> for_ bezier $ \(Bezier _ (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3)) -> do
            curveTo x1 y1 x2 y2 x3 y3
        let Line _ end = lineFoot
        lineToVec end
        setLineWidth 3
        stroke
    restoreStateAfter $ do -- Paint visualization points
        setLineWidth 1.5
        for_ beziers $ \bezier -> for_ bezier $ \(Bezier start c1 c2 end) -> do
            restoreStateAfter . grouped (paintWithAlpha 0.7) $ do
                    mmaColor 3 1
                    circleSketch c1 (Distance 2) >> fill
                    moveToVec start >> lineToVec c1 >> stroke
            restoreStateAfter . grouped (paintWithAlpha 0.7) $ do
                    mmaColor 1 1
                    circleSketch c2 (Distance 2) >> fill
                    moveToVec c2 >> lineToVec end >> stroke
    restoreStateAfter $ do
        setSourceRGB 0 0 0
        for_ (concat [face, ear, back, tail1, tail2, foot]) $ \p -> do
            circleSketch p (Distance 2.5) >> fill



  where
    bezierFace = bezierSmoothenOpen face
    bezierEar = bezierSmoothenOpen (last face : ear)
    bezierBack = bezierSmoothenOpen (last ear : back)
    bezierTail1 = bezierSmoothenOpen (last back : tail1)
    bezierTail2 = bezierSmoothenOpen (last tail1 : tail2)
    lineFoot = Line (last tail2) (head foot) -- Foot is just a line

    moveToCanvas = Geometry.translate (Vec2 (-30) 320) . mirrorY . Geometry.scale 1.5

    face = moveToCanvas
        [ Vec2 50.77511154733325  152.3192840188383
        , Vec2 33.646158254256974 148.34291986151698
        , Vec2 31.810913258570228 150.17816485720374
        , Vec2 34.56378075210034  163.33075399295876
        , Vec2 47.71636988785535  179.23621062224385
        ]
    ear = moveToCanvas
        [ Vec2 53.22210487491557  200.953276404537
        , Vec2 75.5509189891043   206.15313722564943
        , Vec2 82.89189897185128  187.18893893688642
        , Vec2 70.35105816799185  176.17746896276594
        ]
    back = moveToCanvas
        [ Vec2 122.65554054506407 128.76697324085836
        , Vec2 143.76085799546163 57.80416674097094
        ]
    tail1 = moveToCanvas
        [ Vec2 184.13624790057    83.49759668058542
        , Vec2 198.20645953416837 129.99046990464961
        , Vec2 162.41918211827686 170.36585980975798
        , Vec2 146.51372548899175 145.89592653393476
        , Vec2 160.5839371225901  131.51984073438854
        ]
    tail2 = moveToCanvas
        [ Vec2 148.0430963187307  124.17886075164157
        , Vec2 134.5846330170279  132.1315890662841
        , Vec2 132.13763968944556 157.21327067400296
        , Vec2 145.59610299114834 180.45970728603504
        , Vec2 175.2658970880841  190.85942892825994
        , Vec2 201.26520119364633 182.90670061361737
        , Vec2 226.65275696731285 135.49620489170982
        , Vec2 214.72366449534903 79.82710668921186
        , Vec2 184.44212206651778 54.74542508149307
        , Vec2 143.45498382951382 40.67521344789472
        , Vec2 104.30309058819662 40.67521344789472
        , Vec2 76.77441565289546  43.122206775477025
        ]
    foot = moveToCanvas
        [ Vec2 94.20924311191953  67.99997227256398
        ]

simplifyPathTest :: TestTree
simplifyPathTest = testCase "Simplify path" (renderAllFormats 400 300 "test/out/bezier_interpolation/3_simplify_path" simplifyPathTestRender)

simplifyPathTestRender :: Render ()
simplifyPathTestRender = do
    let graph = [Vec2 x (sin x / (0.5 * x)) | x <- [0.1, 0.2 .. 16]]
        graphBB = boundingBox graph
        fitToBox :: Transform geo => geo -> geo
        fitToBox = Geometry.transform (transformBoundingBox graphBB (boundingBox (Vec2 10 10, Vec2 (400-10) (100-10))) IgnoreAspectRatio)

    let plotPath points = restoreStateAfter $ do
            setLineWidth 1
            newPath
            pathSketch points
            stroke
        plotPoints points = for_ points $ \p -> do
            circleSketch p (Distance 1)
            fillPreserve
            restoreStateAfter $ do
                setSourceRGB 0 0 0
                setLineWidth 0.5
                stroke
        plotBezier segments = restoreStateAfter $ do
            setLineWidth 1
            bezierCurveSketch segments
            stroke
        epsilons = [ Distance (2**(-e)) | e <- [10,9..1]]

    restoreStateAfter $ do
        mmaColor 0 1
        plotPath (fitToBox graph)
        plotPoints (fitToBox graph)

    restoreStateAfter $ do
        for_ epsilons $ \epsilon -> do
            Cairo.translate 0 20
            let simplified = simplifyPath epsilon graph
                interpolatedAgain = bezierSmoothenOpen simplified
            mmaColor 1 1
            plotBezier (fitToBox interpolatedAgain)
            plotPoints (fitToBox simplified)
