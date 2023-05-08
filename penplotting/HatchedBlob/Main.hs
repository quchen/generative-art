module Main (main) where



import Draw                             as D
import Draw.Plotting
import Geometry                         as G
import Numerics.VectorAnalysis



main :: IO ()
main = do
    let w = 210
        h = 148
        margin = 20
        paperBB = boundingBox [zero, Vec2 w h]
        drawInsideBB = boundingBox [zero +. Vec2 margin margin, Vec2 w h -. Vec2 margin margin]

    let plotSettings = def
            { _canvasBoundingBox = Just paperBB
            , _previewDrawnShapesBoundingBox = True
            -- , _previewPenTravelColor = Nothing
            }
        plotLines = runPlot plotSettings { _previewPenColor = mathematica97 3, _feedrate = 2000 } $ do
            for_ (geometry drawInsideBB) $ \points -> do
                plot (Polyline points)

    writeGCodeFile "out/hatched-blob.g" plotLines

    D.render "out/hatched-blob.png" (round w) (round h) $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp h)
        _plotPreview plotLines

geometry :: BoundingBox -> [[Vec2]]
geometry bb =
    let hatchLines = hatching bb
        subdividedLines = map (subdivideLineByLength 1) hatchLines
        sigma = 15
        strength = 200
        deformed = (map.map) (deform (boundingBoxCenter bb) sigma strength) subdividedLines
        simplified = map (toList . simplifyTrajectoryVW 1) deformed
    in simplified

hatching :: BoundingBox -> [Line]
hatching bb = hatch hatchPolygon angle spacing
  where
    hatchPolygon = boundingBoxPolygon bb
    BoundingBox b1 b2 = bb
    angle = angleOfLine (Line b1 b2)
    spacing = 1

deform :: Vec2 -> Double -> Double -> Vec2 -> Vec2
deform mu sigma strength vec = vec -. strength *. grad gauss vec
  where
    gauss v = exp (-1/2* normSquare((v-.mu)/.sigma))
    cauchy v = 1 / (1 + normSquare((v-.mu)/.sigma))
