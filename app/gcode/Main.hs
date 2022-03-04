{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import qualified Data.Vector                   as V
import           Draw
import           Draw.GCode
import           Formatting
import           Geometry                      as G
import           Numerics.DifferentialEquation



main :: IO ()
main = do
    let tMax = 1000

        polyline = takeWhile (\(t,_) -> t < tMax) planetPhaseDiagram

    for_ [5,10..50] $ \size -> do
        let transformNicely =
                let trajectoryBoundingBox = boundingBox [x | (_t, (x,_v)) <- polyline]
                                            <> boundingBox (Vec2 0 0) -- Don’t forget about the sun :-)
                    canvasBoundingBox = boundingBox [zero, Vec2 size size]
                    scaleToCanvas = transformBoundingBox trajectoryBoundingBox canvasBoundingBox def
                in G.transform scaleToCanvas
            planetTrajectory = [transformNicely x | (_t, (x,_v)) <- polyline]

            filename = T.unpack (format ("out/planets-" % int % "x" % int % ".gcode") (round size) (round size))

            gcode = V.fromList [ GComment (format ("Planet trajectory, rescaled to " % int % "x" % int) (round size) (round size)), GDeclareAbsoluteMovement ] <> toGCode planetTrajectory

        putStrLn ("Rendering " <> filename)
        T.writeFile filename (T.unlines (V.toList (V.map renderGCode gcode)))

planetPhaseDiagram :: [(Double, (Vec2, Vec2))]
planetPhaseDiagram = rungeKuttaAdaptiveStep f y0 t0 dt0 tolNorm tol
  where
    f :: Double -> (Vec2, Vec2) -> (Vec2, Vec2)
    f _t (x, v)
        -- Gravity is a bit weaker with r^1.94 instead of r^2 falloff to make the picture more interesting.
        -- Also some tiny friction for the same reason.
      = let gravity = (- attraction / let r2 = normSquare x in r2 ** (2.94/2)) *. x
            attraction = 2200
            friction = (-0.0001 * norm v) *. v
            a = gravity +. friction
        in (v, a)
    y0 = (Vec2 100 0, Vec2 4 4)
    t0 = 0

    dt0 = 10
    tol = 0.001

    tolNorm (x,v) = max (norm x) (norm v)
