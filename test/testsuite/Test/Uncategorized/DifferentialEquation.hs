module Test.Uncategorized.DifferentialEquation (tests) where



import           Control.Parallel.Strategies
import           Data.Foldable
import qualified Data.Vector                   as V
import           Draw
import           Geometry
import           Geometry.Chaotic
import           Geometry.Processes.Geodesics
import           Graphics.Rendering.Cairo      as Cairo hiding (x, y)
import           Numerics.DifferentialEquation
import           Numerics.Interpolation

import Test.TastyAll



tests :: TestTree
tests = testGroup "Differential equations"
    [ testGroup "Visual"
        [ twoBodyProblem
        , doublePendulum
        , noisePendulum
        , testGroup "Geodesics"
            [ geodesicsHillAndValley
            ]
        ]
    ]

twoBodyProblem :: TestTree
twoBodyProblem = testVisual "Two-body problem" 400 400 "docs/differential_equations/1_two_body_problem" renderTwoBodyProblem

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

renderTwoBodyProblem :: (Double, Double) -> Render ()
renderTwoBodyProblem (w, h) = do
    let phaseDiagram' = takeWhile (\(t,_) -> t < 1000) planetPhaseDiagram

        transformNicely
          = let trajectoryBoundingBox = boundingBox [x | (_t, (x,_v)) <- phaseDiagram']
                                     <> boundingBox (Vec2 0 0) -- Don’t forget about the sun :-)
                canvasBoundingBox = boundingBox (Vec2 10 10, Vec2 (w-10) (h-10))
                scaleToCanvas = transformBoundingBox trajectoryBoundingBox canvasBoundingBox def
            in Geometry.transform scaleToCanvas
        planetTrajectory = [(t, transformNicely x) | (t, (x,_v)) <- phaseDiagram']
        sun = transformNicely (Vec2 0 0)

    let paintSun = cairoScope $ do
            newPath
            sketch (Circle sun 16)
            setLineWidth 2
            setColor $ mathematica97 1
            fillPreserve
            setSourceRGB 0 0 0
            stroke

        paintTrajectory = cairoScope $ do
            setLineWidth 1.5
            for_ planetTrajectory $ \(_t, Vec2 x y) -> lineTo x y
            stroke

        paintPlanet = cairoScope $ do
            let (_t0, planet) = head planetTrajectory
            sketch (Circle planet 8)
            setColor $ mathematica97 3
            fillPreserve
            setSourceRGB 0 0 0
            setLineWidth 2
            stroke

    paintSun
    paintTrajectory
    paintPlanet

doublePendulum :: TestTree
doublePendulum = testVisual "Double pendulum" 400 400 "docs/differential_equations/2_double_pendulum" renderDoublePendulum

data DoublePendulum = DoublePendulum {
      _m1 :: Double
    , _m2 :: Double
    , _l1 :: Double
    , _l2 :: Double
    , _g :: Double
    , _y0 :: ((Double, Double), (Double, Double))
}

solveDoublePendulum :: DoublePendulum -> [(Double, ((Double, Double), (Double, Double)))]
solveDoublePendulum DoublePendulum{..} = rungeKuttaAdaptiveStep f _y0 t0 dt tolNorm tol
  where
    f :: Double -> ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double))
    f _t ((theta1, theta2), (omega1, omega2))
      = (
            (
                omega1
            ,
                omega2
            ),(
                (- _g * (2*_m1 + _m2) * sin theta1 - _m2 * _g * sin (theta1 - 2*theta2) - 2 * sin (theta1 - theta2) * _m2 * (omega2**2 * _l2 + omega1**2 * _l1 * cos (theta1 - theta2)))
                / --------------------------------------------------------------------------------------------------------------------------------------------------------------
                (_l1 * (2*_m1 + _m2 + _m2 * cos(2*theta1 - 2*theta2)))
            ,
                (2 * sin(theta1 - theta2) * (omega1**2 * _l1 * (_m1 + _m2) + _g * (_m1 + _m2) * cos theta1 + omega2**2 * _l2 * _m2 * cos (theta1 - theta2)))
                / ----------------------------------------------------------------------------------------------------------------------------------
                (_l1 * (2*_m1 + _m2 + _m2 * cos(2*theta1 - 2*theta2)))
            )
        )

    t0 = 0


    dt = 0.001
    tol = 0.001

    tolNorm ((theta1, theta2), (omega1, omega2)) = maximum (map abs [theta1, theta2, omega1, omega2])

system :: DoublePendulum
system = DoublePendulum {
      _m1 = 1
    , _m2 = 1
    , _l1 = 100
    , _l2 = 100
    , _g = 9.81
    , _y0 = ((120 * 2*pi/360, 120 * 2*pi/360), (0, 0))
}

renderDoublePendulum :: (Double, Double) -> Render ()
renderDoublePendulum (w,h) = do
    let trajectory = takeWhile (\(t, _x) -> t < 2000) (doublePendulumTrajectory system)
        scaleToCanvas = Geometry.transform (Geometry.translate (Vec2 (w/2) (h/2)))
        transformedTrajectory = V.fromList [(t, scaleToCanvas x) | (t,x) <- trajectory]
        bezierSmoothTrajectory = bezierSmoothen (fmap snd transformedTrajectory)

    cairoScope $ do
        let (_t0, start) = V.head transformedTrajectory
        moveToVec start
        setLineWidth 1
        for_ (V.zip transformedTrajectory bezierSmoothTrajectory) $ \((t, _), bezier) -> do
            setColor $ rocket (1-exp (-t / 500)) `withOpacity` exp (-t / 500)
            sketch [bezier]
            stroke

doublePendulumTrajectory :: DoublePendulum -> [(Double, Vec2)]
doublePendulumTrajectory sys =
    let solution = solveDoublePendulum sys

        -- Position of the lower pendulum
        v2 (t, ((theta1, theta2), _omegas))
          = let DoublePendulum{..} = sys
                x1 = _l1 * sin theta1
                y1 = _l1 * cos theta1
                x2 = x1 + _l2 * sin theta2
                y2 = y1 + _l2 * cos theta2
            in (t, Vec2 x2 y2)
    in map v2 solution


noisePendulum :: TestTree
noisePendulum = testVisual "Phase space of dampened noise pendulum" 260 200 "docs/differential_equations/3_noise_pendulum" (renderPhaseSpace solveNoisePendulum)

solveNoisePendulum :: [(Double, (Double, Double))]
solveNoisePendulum = rungeKuttaConstantStep ode y0 t0 dt
  where
    ode t (phi,omega)
      = let phi' = omega
            omega' = gravity + friction + noise + driver
            gravity = - sin phi
            friction = - 0.1*omega
            noise = 0.5*noiseF t phi omega
            driver = 0.5 * sin t
        in (phi', omega')
    y0 = (1, 1)
    t0 = 0
    dt = 0.01

    noiseF :: Double -> Double -> Double -> Double
    noiseF t phi omega
      = let noise:_ = normals (t, phi, omega)
        in noise

renderPhaseSpace :: [(Double, (Double, Double))] -> (Double, Double) -> Render ()
renderPhaseSpace solutionInfinite (w, h) = do
    let tMin = 100
        tMax = 150
        solution = V.fromList . takeWhile (\(t, _) -> t < tMax) . dropWhile (\(t, _) -> t < tMin) $ solutionInfinite
        bb = boundingBox (fmap (\(_t, (x,v)) -> Vec2 x v) solution)
        bbCanvas = boundingBox (Vec2 10 10, Vec2 (w-10) (h-10))
        scaleToCanvas :: Transform geo => geo -> geo
        scaleToCanvas = Geometry.transform (transformBoundingBox bb bbCanvas def)
        trajectory = scaleToCanvas
            . simplifyTrajectoryRdpBy 0.01 (\(_t, phaseSpacePoint) -> phaseSpacePoint) -- SVG compression :-)
            . fmap (\(t, (x, v)) -> (NoTransform t, Vec2 x v))
            $ solution

    setLineWidth 1
    cairoScope $
        for_ (V.zipWith (\(NoTransform t, p) (_t', p') -> (t, Line p p')) trajectory (V.tail trajectory)) $ \(t, line) -> do
            sketch line
            let val = lerp (tMin, tMax) (0,1) t
            setColor (icefire val `withOpacity` exp (-val))
            stroke

geodesicsHillAndValley :: TestTree
geodesicsHillAndValley = testVisual "Family of geodesics though hill and valley" 360 360 "docs/differential_equations/geodesic_hill_and_valley" $ \(w,h) -> do
    let -- Cauchy distribution, normalized to 1 at v0.
        cauchyHill g v0 v = 1 / (1+(normSquare (v-.v0)/g^2))
        hills =
            [ ( 100, Vec2 (2/3*w) (1/3*h))
            , (-33, Vec2 (1/3*w) (2/3*h))]
        terrain v = sum [height * cauchyHill 49 center v | (height, center) <- hills]
        ode = geodesicEquation (\_t -> terrain)
        startingAngles = map (deg . fromIntegral) [20,21..70]
        geodesics =
            [ (angle, rungeKuttaAdaptiveStep ode (x0, v0) t0 dt0 tolNorm tol)
            | angle <- startingAngles
            , let v0 = polar angle 1
            ]

        x0 = zero
        t0 = 0
        dt0 = 1
        tolNorm (x,v) = max (norm x) (norm v)
        tol = 1e-3

    let entireCanvas = (Vec2 0 0, Vec2 w h)

    let isos =
            let thresholds = [-30, -29 .. 30]
                grid = Grid entireCanvas (round (w/10), round (h/10))
                computeIso = isoLines grid terrain
            in [(threshold, computeIso threshold) | threshold <- thresholds]

    setLineWidth 1
    cairoScope $
        for_ hills $ \(height, center) -> do
            setColor (icefire (if height > 0 then 0.75 else 0.25))
            sketch (Circle center 2)
            fill

    cairoScope $
        for_ isos $ \(threshold, isosAtThreshold) ->
            for_ isosAtThreshold $ \singleIso -> do
                sketch (Polyline singleIso)
                let colorValue= lerp (-30, 30) (0,1) threshold
                setColor (icefire colorValue `withOpacity` 0.07)
                stroke

    cairoScope $ do
        let trajectories = withStrategy (parList rdeepseq) $ do
                (startingAngle, trajectory) <- geodesics
                let cutoff = takeWhile (\(_, (x, _v)) -> insideBoundingBox x entireCanvas)
                    justPosition = map (\(_t, (x, _v)) -> x)
                    simplify = simplifyTrajectoryRdp 0.5
                pure (startingAngle, (simplify . V.fromList . justPosition . cutoff) trajectory)
            angles = (getDeg (head startingAngles), getDeg (last startingAngles))
        for_ trajectories $ \(startingAngle, trajectory) -> do
            sketch (Polyline trajectory)
            let colorValue = lerp angles (1,0) (getDeg startingAngle)
            setColor (icefire colorValue)
            stroke
