-- | Visualize the flow of vector fields
--
-- <<docs/haddock/Geometry/Processes/FlowField/module_header.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Processes/FlowField/module_header.svg" 500 400 $ \wh@(Vec2 w h) -> do
--     gen <- C.liftIO $ MWC.withRng [] pure
--     noiseSeed <- C.liftIO $ MWC.uniform gen
--     let startPoints = [Vec2 0 y | y <- [75, 75+10 .. h-75]]
--     let
--         -- 2D vector potential, which in 2D is umm well a scalar potential.
--         vectorPotential :: Vec2 -> Double
--         vectorPotential p = perlin2 params p
--           where
--             params = PerlinParameters
--                 { _perlinFrequency   = 5 / min w h
--                 , _perlinLacunarity  = 2
--                 , _perlinOctaves     = 2
--                 , _perlinPersistence = 0.5
--                 , _perlinSeed        = noiseSeed
--                 }
--         --
--         rotationField :: Vec2 -> Vec2
--         rotationField = curlZ vectorPotential
--         --
--         velocityField :: Vec2 -> Vec2
--         velocityField p@(Vec2 x y) = Vec2 1 0 +. perturbationStrength *. rotationField p
--           where
--             perturbationStrength =
--                 w
--                 * logisticRamp (0.7*w) (w/6) x
--                 * gaussianFalloff (0.5*h) (0.1*h) y
--         --
--         drawBB = shrinkBoundingBox 10 [zero, wh]
--         --
--         fieldLineTrajectory :: (Vec2 -> Vec2) -> Vec2 -> [(Double, Vec2)]
--         fieldLineTrajectory f p0 = rungeKuttaAdaptiveStep (fieldLine f) p0 t0 dt0 tolNorm tol
--           where
--             t0 = 0
--             dt0 = 1
--             -- Decrease exponent for more accurate results
--             tol = 1e-3
--             tolNorm = norm
--         --
--         trajectoryStartingFrom :: Vec2 -> Polyline
--         trajectoryStartingFrom start =
--             Polyline
--             . map (\(_t, pos) -> pos)
--             . takeWhile
--                 (\(_, Vec2 x _) -> let BoundingBox _ (Vec2 xMax _) = drawBB in x < xMax)
--             $ fieldLineTrajectory velocityField start
--         --
--         limitXInversions n (Polyline points) =
--             let foo1 = zipWith (\start end -> let Vec2 dx _ = vectorOf (Line start end) in (start, dx < 0)) points (tail points)
--                 foo2 = groupBy (\(_, dir1) (_, dir2) -> dir1 == dir2) foo1
--                 foo3 = map (map fst) foo2
--                 foo4 = take n foo3
--                 foo5 = concat foo4
--             in Polyline foo5
--     --
--     for_ startPoints $ \startingPoint -> cairoScope $ do
--         do
--             colorI <- C.liftIO (MWC.uniformRM (0,0.95) gen)
--             setColor (viridis colorI)
--         let simplify (Polyline p) = Polyline . toList . simplifyTrajectoryVW 2 $ p
--             paintme = limitXInversions 10 $ trajectoryStartingFrom startingPoint
--         sketch (simplify paintme)
--         C.stroke
-- :}
-- Generated file: size 50KB, crc32: 0xe4ba9565
module Geometry.Processes.FlowField (
      fieldLine
    , flowLine
) where



import Geometry.Core



-- $setup
-- >>> import           Data.List
-- >>> import           Draw
-- >>> import           Geometry
-- >>> import           Geometry.Algorithms.PerlinNoise
-- >>> import qualified Graphics.Rendering.Cairo        as C
-- >>> import           Numerics.DifferentialEquation
-- >>> import           Numerics.Functions
-- >>> import           Numerics.VectorAnalysis
-- >>> import qualified System.Random.MWC.Extended      as MWC



-- | Calculate a single field line of a vector field, i.e. a particle following its
-- arrows.
--
-- This can for example be used to create the typical magnetic field plots.
fieldLine
    :: (Vec2 -> Vec2) -- ^ Vector field
    -> time -- ^ (Unused) time parameter, kept so the result fits in the ODE solvers.
            --   Use 'flowLine' for time-dependent fields.
    -> Vec2 -- ^ Start of the flow line
    -> Vec2
fieldLine f = \_ x -> f x

-- | Calculate a single flow line of a vector field, i.e. the trajectory of a
-- particle following its arrows, by putting this into an ODE
-- solver such as 'Numerics.DifferentialEquation.rungeKuttaAdaptiveStep'.
--
-- This is a more general version of 'fieldLine', which also works for time-dependent fields.
flowLine
    :: (Double -> Vec2 -> Vec2) -- ^ Vector field
    -> Double -- ^ Time
    -> Vec2   -- ^ Start of the flow line
    -> Vec2
flowLine = id -- lol
