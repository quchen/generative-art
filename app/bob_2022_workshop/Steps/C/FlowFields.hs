module Steps.C.FlowFields (
      drawSimpleVectorField
    , drawFieldLines
    , drawFieldLinesWithRandomness
) where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo (liftIO)
import Math.Noise (Perlin(..), perlin, getValue)
import System.Random.MWC (create, uniformR)

import Draw
import Geometry
import Numerics.DifferentialEquation (rungeKuttaConstantStep)

seed :: Int
seed = 123

-- | Let's start with drawing a simple vector field
drawSimpleVectorField :: Int -> Int -> Cairo.Render ()
drawSimpleVectorField w h = do
    let scaleFactor = fromIntegral (max w h) / 1000
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ setColor white >> Cairo.paint

    Cairo.setLineWidth 1
    for_ [ Vec2 x y | x <- [0, 50 .. 1000], y <- [0, 50 .. 1000]] $ \point -> do
        let endPoint = point +. simpleVectorField point
        arrowSketch (Line point endPoint) def
        Cairo.stroke

-- | Simple random field. We get the randomness from coherent Perlin noise.
simpleVectorField :: Vec2 -> Vec2
simpleVectorField = 100 *. noise2d
  where
    noise = perlin { perlinFrequency = 1/200, perlinOctaves = 1, perlinSeed = seed }
    noise2d (Vec2 x y) = Vec2
        (fromMaybe 0 $ getValue noise (x, y, 0))
        (fromMaybe 0 $ getValue noise (x, y, 42))

-- | Now, let's draw some field lines in that vector field.
drawFieldLines :: Int -> Int -> Cairo.Render ()
drawFieldLines w h = do
    let scaleFactor = fromIntegral (max w h) / 1000
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ setColor white >> Cairo.paint

    Cairo.setLineWidth 2
    for_ [ Vec2 x y | x <- [0, 50 .. 1000], y <- [0, 50 .. 1000]] $ \point -> do
        let (p:ps) = [ x | (_time, x) <- fieldLine simpleVectorField point 5 ]
        moveToVec p
        for_ ps lineToVec
        Cairo.stroke

-- A field line is obtained by following the arrow a little bit, then looking
-- for the arrow in that point, following that, and so on. There is a standard
-- method called Runge-Kutta for this.
-- Runge-Kutta allows for time-dependent vector fields, but our vector field
-- is constant in time, so we just ignore the time.
fieldLine :: (Vec2 -> Vec2) -> Vec2 -> Double -> [(Double, Vec2)]
fieldLine = \vectorField startingPoint len -> timeLimit len $ followCurve vectorField startingPoint
  where
    -- Use Runge-Kutta to follow a curve through the vector field
    followCurve vectorField startingPoint = rungeKuttaConstantStep (fieldEquation vectorField) startingPoint startingTime step

    -- Our vector field is constant in time
    fieldEquation vectorField = \_time point -> vectorField point

    startingTime = 0

    -- How often should we look if we're still going in the right direction?
    -- Choosing a smaller step size takes more time, but choosing the step size
    -- too large makes the curves miss the arrows.
    step = 1

    -- Limiting the the length of the field line
    timeLimit len = takeWhile (\(time, _) -> time <= len)

-- | Let's add a bit of randomness: Randomly place the starting points,
-- add some random color, and change the line thickness over time.
--
-- You can play around with the parameters: Adding more or less points,
-- changing the thickness distribution, changing the line length, …
drawFieldLinesWithRandomness :: Int -> Int -> Cairo.Render ()
drawFieldLinesWithRandomness w h = do
    let scaleFactor = fromIntegral (max w h) / 1000
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ setColor white >> Cairo.paint

    gen <- liftIO create

    randomPoints <- liftIO $ replicateM 2000 $ do
        x <- uniformR (0, 1000) gen
        y <- uniformR (0, 1000) gen
        thickness <- uniformR (0.0, 3.0) gen
        color <- uniformR (0, 16) gen
        pure (Vec2 x y, thickness, color)

    for_ randomPoints $ \(point, thickness, color) -> do
        let ((_, p):ps) = fieldLine simpleVectorField point 5
        moveToVec p
        for_ ps $ \(time, p') -> do
            Cairo.setLineCap Cairo.LineCapRound
            cairoScope $ do
                lineToVec p'
                Cairo.setLineWidth (time * thickness)
                setColor (mathematica97 color)
                Cairo.stroke
            moveToVec p'
