module Steps.C.FlowFields (
      drawSimpleVectorField
    , drawFieldLines
    , drawFieldLinesWithRandomness
) where



import           Control.Monad
import           Data.Maybe
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import           Math.Noise               (Perlin (..), getValue, perlin)
import           System.Random.MWC

import Draw
import Geometry
import Numerics.DifferentialEquation
import Numerics.Interpolation



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
        cairoScope $ do
            setColor (mathematica97 0)
            sketch (Arrow (Line point endPoint) def)
            Cairo.stroke
        cairoScope $ do
            sketch (Circle point 4)
            setColor (mathematica97 3)
            Cairo.fill

-- | Simple random field. We get the randomness from coherent Perlin noise.
simpleVectorField :: Vec2 -> Vec2
simpleVectorField = 100 *. noise2d
  where
    noise = perlin { perlinFrequency = 1/200, perlinOctaves = 2, perlinSeed = seed }
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
        cairoScope $ do
            moveToVec p
            for_ ps lineToVec
            setColor (mathematica97 0)
            Cairo.stroke
        cairoScope $ do
            sketch (Circle p 5)
            setColor (mathematica97 3)
            Cairo.fill

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
    step = 0.1

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
    cairoScope (setColor white >> Cairo.paint)

    gen <- liftIO create

    for_ [1..2000] $ \_i -> do
        (startingPoint, thickness) <- liftIO $ do
            start <- uniformRM (zero, Vec2 1000 1000) gen
            thickness <- uniformRM (0.0, 3.0) gen
            pure (start, thickness)

        let trajectory@((_, p):ps) = fieldLine simpleVectorField startingPoint 5
        let len points = sum (zipWith (\(_t, x) (_t', y) -> lineLength (Line x y)) points (tail points))
        setColor (flare (lerp (50, 220) (0,1) (len trajectory)))
        moveToVec p
        for_ ps $ \(time, p') -> do
            Cairo.setLineCap Cairo.LineCapRound
            cairoScope $ do
                lineToVec p'
                Cairo.setLineWidth (time * thickness)
                Cairo.stroke
            moveToVec p'
