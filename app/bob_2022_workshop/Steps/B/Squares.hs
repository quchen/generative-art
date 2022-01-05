module Steps.B.Squares (squares) where

import System.Random
import Graphics.Rendering.Cairo hiding (x,y)
import Control.Monad
import Data.Foldable
import Data.Function

-- Inspired by
-- https://www.kovach.me/Generating_artwork_with_Haskell.html
squares :: Int -> Int -> Render ()
squares w h = do
    for_ [15, 30 .. w-15] $ \x ->
        for_ [15, 30 .. h-15] $ \y ->
            drawSquare (fromIntegral x,fromIntegral y)

-- Box-Muller transform, see https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
-- Gives us two Gaussian random variables.
gaussianRandom :: Render (Double, Double)
gaussianRandom = liftIO $ do
    u1 <- fix $ \loop -> do -- Dirtily retry until we get a nonzero u1 :-)
        u1' <- randomRIO (0,1)
        if u1' <= 0 then loop else pure u1'
    u2 <- randomRIO (0,1)
    let logU1 = sqrt (- 2 * log u1)
        pi2u2 = 2*pi*u2
    pure (logU1 * sin pi2u2, logU1 * cos pi2u2)

-- Slightly move a point around with some standard deviation
wiggle :: (Double, Double) -> Double -> Render (Double, Double)
wiggle (x,y) sigma = do
    (dx, dy) <- gaussianRandom
    pure (x+sigma*dx, y+sigma*dy)

randomColor :: Render ()
randomColor = do
    n <- liftIO (randomRIO (0,2 :: Int))
    case n of
        0 -> setSourceRGB 0.368417 0.506779 0.709798
        1 -> setSourceRGB 0.880722 0.611041 0.142051
        2 -> setSourceRGB 0.922526 0.385626 0.209179
        _ -> error "tweak your random range"

drawSquare :: (Double, Double) -> Render ()
drawSquare (x,y) = do
    wiggleSquarePath

    paintIt <- randomRIO (0,100 :: Double)
    when (paintIt < 70) $ do
        fillIt <- randomIO
        randomColor
        if fillIt
            then fill
            else do
                thickness <- randomRIO (1,2)
                setLineWidth thickness
                stroke

  where
    wiggleSquarePath = do
        let sigma = 0.5
        newPath
        do  (x', y') <- wiggle (x,y) sigma
            moveTo x' y'
        do  (x', y') <- wiggle (x+10,y) sigma
            lineTo x' y'
        do  (x', y') <- wiggle (x+10,y+10) sigma
            lineTo x' y'
        do  (x', y') <- wiggle (x,y+10) sigma
            lineTo x' y'
        closePath
