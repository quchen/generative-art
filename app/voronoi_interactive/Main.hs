{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative             (liftA2)
import           Control.Monad                   (replicateM)
import           Data.Char                       (ord)
import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Data.Maybe                      (mapMaybe)
import           Data.Vector                     (fromList)
import           Prelude                         hiding ((**))
import           System.Environment              (getArgs)
import           System.Random.MWC               (GenIO, initialize, uniformR)
import           System.Random.MWC.Distributions (normal)

import qualified Graphics.Rendering.Cairo        as Cairo
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Geometry
import           Voronoi
import           Draw

data RGB = RGB { r :: Double, g :: Double, b :: Double }

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
    let w = 1200
        h = 1200
    elemCanvas <- UI.canvas
        # set UI.width w
        # set UI.height h
    getBody window #+ [element elemCanvas]

    let eAddPoint = (\(x, y) voronoi -> addPoint' voronoi (Vec2 x y)) <$> UI.mousedown elemCanvas

    bVoronoi <- accumB (emptyVoronoi (fromIntegral w) (fromIntegral h)) eAddPoint

    onChanges bVoronoi $ \voronoi -> for_ (faces voronoi) $ \face -> drawFaceCanvas elemCanvas face

    pure ()

drawFaceCanvas :: UI.Element -> VoronoiFace a -> UI ()
drawFaceCanvas elemCanvas (VF{..}) = case face of
    Polygon [] -> pure ()
    Polygon (p:ps) -> do
        elemCanvas # UI.beginPath
        elemCanvas # UI.moveTo (coordinates p)
        for_ ps $ \p -> elemCanvas # UI.lineTo (coordinates p)
        elemCanvas # set' UI.fillStyle (UI.htmlColor "#eeeeee")
        elemCanvas # UI.fill
        elemCanvas # set' UI.strokeStyle "#000000"
        elemCanvas # UI.stroke

coordinates :: Vec2 -> (Double, Double)
coordinates (Vec2 x y) = (x, y)

drawFace :: VoronoiFace () -> RGB -> Cairo.Render ()
drawFace VF{..} = drawPoly face

drawPoly :: Polygon -> RGB -> Cairo.Render ()
drawPoly (Polygon []) _ = pure ()
drawPoly poly color = do
    let fillColor = color
        lineColor = lighten 0.2 color
    polygonSketch poly
    setColor fillColor
    Cairo.fillPreserve
    setColor lineColor
    Cairo.setLineWidth 1
    Cairo.stroke

setColor :: RGB -> Cairo.Render ()
setColor RGB{..} = Cairo.setSourceRGB r g b

uniformlyDistributedPoints :: GenIO -> Int -> Int -> Int -> IO [Vec2]
uniformlyDistributedPoints gen width height count = replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width) (randomCoordinate height)
    randomCoordinate mx = fmap fromIntegral (uniformR (0, mx) gen :: IO Int)

gaussianDistributedPoints :: GenIO -> (Int, Double) -> (Int, Double) -> Int -> IO [Vec2]
gaussianDistributedPoints gen (width, sigmaX) (height, sigmaY) count = replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width sigmaX) (randomCoordinate height sigmaY)
    randomCoordinate mx sigma = do
        coord <- normal (fromIntegral mx/2) sigma gen :: IO Double
        if coord < 0 || coord > fromIntegral mx
            then randomCoordinate mx sigma
            else pure coord

darkGrey :: RGB
darkGrey = RGB 0.1 0.1 0.1

lighten :: Double -> RGB -> RGB
lighten d RGB{..} = RGB (r + d) (g + d) (b + d)
