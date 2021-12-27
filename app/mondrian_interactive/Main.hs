{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative             (liftA2)
import           Control.Monad                   (replicateM)
import           Data.Char                       (ord)
import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Data.Map                        as Map
import           Data.Maybe                      (mapMaybe)
import           Data.Vector                     (fromList)
import           Prelude                         hiding ((**), Either(..))
import           System.Environment              (getArgs)
import           System.Random.MWC               (GenIO, initialize, uniformR)
import           System.Random.MWC.Distributions (normal)

import qualified Graphics.Rendering.Cairo        as Cairo
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Geometry
import           Mondrian
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

    let eToggleEdge = toggleEdge <$> UI.mousedown elemCanvas

    bMondrian <- accumB (mondrianBaseGrid 10 10) eToggleEdge

    onChanges bMondrian $ \mondrian -> for_ (zip (asPolygons 100 mondrian) [0..]) $
        \(poly, color) -> drawFaceCanvas elemCanvas poly color

    pure ()

toggleEdge :: (Double, Double) -> Mondrian -> Mondrian
toggleEdge (x, y) mondrian = case find (pointInPolygon (Vec2 x y) . fst) edgeClickAreas of
    Just (_, (vertex, direction)) -> removeEdge direction vertex mondrian
    Nothing -> mondrian

edgeClickAreas :: [(Polygon, (Vertex, Direction))]
edgeClickAreas = do
    x <- [0..10]
    y <- [0..10]
    let x' = 100 * fromIntegral x
        y' = 100 * fromIntegral y
    [ (Polygon [Vec2 (x' + 20) (y' - 10), Vec2 (x' + 80) (y' - 10), Vec2 (x' + 80) (y' + 10), Vec2 (x' + 20) (y' + 10)], ((x, y), Right)) ,
      (Polygon [Vec2 (x' + 10) (y' + 20), Vec2 (x' + 10) (y' + 80), Vec2 (x' - 10) (y' + 80), Vec2 (x' - 10) (y' + 20)], ((x, y), Down)) ]

mondrianColor :: Int -> UI.FillStyle
mondrianColor color = case color of
    0 -> white
    1 -> white
    2 -> white
    3 -> white
    4 -> blue
    5 -> white
    6 -> red
    7 -> white
    8 -> white
    9 -> yellow
    10 -> blue
    11 -> red
    12 -> white
    13 -> blue
    14 -> yellow
    15 -> red
    n -> mondrianColor (n `mod` 16)
  where
    white = UI.htmlColor "#FFFFFF"
    red = UI.htmlColor "#FF0000"
    blue = UI.htmlColor "#0000FF"
    yellow = UI.htmlColor "#FFFF00"

drawFaceCanvas :: UI.Element -> Polygon -> Int -> UI ()
drawFaceCanvas elemCanvas poly color = case poly of
    Polygon [] -> pure ()
    Polygon (p:ps) -> do
        elemCanvas # UI.beginPath
        elemCanvas # UI.moveTo (coordinates p)
        for_ ps $ \p -> elemCanvas # UI.lineTo (coordinates p)
        elemCanvas # UI.lineTo (coordinates p)
        elemCanvas # set' UI.fillStyle (mondrianColor color)
        elemCanvas # UI.fill
        elemCanvas # set' UI.strokeStyle "#000000"
        elemCanvas # set' UI.lineWidth 20
        elemCanvas # UI.stroke

coordinates :: Vec2 -> (Double, Double)
coordinates (Vec2 x y) = (x, y)
