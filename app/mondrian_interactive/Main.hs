import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Prelude                         hiding ((**), Either(..))

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Geometry
import           Mondrian
import qualified Data.Map as Map




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

    onChanges bMondrian $ \mondrian -> do
        for_ (zip (asPolygons 100 mondrian) [0..]) $ \(poly, color) -> drawFaceCanvas elemCanvas poly color
        for_ (edges mondrian) $ \(Line a b) -> do
            elemCanvas # UI.moveTo (coordinates a)
            elemCanvas # UI.lineTo (coordinates b)
            elemCanvas # set' UI.strokeStyle "#ff00ff"
            elemCanvas # set' UI.lineWidth 4
            elemCanvas # UI.stroke


    pure ()

toggleEdge :: (Double, Double) -> Mondrian -> Mondrian
toggleEdge (x, y) mondrian = case find (pointInPolygon (Vec2 x y) . fst) edgeClickAreas of
    Just (_, (vertex, direction)) -> removeEdge (direction, vertex) mondrian
    Nothing -> mondrian

edges :: Mondrian -> [Line]
edges mondrian = do
    ((x1, y1), edges) <- Map.toList mondrian
    (_, (x2, y2)) <- Map.toList edges
    pure $ Line (Vec2 (fromIntegral x1 * 100) (fromIntegral y1 * 100)) (Vec2 (fromIntegral x2 * 100) (fromIntegral y2 * 100))

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
