{-# LANGUAGE RecordWildCards #-}
module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (rotate, transform, x, y)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Cairo (Matrix(..))
import Graphics.Rendering.Cairo.SVG as Cairo
import System.Environment (getArgs)

import Draw
import Geometry
import Penrose



picWidth, picHeight :: Num a => a
picWidth = 2000
picHeight = 2000

main :: IO ()
main = do
    [protoFile, numGenerations, targetFile] <- getArgs
    let generations = read numGenerations :: Int
    svg <- svgNewFromFile protoFile
    withSurfaceAuto targetFile picWidth picHeight $ \surface ->
        renderWith surface $ do
            Cairo.rectangle 0 0 picWidth picHeight
            Cairo.setSourceRGB 1 1 1
            Cairo.fill
            let initial = rotateAround (Vec2 (picWidth/2) (picHeight/2)) (deg 90) $ asymmetricDecagon (Vec2 (picWidth/2) (picHeight/2)) (min picWidth picHeight / 2)
                tiling = foldl' (>>=) initial (replicate generations subdivide)
            for_ tiling $ renderProtoTile svg

renderProtoTile :: SVG -> Tile -> Render ()
renderProtoTile prototile t@Tile{..} = restoreStateAfter $ do
    polygonSketchOpen (asPolygon t)
    restoreStateAfter $ do
        Cairo.setSourceRGBA 1 0 0 0.5
        Cairo.setLineWidth 0.5
        --strokePreserve
    clip
    let Distance unitLength = norm (tileP1 -. tileP0)
    case (tileType, polygonOrientation (asPolygon t)) of
        (Thin, PolygonPositive) -> do
            let Vec2 x y = tileP1 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP2 tileP1)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
            Cairo.rotate (-2 * getRad alpha)
            Cairo.translate (-100 * phi * cos (getRad alpha)) (-100 * phi * sin(getRad alpha))
        (Thin, PolygonNegative) -> do
            let Vec2 x y = tileP1 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP1 tileP0)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
            Cairo.rotate (-2 * getRad alpha)
            Cairo.translate (-100 * phi * cos (getRad alpha)) (100 * phi * sin(getRad alpha))
            cairoMirrorV
        (Thick, PolygonPositive) -> do
            let Vec2 x y = tileP2 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP1 tileP0)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
        (Thick, PolygonNegative) -> do
            let Vec2 x y = tileP2 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP2 tileP1)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
    let scaleFactor = 100 / 93.75 -- Why? I have no idea.
    Cairo.scale scaleFactor scaleFactor
    _ <- svgRender prototile
    pure ()

cairoMirrorV :: Render ()
cairoMirrorV = Cairo.transform (Cairo.Matrix 1 0 0 (-1) 0 0)

polygonSketchOpen :: Polygon -> Render ()
polygonSketchOpen (Polygon []) = pure ()
polygonSketchOpen (Polygon (Vec2 x y : vecs)) = do
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
