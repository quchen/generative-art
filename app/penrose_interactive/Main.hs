{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Data.Foldable (for_, traverse_)
import Prelude hiding (length, flip)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Graphics.Rendering.Cairo as Cairo
import Data.List (partition)
import Geometry
import Penrose

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = mdo
    elemCanvas <- UI.canvas
        # set UI.width 800
        # set UI.height 800
    btnType <- UI.button
    btnSubdivide <- UI.button # set UI.text "subdivide"
    btnReset <- UI.button # set UI.text "reset"
    btnClear <- UI.button # set UI.text "clear"
    btnUndo <- UI.button # set UI.text "undo"
    btnSave <- UI.button # set UI.text "save svg"
    inputFileName <- UI.input # set UI.type_ "text"
        # set UI.value "penrose.svg"
    _ <- getBody window #+ [ column
        [ row $ map element
            [ btnClear, btnReset, btnSubdivide, btnUndo, btnType ]
        , row $ map element
            [ inputFileName, btnSave ]
        , element elemCanvas ] ]

    let initialTiles = decagonRose origin 400
        origin = Vec2 400 400

    let eSubdivide = (>>= subdivide) <$ UI.click btnSubdivide
        eReset = const initialTiles <$ UI.click btnReset
        eHighlighted = subdivideHighlighted <$> (uncurry Vec2 <$> UI.mousedown elemCanvas)
        eTiles = concatenate <$> unions [eSubdivide, eHighlighted, eReset]
        eHistory = (\f h -> f (head h) : h) <$> eTiles
        eUndo = tail <$ UI.click btnUndo
    bTiles <- accumB (pure initialTiles) $ unionWith (.) eHistory eUndo
    bDrawingType <- accumB Rhombs (toggleDrawingType <$ UI.click btnType)
    onChanges (liftA2 (,) bTiles bDrawingType) $ \(tiles:_, drawingType) ->
        render elemCanvas tiles drawingType
    _ <- onEvent (UI.click btnClear) $ \() -> UI.clearCanvas elemCanvas
    _ <- element btnType # sink UI.text (show <$> bDrawingType)

    _ <- onEvent (liftA2 (,) bTiles bDrawingType <@ UI.click btnSave) $ \(tiles:_, drawingType) -> do
        fileName <- get UI.value inputFileName
        liftIO $ Cairo.withSVGSurface fileName 800 800 $ \surface ->
            case drawingType of
                Rhombs -> traverse_ (drawTileSvg surface) tiles
                Pentagons -> traverse_ (drawPentagonSvg surface) tiles
                Both -> do
                    traverse_ (drawTileSvg surface) tiles
                    traverse_ (drawPentagonSvg surface) tiles
    render elemCanvas initialTiles Rhombs

subdivideHighlighted :: Vec2 -> [Tile] -> [Tile]
subdivideHighlighted point tiles = otherTiles ++ (highlightedTiles >>= subdivide)
  where
    (highlightedTiles, otherTiles) = partition (pointInPolygon point . asPolygon) tiles

render :: Element -> [Tile] -> DrawingType -> UI ()
render elemCanvas tiles drawingType = do
    UI.clearCanvas elemCanvas
    case drawingType of
        Rhombs -> traverse_ (drawTile elemCanvas) tiles
        Pentagons -> traverse_ (drawInscribedPentagon elemCanvas) tiles
        Both -> do
            traverse_ (drawTile elemCanvas) tiles
            traverse_ (drawInscribedPentagon elemCanvas) tiles

drawTile :: Element -> Tile -> UI ()
drawTile elemCanvas Tile{..} = do
    let tileColor = UI.htmlColor $ case tileType of
            Thin  -> "#aaaaff"
            Thick -> "#ffaaaa"
    elemCanvas # set' UI.fillStyle tileColor
    elemCanvas # UI.beginPath
    elemCanvas # UI.moveTo (coordinates tileP0)
    elemCanvas # UI.lineTo (coordinates tileP1)
    elemCanvas # UI.lineTo (coordinates tileP2)
    elemCanvas # UI.fill

drawInscribedPentagon :: Element -> Tile -> UI ()
drawInscribedPentagon elemCanvas tile =
    for_ (inscribedPentagons tile) $ \(Polygon (p0:ps)) -> do
        elemCanvas # UI.beginPath
        elemCanvas # UI.moveTo (coordinates p0)
        for_ ps $ \p -> elemCanvas # UI.lineTo (coordinates p)
        elemCanvas # UI.stroke

drawTileSvg :: Cairo.Surface  -> Tile -> IO ()
drawTileSvg surface Tile{..} = Cairo.renderWith surface $ do
    Cairo.newPath
    let Vec2 x y = tileP0 in Cairo.moveTo x y
    let Vec2 x y = tileP1 in Cairo.lineTo x y
    let Vec2 x y = tileP2 in Cairo.lineTo x y
    case tileType of
        Thin -> Cairo.setSourceRGB (2/3) (2/3) 1
        Thick -> Cairo.setSourceRGB 1 (2/3) (2/3)
    Cairo.fillPreserve

drawPentagonSvg :: Cairo.Surface -> Tile -> IO ()
drawPentagonSvg surface tile =
    for_ (inscribedPentagons tile) $ \(Polygon (p0:ps)) -> Cairo.renderWith surface $ do
        Cairo.newPath
        let Vec2 x y = p0 in Cairo.moveTo x y
        for_ ps $ \(Vec2 x y) -> Cairo.lineTo x y
        Cairo.setSourceRGB 0 0 0
        Cairo.stroke

coordinates :: Vec2 -> UI.Point
coordinates (Vec2 x y) = (x, y)

data DrawingType = Pentagons | Rhombs | Both
    deriving (Eq, Show)

toggleDrawingType :: DrawingType -> DrawingType
toggleDrawingType = \case
    Rhombs -> Pentagons
    Pentagons -> Both
    Both -> Rhombs
