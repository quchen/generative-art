{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Numeric           (showHex)
import Prelude           hiding ((**))
import System.IO.Temp    (withSystemTempDirectory)
import System.Random.MWC (createSystemRandom, uniform)

import qualified Graphics.Rendering.Cairo    as Cairo
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import Geometry.Algorithms.Delaunay
import Draw
import Geometry
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi

main :: IO ()
main = withSystemTempDirectory "voronoi-interactive" $ \tmpDir -> UI.startGUI UI.defaultConfig (setup tmpDir)

setup :: FilePath -> Window -> UI ()
setup tmpDir window = do
    let w = 1200
        h = 1200
    gen <- liftIO createSystemRandom

    elemCanvas <- UI.canvas # set UI.width w # set UI.height h # set UI.style [("background", "#eeeeee")]
    btnAddPointsGaussian <- UI.button # set UI.text "Add Gaussian distributed points"
    btnAddPointsUniform <- UI.button # set UI.text "Add uniformly distributed points"
    btnReset <- UI.button # set UI.text "Reset"
    btnSave <- UI.button # set UI.text "Save"
    inputFileName <- UI.input # set UI.type_ "text" # set UI.value "voronoi.svg"

    _ <- getBody window #+
        [ row
            [ element elemCanvas
            , column
                [ element btnAddPointsGaussian
                , element btnAddPointsUniform
                , element btnReset
                , row [element inputFileName, element btnSave]
                ]
            ]
        ]

    let bb = boundingBox (Vec2 0 0, Vec2 (fromIntegral w) (fromIntegral h))
        initialState = bowyerWatson bb []

    eAddPointsGaussian <- do
        (eAddPoints, triggerAddPoints) <- liftIO newEvent
        on UI.click btnAddPointsGaussian $ \() -> liftIO $ do
            points <- gaussianDistributedPoints gen bb (Mat2 380 0 0 380) 100
            triggerAddPoints (\delaunay -> foldl' bowyerWatsonStep delaunay points)
        pure eAddPoints
    eAddPointsUniform <- do
        (eAddPoints, triggerAddPoints) <- liftIO newEvent
        on UI.click btnAddPointsUniform $ \() -> liftIO $ do
            points <- uniformlyDistributedPoints gen w h 100
            triggerAddPoints (\delaunay -> foldl' bowyerWatsonStep delaunay points)
        pure eAddPoints
    let eAddPointByClicking = (\(x, y) -> flip bowyerWatsonStep (Vec2 x y)) <$> UI.mousedown elemCanvas
        eReset = const initialState <$ UI.click btnReset
        eDelaunay = concatenate <$> unions [eAddPointsGaussian, eAddPointsUniform, eAddPointByClicking, eReset]

    bDelaunay <- accumB initialState eDelaunay

    onChanges bDelaunay $ \delaunay -> do
        tmpFile <- liftIO $ do
            randomNumber <- uniform gen :: IO Int
            pure (tmpDir ++ "/" ++ showHex (abs randomNumber) ".png")
        let voronoi = toVoronoi delaunay
        liftIO $ withSurfaceAuto tmpFile w h $ \surface -> Cairo.renderWith surface $ for_ (cells voronoi) drawCellCairo
        outFile <- loadFile "image/png" tmpFile
        outImg <- UI.img # set UI.src outFile
        on (UI.domEvent "load") outImg $ \_ -> do
            elemCanvas # UI.clearCanvas
            elemCanvas # UI.drawImage outImg (0, 0)

    on UI.click btnSave $ \() -> do
        fileName <- get UI.value inputFileName
        voronoi <- liftIO $ toVoronoi <$> currentValue bDelaunay
        liftIO $ withSurfaceAuto fileName w h $ \surface -> Cairo.renderWith surface $ for_ (cells voronoi) drawCellCairo

drawCellCairo :: VoronoiCell () -> Cairo.Render ()
drawCellCairo Cell{..} = case region of
    Polygon [] -> pure ()
    poly -> do
        let fillColor = parseRgbHex "#eeeeee"
            lineColor = parseRgbHex "#5d81b4"
        sketch poly
        setColor fillColor
        Cairo.fillPreserve
        setColor lineColor
        Cairo.setLineWidth 1
        Cairo.stroke
        sketch (Circle seed 5)
        Cairo.fill
