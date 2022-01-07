{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad                   (replicateM)
import           Data.Foldable                   (for_, Foldable (foldl'))
import           Numeric                         (showHex)
import           Prelude                         hiding ((**))
import           System.Random.MWC               (GenIO, uniformR, Variate (uniform), createSystemRandom)
import           System.Random.MWC.Distributions (normal)

import qualified Graphics.Rendering.Cairo        as Cairo
import qualified Graphics.UI.Threepenny          as UI
import           Graphics.UI.Threepenny.Core

import           Geometry
import           Voronoi
import           Draw

main :: IO ()
main = UI.startGUI UI.defaultConfig setup

setup :: Window -> UI ()
setup window = do
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

    let initialState = emptyVoronoi (fromIntegral w) (fromIntegral h)

    eAddPointsGaussian <- do
        (eAddPoints, triggerAddPoints) <- liftIO newEvent
        on UI.click btnAddPointsGaussian $ \() -> liftIO $ do
            points <- gaussianDistributedPoints gen (w, 380) (h, 380) 100
            triggerAddPoints (\voronoi -> foldl' addPoint' voronoi points)
        pure eAddPoints
    eAddPointsUniform <- do
        (eAddPoints, triggerAddPoints) <- liftIO newEvent
        on UI.click btnAddPointsUniform $ \() -> liftIO $ do
            points <- uniformlyDistributedPoints gen w h 100
            triggerAddPoints (\voronoi -> foldl' addPoint' voronoi points)
        pure eAddPoints
    let eAddPointByClicking = (\(x, y) -> flip addPoint' (Vec2 x y)) <$> UI.mousedown elemCanvas
        eReset = const initialState <$ UI.click btnReset
        eVoronoi = concatenate <$> unions [eAddPointsGaussian, eAddPointsUniform, eAddPointByClicking, eReset]

    bVoronoi <- accumB initialState eVoronoi

    onChanges bVoronoi $ \voronoi -> do
        tmpFile <- liftIO $ do
            randomNumber <- uniform gen :: IO Int
            pure ("tmp/" ++ showHex (abs randomNumber) ".png")
        liftIO $ withSurface PNG tmpFile w h $ \surface -> Cairo.renderWith surface $ for_ (faces voronoi) drawFaceCairo
        outFile <- loadFile "image/png" tmpFile
        outImg <- UI.img # set UI.src outFile
        on (UI.domEvent "load") outImg $ \_ -> do
            elemCanvas # UI.clearCanvas
            elemCanvas # UI.drawImage outImg (0, 0)

    on UI.click btnSave $ \() -> do
        fileName <- get UI.value inputFileName
        voronoi <- liftIO $ currentValue bVoronoi
        liftIO $ withSurfaceAuto fileName w h $ \surface -> Cairo.renderWith surface $ for_ (faces voronoi) drawFaceCairo

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

drawFaceCairo :: VoronoiFace () -> Cairo.Render ()
drawFaceCairo VF{..} = case face of
    Polygon [] -> pure ()
    poly -> do
        let fillColor = parseHex "#eeeeee"
            lineColor = parseHex "#5d81b4"
        polygonSketch poly
        setColor fillColor
        Cairo.fillPreserve
        setColor lineColor
        Cairo.setLineWidth 1
        Cairo.stroke
        circleSketch center (Distance 5)
        Cairo.fill

data RGB = RGB { r :: Double, g :: Double, b :: Double }

setColor :: RGB -> Cairo.Render ()
setColor RGB{..} = Cairo.setSourceRGB r g b

parseHex :: String -> RGB
parseHex ['#', r1, r2, g1, g2, b1, b2] = RGB
    { r = read ("0x" ++ [r1, r2]) / 255
    , g = read ("0x" ++ [g1, g2]) / 255
    , b = read ("0x" ++ [b1, b2]) / 255 }
parseHex _ = undefined
