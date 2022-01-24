{-# LANGUAGE RecordWildCards #-}

module Circuits.Render (
      renderCircuits
    , renderProcessGeometry

    , ColorScheme(..)
    , purple
    , grey
    , pitchBlackForDebugging
) where



import           Data.Function
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (x, y)
import qualified System.Random.MWC        as MWC

import Circuits.GrowingProcess
import Draw                           as D
import Geometry                       as G
import Geometry.Chaotic
import Geometry.Coordinates.Hexagonal as Hex



renderSingleWire
    :: (HexagonalCoordinate hex, Ord hex)
    => Double
    -> Map hex (CellState hex)
    -> hex
    -> Render ()
renderSingleWire cellSize allKnownCells start = do
    moveToVec (toVec2 cellSize start)
    fix (\go currentPosHex -> case M.lookup currentPosHex allKnownCells of
            Nothing -> do
                stroke
                crossSketch (toVec2 cellSize currentPosHex) (cellSize/2)
            Just (WireTo target) -> do
                case M.lookup target allKnownCells of
                    Just WireEnd -> do
                        let circleRadius = cellSize/2
                            currentPosVec = toVec2 cellSize currentPosHex
                            circleCenterVec = toVec2 cellSize target
                            Line _ targetVecShortened = resizeLine (\d -> d - circleRadius) (Line currentPosVec circleCenterVec)
                        lineToVec targetVecShortened
                        stroke
                        circleSketch circleCenterVec circleRadius
                        stroke
                    _other -> lineToVec (toVec2 cellSize target)
                go target
            Just WireEnd ->
                -- We handle this case in the WireTo part so we can shorten the line leading
                -- to the circle to avoid circle/line overlap
                pure ()
        )
        start

renderCircuits
    :: (HexagonalCoordinate hex, Ord hex)
    => ColorScheme
    -> Double
    -> Circuits hex
    -> Render ()
renderCircuits scheme cellSize allCircuits = do
    gen <- liftIO $ MWC.initialize (V.fromList [fromIntegral (perturb cellSize), fromIntegral $ perturb (S.size (_starts allCircuits))])
    let loop circuits = case S.minView (_starts circuits) of
            Nothing -> pure ()
            Just (start, rest) -> do
                randomColor gen scheme
                renderSingleWire cellSize (_nodes circuits) start
                loop circuits{ _starts = rest }
    loop allCircuits

newtype ColorScheme = ColorScheme (V.Vector (Render ()))

purple :: ColorScheme
purple = ColorScheme (V.fromList [darker, dark, brighter])
  where
    darker = setColor (hsva 257 0.40 0.38 1)
    dark = setColor (hsva 256 0.40 0.50 1)
    brighter = setColor (hsva 304 0.45 0.56 1)

grey :: ColorScheme
grey = ColorScheme (V.fromList [setGrey x | x <- [850, 875, 900]])
  where
    setGrey per1000 =
        let x = fromIntegral per1000 / 1000
        in setColor (rgb x x x)

pitchBlackForDebugging :: ColorScheme
pitchBlackForDebugging = ColorScheme (V.fromList [setColor (rgb 0 0 0)])

randomColor
    :: MWC.GenIO
    -> ColorScheme
    -> Render ()
randomColor gen (ColorScheme scheme) = do
    n <- liftIO $ MWC.uniformRM (0, V.length scheme-1) gen
    scheme V.! n

-- renderProcessGeometry
--     :: HexagonalCoordinate  hex
--     => Double
--     -> ProcessGeometry hex
--     -> Render ()
renderProcessGeometry
    :: (HexagonalCoordinate hex, CairoColor filling, CairoColor edges)
    => filling
    -> edges
    -> Double
    -> ProcessGeometry hex
    -> Render ()
renderProcessGeometry insideColor edgeColor cellSize ProcessGeometry{..} = do
    cairoScope $ do
        for_ _inside $ \hex -> do
            D.polygonSketch (hexagonPoly cellSize hex)
        setColor (insideColor)
        fillPreserve
        setSourceRGB 0 0 0
        stroke

    cairoScope $ do
        setColor (edgeColor)
        for_ _edge $ \hex -> do
            D.polygonSketch (hexagonPoly cellSize hex)
        setColor (edgeColor)
        fillPreserve
        setSourceRGB 0 0 0
        stroke
