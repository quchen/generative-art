module Circuits.Render (
      renderWire
    , renderWires
    , renderProcessGeometry

    , ColorScheme(..)
    , purple
    , grey
    , pitchBlackForDebugging
) where



import           Data.Set                 (Set)
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (x, y)
import qualified System.Random.MWC        as MWC

import Circuits.GrowingProcess
import Draw                           as D
import Geometry                       as G
import Geometry.Coordinates.Hexagonal as Hex



renderWire :: Double -> Double -> [Hex] -> Render ()
renderWire _ _ [] = pure ()
renderWire cellSize circleRadius hexes@(first:_) = do
    moveToVec (toVec2 cellSize first)
    go (map (toVec2 cellSize) hexes)
  where
    go [] = pure ()
    go [x] = sketch (Circle x circleRadius) >> stroke
    go [x,y] = do
        let Line _ end = resizeLine (\d -> d - circleRadius) (Line x y)
        lineToVec end
        stroke
        sketch (Circle y circleRadius)
        stroke
    go (_:rest@(y:_)) = do
        lineToVec y
        go rest

renderWires
    :: ColorScheme
    -> Double
    -> Double
    -> Set [Hex]
    -> Render ()
renderWires scheme cellSize circleRadius wires = do
    gen <- liftIO MWC.create
    for_ wires $ \wire -> do
        randomColor gen scheme
        renderWire cellSize circleRadius wire

newtype ColorScheme = ColorScheme (V.Vector (Render ()))

purple :: ColorScheme
purple = ColorScheme (V.fromList [darker, dark, brighter])
  where
    darker   = setColor (haskell 0)
    dark     = setColor (haskell 0.5)
    brighter = setColor (haskell 1)

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

renderProcessGeometry
    :: (CairoColor filling, CairoColor edges)
    => filling
    -> edges
    -> Double
    -> ProcessGeometry
    -> Render ()
renderProcessGeometry insideColor edgeColor cellSize processGeometry = do
    cairoScope $ do
        for_ (_inside processGeometry) $ \hex -> D.sketch (hexagonPoly cellSize hex)
        setColor insideColor
        fillPreserve
        setSourceRGB 0 0 0
        stroke

    cairoScope $ do
        setColor edgeColor
        for_ (_edge processGeometry) $ \hex -> D.sketch (hexagonPoly cellSize hex)
        setColor edgeColor
        fillPreserve
        setSourceRGB 0 0 0
        stroke
