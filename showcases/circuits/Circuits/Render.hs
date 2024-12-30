module Circuits.Render (
      renderWires
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



renderWire :: Double -> [Hex] -> Render ()
renderWire cellSize = go . map (toVec2 cellSize)
  where
    circleRadius = cellSize/2
    go [] = pure ()
    go [x] = sketch (Circle x circleRadius) >> stroke
    go [x,y] = do
        let shortLine = resizeLine (\d -> d - circleRadius) (Line x y)
        sketch shortLine
        stroke
        sketch (Circle y circleRadius)
        stroke
    go (x:rest@(y:_)) = do
        sketch (Line x y)
        go rest

renderWires
    :: ColorScheme
    -> Double
    -> Set [Hex]
    -> Render ()
renderWires scheme cellSize wires = do
    gen <- liftIO MWC.create
    for_ wires $ \wire -> do
        randomColor gen scheme
        renderWire cellSize wire

newtype ColorScheme = ColorScheme (V.Vector (Render ()))

purple :: ColorScheme
purple = ColorScheme (V.fromList [darker, dark, brighter])
  where
    darker   = setColor (haskell 0)
    dark     = setColor (haskell 1)
    brighter = setColor (haskell 2)

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
renderProcessGeometry insideColor edgeColor cellSize ProcessGeometry{..} = do
    cairoScope $ do
        for_ _inside $ \hex -> D.sketch (hexagonPoly cellSize hex)
        setColor insideColor
        fillPreserve
        setSourceRGB 0 0 0
        stroke

    cairoScope $ do
        setColor edgeColor
        for_ _edge $ \hex -> D.sketch (hexagonPoly cellSize hex)
        setColor edgeColor
        fillPreserve
        setSourceRGB 0 0 0
        stroke
