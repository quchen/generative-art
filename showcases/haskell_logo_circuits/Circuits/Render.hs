module Circuits.Render (
    renderCircuits

    , ColorScheme(..)
    , purple
    , grey
) where



import           Data.Function
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Vector                    as V
import           Draw                           as D
import           Geometry                       as G
import           Geometry.Chaotic
import           Geometry.Coordinates.Hexagonal as Hex
import           Graphics.Rendering.Cairo       as C hiding (x, y)
import qualified System.Random.MWC              as MWC

import Circuits.GrowingProcess



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
                crossSketch (toVec2 cellSize currentPosHex) (Distance (cellSize/2))
            Just (WireTo target) -> do
                case M.lookup target allKnownCells of
                    Just WireEnd -> do
                        let circleRadius = cellSize/2
                            currentPosVec = toVec2 cellSize currentPosHex
                            circleCenterVec = toVec2 cellSize target
                            Line _ targetVecShortened = resizeLine (\(Distance d) -> Distance (d - circleRadius)) (Line currentPosVec circleCenterVec)
                        lineToVec targetVecShortened
                        stroke
                        circleSketch circleCenterVec (Distance circleRadius)
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
renderCircuits scheme cellSize allCircuits = case S.minView (_starts allCircuits) of
    Nothing -> pure ()
    Just (start, rest) -> do
        gen <- liftIO $ MWC.initialize (V.fromList [fromIntegral $ perturb (S.size (_starts allCircuits))])
        randomColor gen scheme
        renderSingleWire cellSize (_nodes allCircuits) start
        renderCircuits scheme cellSize allCircuits{ _starts = rest }

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

randomColor
    :: MWC.GenIO
    -> ColorScheme
    -> Render ()
randomColor gen (ColorScheme scheme) = do
    n <- liftIO $ MWC.uniformRM (0, V.length scheme-1) gen
    scheme V.! n
