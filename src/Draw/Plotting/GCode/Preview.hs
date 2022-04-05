module Draw.Plotting.GCode.Preview (preview) where

import Graphics.Rendering.Cairo as C
import Draw.Plotting.GCode
import Data.Maybe (fromMaybe)
import Geometry

data PreviewState = S
    { _penXY :: Vec2
    , _penZ :: Double
    }

preview :: [GCode] -> Render ()
preview = go S { _penXY = Vec2 0 0, _penZ = 0 }
  where
    go _ [] = pure ()
    go s@S { _penXY = Vec2 x0 y0, _penZ = z0 } (g:gs) = case g of
        G00_LinearRapidMove x1 y1 Nothing | z0 > 0 -> do
            let x' = fromMaybe x0 x1
                y' = fromMaybe y0 y1
            moveTo x' y'
            go s { _penXY = Vec2 x' y'} gs
        G00_LinearRapidMove x1 y1 Nothing -> do
            let x' = fromMaybe x0 x1
                y' = fromMaybe y0 y1
            lineTo x' y'
            go s { _penXY = Vec2 x' y'} gs
        G00_LinearRapidMove Nothing Nothing (Just z1) | z0 < 0 -> do
            stroke
            go s { _penZ = z1 } gs
        G00_LinearRapidMove Nothing Nothing (Just z1) -> do
            go s { _penZ = z1 } gs
        G00_LinearRapidMove _ _ _ -> error "Weird move"
        G01_LinearFeedrateMove _ x1 y1 Nothing | z0 > 0 -> do
            let x' = fromMaybe x0 x1
                y' = fromMaybe y0 y1
            moveTo x' y'
            go s { _penXY = Vec2 x' y'} gs
        G01_LinearFeedrateMove _ x1 y1 Nothing -> do
            let x' = fromMaybe x0 x1
                y' = fromMaybe y0 y1
            lineTo x' y'
            go s { _penXY = Vec2 x' y'} gs
        G01_LinearFeedrateMove _ Nothing Nothing (Just z1) | z0 < 0 -> do
            stroke
            go s { _penZ = z1 } gs
        G01_LinearFeedrateMove _ Nothing Nothing (Just z1) -> do
            go s { _penZ = z1 } gs
        G01_LinearFeedrateMove _ _ _ _ -> error "Weird move"
        _otherwise -> go s gs
