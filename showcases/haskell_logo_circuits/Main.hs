module Main (main) where

import qualified Data.Set                       as S
import           Draw                           as D
import           Geometry.Coordinates.Hexagonal as Hex
import           Graphics.Rendering.Cairo       as C hiding (x, y)

import Circuits.GrowingProcess
import Circuits.Render

-- ghcid --command='stack ghci generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
-- ghcid --command='stack ghci generative-art:lib generative-art:exe:haskell-logo-circuits --main-is=generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
main :: IO ()
main = do
    let lambdaScale = 3
        lambdaGeometry = hexLambda lambdaScale
        lambdaCircuits = circuitProcess lambdaGeometry

        surroundingScale = lambdaScale*8
        surroundingGeometry = largeSurroundingCircle surroundingScale lambdaGeometry
        surroundingCircuits = circuitProcess surroundingGeometry
    let mainRender = do
            let cellSize = 6
            C.translate 240 220
            -- cairoScope $ grouped (paintWithAlpha 0.5) $ hexagonalCoordinateSystem cellSize 50
            setLineWidth 1
            renderCircuits purple cellSize lambdaCircuits
            renderCircuits grey cellSize surroundingCircuits
    withSurfaceAuto "out/haskell_logo_circuits.svg" picWidth picHeight (\surface -> renderWith surface mainRender)
    withSurfaceAuto "out/haskell_logo_circuits.png" picWidth picHeight (\surface -> renderWith surface $ do
        cairoScope $ do
            setSourceRGB 1 1 1
            paint
        mainRender
        )
  where
    picWidth = 480
    picHeight = 440

hexLambda :: Int -> ProcessGeometry Cube
hexLambda c | c <= 0 = ProcessGeometry S.empty S.empty
hexLambda c = ProcessGeometry
    { _inside = pointsOnInside
    , _edge =  pointsOnEdge
    }
  where
    polygon = Hex.Polygon corners
    corners = walkInSteps
        [ id
        , move R  (c*2)
        , move DR (c*10)
        , move L  (c*2)
        , move UL (c*3)
        , move DL (c*3)
        , move L  (c*2)
        , move UR (c*5)
        ]
        (move UL (c*5) (move L c hexZero))
    walkInSteps [] _pos = []
    walkInSteps (f:fs) pos =
        let newPoint = f pos
        in newPoint : walkInSteps fs newPoint

    floodFillStart = hexZero
    floodFilled = floodFill floodFillStart (edgePoints polygon)
    pointsOnInside = floodFilled `S.difference` pointsOnEdge
    pointsOnEdge = edgePoints polygon

largeSurroundingCircle :: Int -> ProcessGeometry Cube -> ProcessGeometry Cube
largeSurroundingCircle c excludes =
    let allExcluded = _inside excludes <> _edge excludes
        largeCircle = S.fromList (hexagonsInRange c hexZero)
        excludesExtended = S.unions (S.map (\hex -> S.fromList (ring 1 hex)) (_edge excludes))
        edge = let outer = S.fromList (ring c hexZero)
                   inner = excludesExtended `S.difference` allExcluded
               in outer <> inner
        inside = largeCircle `S.difference` edge `S.difference` allExcluded
    in ProcessGeometry
        { _inside = inside
        , _edge = edge
        }
