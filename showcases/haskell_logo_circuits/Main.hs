module Main (main) where

import qualified Data.Set                 as S
import           Graphics.Rendering.Cairo as C hiding (x, y)

import Draw                           as D
import Geometry.Coordinates.Hexagonal as Hex

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
            -- cartesianCoordinateSystem
            let cellSize = 6
            C.translate 260 220
            cairoScope $ grouped (paintWithAlpha 0.2) $ do
                setLineWidth 1
                renderProcessGeometry (mmaColor 0 1) (mmaColor 1 1) cellSize lambdaGeometry
                renderProcessGeometry (mmaColor 2 1) (mmaColor 3 1) cellSize surroundingGeometry
            -- cairoScope $ grouped (paintWithAlpha 0.5) $ hexagonalCoordinateSystem cellSize 50
            cairoScope $ grouped (paintWithAlpha 0.3) $ do
                setLineWidth 2
                renderCircuits pitchBlackForDebugging cellSize lambdaCircuits
                renderCircuits pitchBlackForDebugging cellSize surroundingCircuits
    withSurfaceAuto "out/haskell_logo_circuits.svg" picWidth picHeight (\surface -> renderWith surface mainRender)
    withSurfaceAuto "out/haskell_logo_circuits.png" picWidth picHeight (\surface -> renderWith surface $ do
        cairoScope $ do
            setSourceRGB 1 1 1
            paint
        mainRender
        )
  where
    picWidth = 520
    picHeight = 440

-- | A lambda in hexagonal coordinates.
hexLambda
    :: Int -- ^ Scale parameter. c*10 will be the total height.
    -> ProcessGeometry Cube
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

-- | A large hexagon with some geometry cut out.
largeSurroundingCircle
    :: Int                  -- ^ Radius of the hexagon
    -> ProcessGeometry Cube -- ^ Geometry to be cut out
    -> ProcessGeometry Cube
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
