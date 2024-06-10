module Main (main) where



import           Control.Parallel.Strategies
import qualified Data.Set                    as S
import           Graphics.Rendering.Cairo    as C hiding (x, y)

import Draw                           as D
import Geometry.Coordinates.Hexagonal as Hex

import Circuits.GrowingProcess
import Circuits.ReconstructWires
import Circuits.Render



-- ghcid --command='stack ghci generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
-- ghcid --command='stack ghci generative-art:lib generative-art:exe:haskell-logo-circuits --main-is=generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
main :: IO ()
main = do
    let lambdaScale = 2
        lambdaGeometry = hexLambda lambdaScale

        surroundingScale = lambdaScale*20
        surroundingGeometry = largeSurroundingCircle surroundingScale lambdaGeometry

        (lambdaCircuits, surroundingCircuits) =
            (reconstructWires (circuitProcess lambdaGeometry), reconstructWires (circuitProcess surroundingGeometry))
            `using` parTuple2 rdeepseq rdeepseq
    let mainRender = do
            let cellSize = 3
            C.translate (fromIntegral picWidth/2) (fromIntegral picHeight/2)
            cairoScope $ do
                setLineWidth 1.5
                renderWires purple  cellSize lambdaCircuits
                renderWires grey cellSize surroundingCircuits
    render "out/circuits.svg" picWidth picHeight mainRender
    render "out/circuits.png" picWidth picHeight $ do
        cairoScope $ do
            setSourceRGB 1 1 1
            paint
        mainRender
  where
    picWidth = 128
    picHeight = 128

-- | A lambda in hexagonal coordinates.
hexLambda
    :: Int -- ^ Scale parameter. c*10 will be the total height.
    -> ProcessGeometry
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
    :: Int             -- ^ Radius of the hexagon
    -> ProcessGeometry -- ^ Geometry to be cut out
    -> ProcessGeometry
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
