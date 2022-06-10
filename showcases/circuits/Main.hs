module Main (main) where



import           Control.Parallel.Strategies
import qualified Data.Set                    as S
import           Graphics.Rendering.Cairo    as C hiding (x, y)

import Draw                           as D
import Geometry.Coordinates.Hexagonal as Hex

import Circuits.GrowingProcess
import Circuits.ReconstructWires
import Circuits.Render
import Geometry as G



-- ghcid --command='stack ghci generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
-- ghcid --command='stack ghci generative-art:lib generative-art:exe:haskell-logo-circuits --main-is=generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
main :: IO ()
main = do
    let lambdaScale = 6
        lambdaGeometry = hexLambda lambdaScale

        lambdaCircuits = reconstructWires (circuitProcess lambdaGeometry)
    let mainRender = do
            let cellSize = 3
            C.translate (fromIntegral picWidth/2) (fromIntegral picHeight/2)
            cairoScope $ do
                setLineWidth 1
                renderWires purple cellSize lambdaCircuits
            cairoScope $ do
                setLineWidth 1
                let padding = 1
                    Hex.Polygon corners = outerPolygon lambdaScale padding
                for_ (zip corners (tail (cycle corners))) $ \(a,b) -> do
                    let line = Line (Hex.toVec2 cellSize a) (Hex.toVec2 cellSize b)
                    D.sketch line
                setColor (mathematica97 3)
                stroke
    render "out/circuits.svg" picWidth picHeight mainRender
    render "out/circuits.png" picWidth picHeight $ do
        cairoScope $ do
            setSourceRGB 1 1 1
            paint
        mainRender
  where
    picWidth = 600
    picHeight = 600

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

    floodFillStart = hexZero
    floodFilled = floodFill floodFillStart (edgePoints polygon)
    pointsOnInside = floodFilled `S.difference` pointsOnEdge
    pointsOnEdge = edgePoints polygon

walkInSteps :: [a -> a] -> a -> [a]
walkInSteps [] _pos = []
walkInSteps (f:fs) pos =
    let newPoint = f pos
    in newPoint : walkInSteps fs newPoint

outerPolygon :: Int -> Int -> Hex.Polygon
outerPolygon c padding = polygon
  where
    polygon = Hex.Polygon corners
    corners = walkInSteps
        [ move L   padding . move UL padding
        , move R  (c*2+2*padding)
        , move DR (c*10+2*padding)
        , move L  (c*2+2*padding)
        , move UL (c*3-1*padding)
        , move DL (c*3-1*padding)
        , move L  (c*2+2*padding)
        , move UR (c*5+1*padding)
        ]
        (move UL (c*5) (move L c hexZero))
