module Main (main) where



import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Graphics.Rendering.Cairo as C hiding (x, y)

import Draw                           as D
import Geometry.Coordinates.Hexagonal as Hex

import Circuits.GrowingProcess
import Circuits.ReconstructWires
import Circuits.Render



-- ghcid --command='stack ghci generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
-- ghcid --command='stack ghci generative-art:lib generative-art:exe:haskell-logo-circuits --main-is=generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
main :: IO ()
main = do
    let lambdaScale = 3
        lambdaGeometry = hexLambda lambdaScale

        lambdaCircuits = reconstructWires (circuitProcess lambdaGeometry)
    let mainRender = do
            let cellSize = 3
            C.translate (fromIntegral picWidth/2) (fromIntegral picHeight/2)
            cairoScope $ do
                setLineWidth 1
                -- hexagonalCoordinateSystem cellSize 30
                renderWires purple cellSize lambdaCircuits
                renderWires purple (cellSize/2) munihacWriting
    render "out/munihac-2022-logo.svg" picWidth picHeight mainRender
    render "out/munihac-2022-logo.png" picWidth picHeight $ do
        cairoScope $ do
            setSourceRGB 1 1 1
            paint
        mainRender
  where
    picWidth = 520
    picHeight = 440

munihacWriting :: Set [Hex]
munihacWriting =
    let letterM =
            [ walkInSteps [id, move UL 8, move UR 2, move R 6, move DR 10] hexZero
            , walkInSteps [move UL 10 . move R 4, move DR 6] hexZero
            ]
        letterU =
            [ walkInSteps [move UL 10, move DR 10, move R 6, move UR 2, move UL 8] hexZero
            ]
        letterN =
            [ walkInSteps [id, move UL 8, move UR 2, move R 6, move DR 10] hexZero
            ]
        letterI =
            [ walkInSteps [id, move UL 10] hexZero
            ]
        letterH =
            [ walkInSteps [id, move UL 10] hexZero
            , walkInSteps [move UL 5, move R 7] hexZero
            , walkInSteps [move R 8, move UL 10] hexZero
            ]
        letterA =
            [ walkInSteps [id, move UL 8, move UR 2, move R 6, move DR 10] hexZero
            , walkInSteps [move UL 5, move R 7] hexZero
            ]
        letterC =
            [ walkInSteps [move R 8, move L 8, move UL 8, move UR 2, move R 6] hexZero
            ]
        digit2 =
            [ walkInSteps [move R 8, move L 8, move UL 5, move R 8, move UL 5, move L 8] hexZero
            ]
        digit0 =
            [ walkInSteps [id, move R 8, move UL 10, move L 8, move DR 9] hexZero
            ]
    in S.fromList . concat . (map.map.map) (move DR 2 . move R 1) $
        [ (map.map) (move R 10) letterM
        , (map.map) (move R 22) letterU
        , (map.map) (move R 34) letterN
        , (map.map) (move R 46) letterI

        ,  (map.map) (move R 10 . move DR 14) letterH
        ,  (map.map) (move R 22 . move DR 14) letterA
        ,  (map.map) (move R 34 . move DR 14) letterC

        , (map.map) (move R 10 . move DR 28) digit2
        , (map.map) (move R 22 . move DR 28) digit0
        , (map.map) (move R 34 . move DR 28) digit2
        , (map.map) (move R 46 . move DR 28) digit2

        ]


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

walkInSteps :: [hex -> hex] -> hex -> [hex]
walkInSteps [] _pos = []
walkInSteps (f:fs) pos =
    let newPoint = f pos
    in newPoint : walkInSteps fs newPoint
