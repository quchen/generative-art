module Main (main) where



import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (x, y, Glyph)

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
            let cellSize = 8
            C.translate 160 (fromIntegral picHeight/2)
            cairoScope $ do
                setLineWidth 2
                setLineJoin LineJoinBevel
                cartesianCoordinateSystem def
                renderWires purple cellSize lambdaCircuits
                renderMunihacWriting purple (cellSize/2)
                -- renderWires purple (cellSize/2) munihacWriting
    render "out/munihac-2022-logo.svg" picWidth picHeight mainRender
    render "out/munihac-2022-logo.png" picWidth picHeight $ do
        cairoScope $ do
            setSourceRGB 1 1 1
            paint
        mainRender
  where
    picWidth = 650
    picHeight = 400

newtype Glyph = Glyph [[Hex]]

mapGlyph :: (Hex -> Hex) -> Glyph -> Glyph
mapGlyph f (Glyph wires) = Glyph ((map.map) f wires)

renderMunihacWriting :: ColorScheme -> Double -> Render ()
renderMunihacWriting colorScheme cellSize = do
    let letterM = Glyph
            [ walkInSteps [id, move UL 8, move UR 2, move R 6, move DR 10] hexZero
            , walkInSteps [move UL 10 . move R 4, move DR 6] hexZero
            ]
        letterU = Glyph
            [ walkInSteps [move UL 10, move DR 10, move R 6, move UR 2, move UL 8] hexZero
            ]
        letterN = Glyph
            [ walkInSteps [id, move UL 8, move UR 2, move R 6, move DR 10] hexZero
            ]
        letterI = Glyph
            [ walkInSteps [id, move UL 10] hexZero
            ]
        letterH = Glyph
            [ walkInSteps [id, move UL 10] hexZero
            , walkInSteps [move UL 5, move R 7] hexZero
            , walkInSteps [move R 8, move UL 10] hexZero
            ]
        letterA = Glyph
            [ walkInSteps [id, move UL 8, move UR 2, move R 6, move DR 10] hexZero
            , walkInSteps [move UL 5, move R 7] hexZero
            ]
        letterC = Glyph
            [ walkInSteps [move R 8, move L 8, move UL 8, move UR 2, move R 6] hexZero
            ]
        digit2 = Glyph
            [ walkInSteps [move R 8, move L 8, move UL 4, move UR 1, move R 6, move UR 1, move UL 4, move L 8] hexZero
            ]
        digit0 = Glyph
            [ walkInSteps [id, move R 7, move UR 1, move UL 9, move L 7, move DL 1, move DR 8] hexZero
            ]

        -- all: (move DR 2 . move R 1)
        muni = map (mapGlyph (move DR 2 . move R 1))
            [ mapGlyph (move R 10) letterM
            , mapGlyph (move R 22) letterU
            , mapGlyph (move R 34) letterN
            , mapGlyph (move R 46) letterI
            ]
        hac = map (mapGlyph (move DR 2 . move R 1))
            [ mapGlyph (move R 10 . move DR 14) letterH
            , mapGlyph (move R 22 . move DR 14) letterA
            , mapGlyph (move R 34 . move DR 14) letterC ]

        x2022 = map (mapGlyph (move DR 2 . move R 1))
            [ mapGlyph (move R 10 . move DR 28) digit2
            , mapGlyph (move R 22 . move DR 28) digit0
            , mapGlyph (move R 34 . move DR 28) digit2
            , mapGlyph (move R 46 . move DR 28) digit2
            ]

    let ColorScheme colors = colorScheme
    cairoScope $ do
        colors V.! 2
        for_ muni $ \(Glyph wires) -> for_ wires $ \wire -> renderWire cellSize wire
    cairoScope $ do
        colors V.! 1
        for_ hac $ \(Glyph wires) -> for_ wires $ \wire -> renderWire cellSize wire
    cairoScope $ do
        colors V.! 0
        for_ x2022 $ \(Glyph wires) -> for_ wires $ \wire -> renderWire cellSize wire


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
