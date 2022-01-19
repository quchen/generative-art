module Test.Geometry.Coordinates.Hexagonal (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C hiding (x, y)

import Draw
import Geometry                       as G
import Geometry.Coordinates.Hexagonal as Hex
import Geometry.Shapes

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Hexagonal coordinate system"
    [ lineAndCircle
    ]



hexagonSketch :: Double -> Vec2 -> Render ()
hexagonSketch sideLength center = polygonSketch (G.transform (G.translate center <> G.scale sideLength <> G.rotate (deg 30)) regularPolygon 6)

lineAndCircle :: TestTree
lineAndCircle = testCase "Line and circle" (renderAllFormats 380 350 "docs/hexagonal/1_line_and_circle" (drawing 30 380 350))

drawing :: Double -> Int -> Int -> Render ()
drawing sideLength w h = do
    C.translate (fromIntegral w/2) (fromIntegral h/2)
    cairoScope $ do
        setFontSize 8
        hexagonalCoordinateSystem sideLength 3

    for_ (Hex.line (Cube (-1) 0 1) (Cube 3 (-2) (-1))) $ \hexLineSegment -> cairoScope $ do
        hexagonSketch sideLength (toVec2 sideLength hexLineSegment)
        setColor (mmaColor 0 0.3)
        fillPreserve
        setColor (mmaColor 0 0.5)
        stroke

    for_ (ring 2 (Axial (-1) 1)) $ \hex -> cairoScope $ do
        hexagonSketch sideLength (toVec2 sideLength hex)
        setColor (mmaColor 1 0.3)
        fillPreserve
        setColor (mmaColor 1 0.5)
        stroke
