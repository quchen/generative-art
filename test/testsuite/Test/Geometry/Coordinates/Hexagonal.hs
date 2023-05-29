module Test.Geometry.Coordinates.Hexagonal (tests) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import qualified Data.Map                        as M
import qualified Data.Vector                     as V
import           Graphics.Rendering.Cairo        as C hiding (x, y)
import           System.Random.MWC
import           System.Random.MWC.Distributions

import Draw
import Geometry                       as G
import Geometry.Coordinates.Hexagonal as Hex
import Numerics.Interpolation

import Test.TastyAll



tests :: TestTree
tests = testGroup "Hexagonal coordinate system"
    [ testGroup "Properties"
        [ rotationDirectionTest
        , rotatingIsCyclicTest
        , rotatingBackAndForthTest
        , rotatingAddsUpTest
        ]
    ]

rotationDirectionTest :: TestTree
rotationDirectionTest = testCase "Rotation direction is correct" $ do
    let center = hexZero
        point = move R 5 center
        pointRotated = Hex.rotateAround center 1 point

        Vec2 _ yOriginal = toVec2 1 point
        Vec2 _ yRotated  = toVec2 1 pointRotated
    assertBool "Rotating by positive amount should move example point downward"
        (yRotated > yOriginal)

rotatingIsCyclicTest :: TestTree
rotatingIsCyclicTest = testProperty "rotate (6*x) == id" $
    \center point angle ->
        point == Hex.rotateAround center (6*angle) point

rotatingBackAndForthTest :: TestTree
rotatingBackAndForthTest = testProperty "Rotating back and forth does nothing" $
    \center point angle ->
        point == Hex.rotateAround center angle (Hex.rotateAround center (-angle) point)

rotatingAddsUpTest :: TestTree
rotatingAddsUpTest = testProperty "rotate x . rotate y == rotate (x+y)" $
    \center point angle1 angle2 ->
        counterexample
            (unlines
                [ "rotateAround (" ++ show center ++ ") " ++ show (angle1 + angle2)
                , "/="
                , "rotateAround (" ++ show center ++ ") " ++ show angle1 ++" . rotateAround (" ++ show center ++ ") " ++ show angle2
                ])
            ((Hex.rotateAround center angle1 . Hex.rotateAround center angle2) point
            ==
            Hex.rotateAround center (angle1+angle2) point)

lineAndCircle :: TestTree
lineAndCircle = testVisual "Line and circle" 380 350 "docs/hexagonal/1_line_and_circle" $ \(w, h) -> do
    let sideLength = 30
    C.translate (fromIntegral w/2) (fromIntegral h/2)
    cairoScope $ do
        setFontSize 8
        hexagonalCoordinateSystem sideLength 3

    for_ (Hex.line (Hex (-1) 0 1) (Hex 3 (-2) (-1))) $ \hexLineSegment -> cairoScope $ do
        Draw.sketch (hexagonPoly sideLength hexLineSegment)
        setColor (mathematica97 0 `withOpacity` 0.3)
        fillPreserve
        setColor (mathematica97 0 `withOpacity` 0.5)
        stroke

    for_ (ring 2 (Hex (-1) 1 0)) $ \hex -> cairoScope $ do
        Draw.sketch (hexagonPoly sideLength hex)
        setColor (mathematica97 1 `withOpacity` 0.3)
        fillPreserve
        setColor (mathematica97 1 `withOpacity` 0.5)
        stroke
