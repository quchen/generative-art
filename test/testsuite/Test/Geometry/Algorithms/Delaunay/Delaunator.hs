module Test.Geometry.Algorithms.Delaunay.Delaunator (tests) where



import           Control.Monad.IO.Class
import qualified Graphics.Rendering.Cairo as C
import qualified System.Random.MWC        as MWC

import qualified Draw                         as D
import           Geometry
import           Geometry.Algorithms.Delaunay.Delaunator

import Test.TastyAll



tests :: TestTree
tests = testGroup "Delaunator"
    [ test_orient
    , test_circum_x
    ]

test_orient :: TestTree
test_orient = testGroup "orient"
    [ testCase "Screen coordinates: counterclockwise => >0" $ do
        let a = Vec2 0 0
            b = Vec2 10 0
            p = Vec2 5 (-5) -- To the left of a---b
            actual = orient a b p
        assertBool "Clockwise: positive" (actual > 0)
    , testCase "Screen coordinates: clockwise => <0" $ do
        let a = Vec2 0 0
            b = Vec2 10 0
            p = Vec2 5 5 -- To the right of a---b
            actual = orient a b p
        assertBool "Couter-clockwise: negative" (actual < 0)
    ]

test_circum_x :: TestTree
test_circum_x = testGroup "Circum* functions"
    [ testCase "Circumdelta" $ do
        let actual = Actual (circumdelta a b c)
            expected = ExpectedWithin 1e-10 (Vec2 50 50)
        assertApproxEqual "" expected actual
    , testCase "circumRadiusSquare" $ do
        let arbitraryOffset = Vec2 12 34
            actual = Actual $ circumradiusSquare (a+.arbitraryOffset) (b+.arbitraryOffset) (c+.arbitraryOffset)
            expected = ExpectedWithin 1e-10 (normSquare (Vec2 50 50))
        assertApproxEqual "circumradius²" expected actual
    , testCase "circumcenter" $ do
        let arbitraryOffset = Vec2 12 34
            actual = Actual $ circumcenter (a+.arbitraryOffset) (b+.arbitraryOffset) (c+.arbitraryOffset)
            expected = ExpectedWithin 1e-10 (Vec2 50 50 +. arbitraryOffset)
        assertApproxEqual "circumcenter" expected actual
    ]
  where
        --  (0,0)            (100,0)
        --  *-----------------*
        --  |             __/
        --  |          __/
        --  |       _X/
        --  |    __/
        --  | __/
        --  |/
        --  * (0,100)
    a = Vec2 0 0
    b = Vec2 100 0
    c = Vec2 0 100
