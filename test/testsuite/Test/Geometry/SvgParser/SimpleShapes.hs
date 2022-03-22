{-# LANGUAGE OverloadedStrings #-}

module Test.Geometry.SvgParser.SimpleShapes (tests) where



import           Data.Set  (Set)
import qualified Data.Set  as S
import           Data.Text (Text)
import qualified Data.Text as T

import Geometry
import Geometry.SvgParser.SimpleShapes

import Test.TastyAll



tests :: TestTree
tests = testGroup "SVG simple shape parser"
    [ testCase "Line" $ assertParsesToShape "LINE x1=0 y1=1 x2=2 y2=3" (SvgLine (Line (Vec2 0 1) (Vec2 2 3)))
    , testCase "Circle" $ assertParsesToShape "CIRCLE cx=0 cy=1 r=2" (SvgCircle (Circle (Vec2 0 1) 2))
    , testCase "Ellipse" $ assertParsesToShape "ELLIPSE cx=0 cy=1 rx=2 ry=3" (SvgEllipse (Ellipse (translate (Vec2 0 1) <> scale' 2 3)))
    ]

assertParsesToShape :: Text -> SimpleShape -> Assertion
assertParsesToShape path expected = case parse path of
    Right actual -> assertApproxEqual "" (ExpectedWithin 1e-10 expected) (Actual actual)
    Left err -> assertFailure ("Parse error: " ++ T.unpack err)
