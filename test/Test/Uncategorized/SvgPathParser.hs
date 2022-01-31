{-# LANGUAGE OverloadedStrings #-}

module Test.Uncategorized.SvgPathParser (tests) where



import           Data.Set  (Set)
import qualified Data.Set  as S
import           Data.Text (Text)
import qualified Data.Text as T

import Geometry
import Geometry.SvgPathParser

import Test.Tasty
import Test.Tasty.HUnit



pairUpLines :: [Vec2] -> [Either Line a]
pairUpLines xs = zipWith (\a b -> Left (Line a b)) xs (tail xs)

tests :: TestTree
tests = testGroup "SVG path parser"
    [ testCase "Absolute lines; M, L" $ do
        let path = "M 1 1 L 1 10 L 10 10 L 10 1"
            expected = pairUpLines [Vec2 1 1, Vec2 1 10, Vec2 10 10, Vec2 10 1]
        assertParsesToPath path (S.singleton expected)
    , testCase "Absolute lines, closed: M, L, Z" $ do
        let path = "M 1 1 L 1 10 L 10 10 L 10 1 Z"
            expected = pairUpLines [Vec2 1 1, Vec2 1 10, Vec2 10 10, Vec2 10 1, Vec2 1 1]
        assertParsesToPath path (S.singleton expected)
    , testCase "Multiple paths at the same time; M, L, Z" $ do
        let path = T.unlines
                [ "M 1 1 L 1 10 L 10 10 L 10 1 Z"
                , "M 2 1 L 2 10 L 12 10 L 12 1 "
                ]
            expected = S.fromList
                [ [ Left (Line (Vec2 1   1) (Vec2  1 10))
                  , Left (Line (Vec2 1  10) (Vec2 10 10))
                  , Left (Line (Vec2 10 10) (Vec2 10  1))
                  , Left (Line (Vec2 10  1) (Vec2  1  1)) ]

                , [ Left (Line (Vec2  2  1) (Vec2  2 10))
                  , Left (Line (Vec2  2 10) (Vec2 12 10))
                  , Left (Line (Vec2 12 10) (Vec2 12  1)) ]
                ]
        assertParsesToPath path expected
    ]

assertParsesToPath :: Text -> Set [Either Line Bezier] -> Assertion
assertParsesToPath path expected = case parse path of
    Right actual -> assertEqual "" expected (S.fromList actual)
    Left err -> assertFailure ("Parse error: " ++ T.unpack err)
