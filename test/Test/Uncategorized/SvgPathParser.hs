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
    [ testGroup "Successful parses"
        [ testCase "Absolute lines; M, L" $ do
            let path = "M 1 1 L 10 10"
                expected = S.singleton (pairUpLines [Vec2 1 1, Vec2 10 10])
            assertParsesToPath path expected
        , testCase "Relative lines; M, l" $ do
            let path = "M 1 1 l 1 1"
                expected = S.singleton (pairUpLines [Vec2 1 1, Vec2 2 2])
            assertParsesToPath path expected
        , testCase "Relative movement; m, l" $ do
            let path = "m 1 1 l 1 1"
                expected = S.singleton (pairUpLines [Vec2 1 1, Vec2 2 2])
            assertParsesToPath path expected
        , testCase "Two movements; M, m, l" $ do
            let path = "M 10 10 m 0 1 l 1 0"
                expected = S.fromList [[], pairUpLines [Vec2 10 11, Vec2 11 11]]
            assertParsesToPath path expected
        , testCase "Absolute lines, closed; M, L, Z" $ do
            let path = "M 1 1 L 5 5 Z"
                expected = S.singleton (pairUpLines [Vec2 1 1, Vec2 5 5, Vec2 1 1])
            assertParsesToPath path expected
        , testCase "Multiple paths at the same time; mM, lL, Z" $ do
            let path = T.unlines
                    [ "M 1 1 L 10 1 Z"
                    , "m 0 1 l 10 1 "
                    ]
                expected = S.fromList
                    [ [ Left (Line (Vec2 1  1) (Vec2 10 1))
                    , Left (Line (Vec2 10 1) (Vec2 1  1)) ]

                    , [ Left (Line (Vec2 1  2) (Vec2 11 3)) ]
                    ]
            assertParsesToPath path expected
        , testCase "Bezier; M, C" $ do
            let path = "M 1 1 C 1 2 1 3 1 4"
                expected = [Right (Bezier (Vec2 1 1) (Vec2 1 2) (Vec2 1 3) (Vec2 1 4))]
            assertParsesToPath path (S.singleton expected)
        ]
    , testGroup "Parse failures"
        [ testCase "Only close; Z"
            (assertFailsToParse "Z" "unexpected 'Z'")
        , testCase "Double close; Z"
            (assertFailsToParse "M 0 0 Z Z" "unexpected 'Z'")
        , testCase "Incomplete Bezier; M, C"
            (assertFailsToParse "M 0 0 C 1 2 3 4 5" "unexpected end of input")
        ]
    ]

assertParsesToPath :: Text -> Set [Either Line Bezier] -> Assertion
assertParsesToPath path expected = case parse path of
    Right actual -> assertEqual "" expected (S.fromList actual)
    Left err -> assertFailure ("Parse error: " ++ T.unpack err)

assertFailsToParse :: Text -> Text -> Assertion
assertFailsToParse path expectedSubstr = case parse path of
    Right actual -> assertFailure ("This should have been a parse error instead of " ++ show actual)
    Left err
        | not (null (T.breakOnAll expectedSubstr err)) -> pure ()
        | otherwise -> assertFailure (unlines
            [ "There was an error, but it did not contain the expected section:"
            , T.unpack (indent expectedSubstr)
            , "What we got was:"
            , T.unpack (indent err)
            ])

indent :: Text -> Text
indent = T.intercalate "\n" . map ("    > " <>) . T.splitOn "\n"
