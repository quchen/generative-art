module Test.Geometry.Core (tests) where



import Geometry.Core
import Data.Ord

import Test.TastyAll


tests :: TestTree
tests = testGroup "Core.hs"
    [ test_pseudoAngle
    , testGroup "Subdividing lines"
        [ test_subdivideLine
        , test_subdivideLineByLength
        ]
    ]

test_pseudoAngle :: TestTree
test_pseudoAngle = testProperty "pseudoAngle" $ \(NonZeroVec2 p) (NonZeroVec2 q) ->
    let atan2Vec (Vec2 x y) = atan2 y x
    in comparing pseudoAngle p q === comparing atan2Vec p q

test_subdivideLine :: TestTree
test_subdivideLine = testGroup "subdivideLine"
    [ testProperty "Number of segments is correct" $ forAll gen $
        \(Line start end, segments, subdivided) ->
            start /= end ==> segments+1 === length subdivided -- +1 because for N segments, we have N+1 points in the result
    , testProperty "Sum of segment lengths is original length" $ forAll gen $
        \(line, segments, subdivided) ->
            let expectedLength = lineLength line
                sumOfSegmentsLength = polylineLength (Polyline subdivided)
            in
                counterexample
                (unlines ["Counterexample:"
                    , "  Line:                " ++ show line
                    , "  Segments:            " ++ show segments
                    , "  Subdivided:          " ++ show subdivided
                    , "  Line length:         " ++ show expectedLength
                    , "  Sum of subdivisions: " ++ show sumOfSegmentsLength
                    ])
                (expectedLength ~== sumOfSegmentsLength)
    , testProperty "Start and end points are contained exactly" $ forAll gen $
        \(line@(Line start end), _segments, subdivided) ->
            let start' = head subdivided
                end' = last subdivided
            in counterexample
                (unlines ["Start point mismatched", "Line: " ++ show line, "Subdivided: " ++ show subdivided ])
                (start == start')
                .&&.
            counterexample
                (unlines ["End point mismatched", "Line: " ++ show line, "Subdivided: " ++ show subdivided ])
                (end == end')
    ]

  where
    gen = do
        start <- arbitrary
        end <- arbitrary
        segments <- choose (1, 100)
        let line = Line start end
            subdivided = subdivideLine segments line
        pure (line, segments, subdivided)

test_subdivideLineByLength :: TestTree
test_subdivideLineByLength = testGroup "subdivideLineByLength"
    [ testProperty "Sum of segment lengths is original length" $ forAll gen $
        \(line, maxLength, subdivided) ->
            let expectedLength = lineLength line
                sumOfSegmentsLength = polylineLength (Polyline subdivided)
            in
                counterexample
                (unlines ["Counterexample:"
                    , "  Line:                " ++ show line ++ ", maxLength: " ++ show maxLength
                    , "  Subdivided:          " ++ show subdivided
                    , "  Line length:         " ++ show expectedLength
                    , "  Sum of subdivisions: " ++ show sumOfSegmentsLength
                    ])
                (expectedLength ~== sumOfSegmentsLength)
    , testProperty "Start and end points are contained exactly" $ forAll gen $
        \(line@(Line start end), _maxLength, subdivided) ->
            let start' = head subdivided
                end' = last subdivided
            in counterexample
                (unlines ["Start point mismatched", "Line: " ++ show line, "Subdivided: " ++ show subdivided ])
                (start == start')
                .&&.
            counterexample
                (unlines ["End point mismatched", "Line: " ++ show line, "Subdivided: " ++ show subdivided ])
                (end == end')
    ]

  where
    gen = do
        start <- arbitrary
        end <- arbitrary
        maxLength <- choose (1, 100)
        let line = Line start end
            subdivided = subdivideLineByLength maxLength line
        pure (line, maxLength, subdivided)
