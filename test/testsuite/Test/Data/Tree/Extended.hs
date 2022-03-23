module Test.Data.Tree.Extended (tests) where



import Data.Tree.Extended
import Test.TastyAll



tests :: TestTree
tests = testGroup "Data.Tree extensions"
    [ testGroup "Smoke tests"
        [ breadthFirstSmokeTest
        , depthFirstSmokeTest
        ]
    ]

smokeTestTree :: Tree String
smokeTestTree = Node "root"
    [ Node "left"
        [ Node "left left" []
        , Node "left right" []
        ]
    , Node "right"
        [ Node "right left" []
        , Node "right right" []
        ]
    ]

breadthFirstSmokeTest :: TestTree
breadthFirstSmokeTest = testCase "Breadth first" $
    let expected = ["root", "left", "right", "left left", "left right", "right left", "right right"]
        actual = breadthFirst smokeTestTree
    in assertEqual "" (Expected expected) (Actual actual)

depthFirstSmokeTest :: TestTree
depthFirstSmokeTest = testCase "Depth first" $
    let expected = ["root", "left", "left left", "left right", "right", "right left", "right right"]
        actual = depthFirst smokeTestTree
    in assertEqual "" (Expected expected) (Actual actual)
