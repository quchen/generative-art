module Test.Draw (tests) where



import Draw

import Test.TastyAll



tests :: TestTree
tests = testGroup "Draw"
    [ testProperty "fromCairoMatrix/toCairoMatrix are inverses" $ \trafo ->
        (fromCairoMatrix . toCairoMatrix) trafo ~=== trafo
    ]
