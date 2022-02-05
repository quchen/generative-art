module Test.TastyAll (
    assertThrowsError
    , module Test.Tasty
    , module Test.Tasty.QuickCheck
    , module Test.Tasty.HUnit
    , module Test.Helpers
    , module Test.Common
) where



import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Helpers
import Test.Common
import Control.Exception



assertThrowsError :: (String -> Bool) -> a -> IO ()
assertThrowsError p x = handle
    (\(ErrorCallWithLocation err _loc) -> if p err
        then pure ()
        else assertFailure ("An ErrorCall was raised, but not with the right contents. Received: " ++ err)
        )
    (evaluate x >> assertFailure "Expected ErrorCall, but none was raised")
