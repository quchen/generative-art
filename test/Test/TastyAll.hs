module Test.TastyAll (
      assertThrowsError

    , Expected(..)
    , Actual(..)
    , assertEqual

    , module Test.Tasty
    , module Test.Tasty.QuickCheck
    , module Test.Tasty.HUnit
    , module Test.Helpers
    , module Test.Common
) where



import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit hiding (assertEqual)
import qualified Test.Tasty.HUnit as HUnit
import Test.Helpers
import Test.Common
import Control.Exception



assertThrowsError
    :: (String -> Bool) -- ^ Predicate on the error message (e.g. check for substring)
    -> a                -- ^ Value whose WHNF raises an 'error'
    -> IO ()            -- ^ Succeeds iff the value raises an 'error' with a certain content
assertThrowsError p x = handle
    (\(ErrorCallWithLocation err _loc) -> if p err
        then pure ()
        else assertFailure ("An ErrorCall was raised, but not with the right contents. Received: " ++ err)
        )
    (evaluate x >> assertFailure "Expected ErrorCall, but none was raised")

-- | Type-safe wrapper for expected values.
newtype Expected a = Expected a
    deriving (Eq, Ord)

instance Show a => Show (Expected a) where
    show (Expected x) = "Expected: " ++ show x

-- | Type-safe wrapper for actual values.
newtype Actual a = Actual a
    deriving (Eq, Ord)

instance Show a => Show (Actual a) where
    show (Actual x) = "Actual: " ++ show x

-- | Type-safe version of 'HUnit.assertEqual', because it’s too easy to mix up the arguments.
assertEqual :: (Eq a, Show a) => String -> Expected a -> Actual a -> Assertion
assertEqual errMsg (Expected e) (Actual a) = HUnit.assertEqual errMsg e a
