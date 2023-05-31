module System.Random.MWC.Extended (
      module System.Random.MWC
    , withRng
) where



import           Control.Monad.Primitive
import qualified Data.Vector             as V
import           System.Random.MWC



-- | Convenience function to generate a random number 'Gen'erator.
withRng :: PrimMonad m => Integral seed => [seed] -> (Gen (PrimState m) -> m a) -> m a
withRng seed actions = initialize (fmap fromIntegral (V.fromList seed)) >>= actions
