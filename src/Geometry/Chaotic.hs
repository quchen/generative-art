{-# LANGUAGE BangPatterns #-}

-- | Functions that vary chaotically based on their input. Useful for introducing
-- deterministic noise in pure code, e.g. for slightly moving points around.
module Geometry.Chaotic where

import qualified System.Random as R
import Geometry
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST
import Data.STRef
import Data.Bits
import Data.Int


newtype ChaosSeed = ChaosSeed Int

instance R.RandomGen ChaosSeed where
    next (ChaosSeed g) = let (i, g') = R.next g in (i, ChaosSeed g')


makeSeed :: PerturbSeed a => a -> ChaosSeed
makeSeed x = runST $ do
    offset <- newSTRef 0
    vec <- VM.replicate 4 0
    perturb (TFSeedVec offset vec) x
    a <- VM.read vec 0
    b <- VM.read vec 1
    c <- VM.read vec 2
    d <- VM.read vec 3
    (pure . ChaosSeed . RTF.seedTFGen) (a,b,c,d)

-- | A seed vector to generate a 'TFGen' out of using 'RTF.seedTFGen'. In order to
-- cycle through the vector when adding multiple values, we also keep the write
-- offset.
data TFSeedVec s = TFSeedVec !(STRef s Word8) !(VM.STVector s Word64)

-- | Write a 'Word64' to the seed vector, incrementing the internal write location
-- such that multiple writes go to different locations.
write :: TFSeedVec s -> Word64 -> ST s ()
write (TFSeedVec offsetRef vec) val = do
    offset <- do
        oldOffset <- readSTRef offsetRef
        let !newOffset = (oldOffset+1) `mod` fromIntegral (VM.length vec)
        writeSTRef offsetRef newOffset
        pure oldOffset

    oldVal <- VM.read vec (fromIntegral offset)
    let !val' = oldVal `xor` val
    VM.write vec (fromIntegral offset) val'

class PerturbSeed a where
    perturb :: TFSeedVec s -> a -> ST s ()

-- | Perturb with 'Word64'-sized chunks recursively.
instance PerturbSeed Integer where
    perturb _ 0 = pure ()
    perturb vec i = do
        let (rest, chunk) = divMod i (fromIntegral (maxBound :: Word64))
        perturbIntegral vec chunk
        perturb vec rest

-- | Perturb using any 'Integral' value. Useful to write instance boilerplate.
perturbIntegral :: Integral a => TFSeedVec s -> a -> ST s ()
perturbIntegral vec i = write vec (fromIntegral i)

instance PerturbSeed Int where perturb = perturbIntegral
instance PerturbSeed Int8 where perturb = perturbIntegral
instance PerturbSeed Int16 where perturb = perturbIntegral
instance PerturbSeed Int32 where perturb = perturbIntegral
instance PerturbSeed Int64 where perturb = perturbIntegral
instance PerturbSeed Word8 where perturb = perturbIntegral
instance PerturbSeed Word16 where perturb = perturbIntegral
instance PerturbSeed Word32 where perturb = perturbIntegral
instance PerturbSeed Word64 where perturb = perturbIntegral



--
-- -- | Create a 'ChaosSeed' value out of some data, useful for generating random but
-- -- determinstic functions for interesting test data.
-- class MakeChaosSeed a where
--     makeChaosSeed :: a -> ChaosSeed
--
-- instance MakeChaosSeed ChaosSeed where
--     makeChaosSeed = id
--
-- instance (MakeChaosSeed a, MakeChaosSeed b) => MakeChaosSeed (a, b) where
--     makeChaosSeed (a,b) = makeChaosSeed [makeChaosSeed a, makeChaosSeed b]
--
-- instance (MakeChaosSeed a, MakeChaosSeed b, MakeChaosSeed c) => MakeChaosSeed (a, b, c) where
--     makeChaosSeed (a, b, c) = makeChaosSeed [makeChaosSeed a, makeChaosSeed b, makeChaosSeed c]
--
-- instance (MakeChaosSeed a, MakeChaosSeed b, MakeChaosSeed c, MakeChaosSeed d) => MakeChaosSeed (a, b, c, d) where
--     makeChaosSeed (a, b, c, d) = makeChaosSeed [makeChaosSeed a, makeChaosSeed b, makeChaosSeed c, makeChaosSeed d]
--
-- instance (MakeChaosSeed a, MakeChaosSeed b, MakeChaosSeed c, MakeChaosSeed d) => MakeChaosSeed (a, b, c, d, e) where
--     makeChaosSeed (a, b, c, d, e) = makeChaosSeed [makeChaosSeed a, makeChaosSeed b, makeChaosSeed c, makeChaosSeed d, makeChaosSeed e]
--
-- instance MakeChaosSeed Int where
--     makeChaosSeed = ChaosSeed . RTF.mkTFGen
--
-- instance MakeChaosSeed Integer where
--     makeChaosSeed i = makeChaosSeed (fromInteger i :: Int)
--
-- instance MakeChaosSeed Float where
--     makeChaosSeed = makeChaosSeed . decodeFloat
--
-- instance MakeChaosSeed Double where
--     makeChaosSeed = makeChaosSeed . decodeFloat
--
-- instance MakeChaosSeed Vec2 where
--     makeChaosSeed (Vec2 x y) = makeChaosSeed [x, y]
--
-- instance MakeChaosSeed Line where
--     makeChaosSeed (Line (Vec2 x1 y1) (Vec2 x2 y2))
--       = makeChaosSeed [makeChaosSeed x1, makeChaosSeed x2, makeChaosSeed y1, makeChaosSeed y2]
--
-- instance MakeChaosSeed a => MakeChaosSeed [a] where
--     makeChaosSeed xs = ChaosSeed (sum (zipWith (\(ChaosSeed s) n -> s^n) (map makeChaosSeed xs) [1..]))
--
-- instance MakeChaosSeed Polygon where
--     makeChaosSeed (Polygon corners) = makeChaosSeed corners
--
-- gaussianVecs :: ChaosSeed -> [Vec2]
-- gaussianVecs (ChaosSeed seed)
--   = let go (u1:rest)
--             | u1 <= 0 = go rest -- to avoid diverging on log(0)
--         go (u1:u2:rest)
--             = let root1 = sqrt (-2 * log u1)
--                   pi2u2 = 2 * pi * u2
--                   x = root1 * cos pi2u2
--                   y = root1 * sin pi2u2
--               in Vec2 x y : go rest
--         go _ = error "Can’t happen, input is infinite"
--     in go (randomRs (0, 1) (mkStdGen seed))
