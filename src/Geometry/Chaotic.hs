{-# LANGUAGE BangPatterns #-}

-- | Functions that vary chaotically based on their input. Useful for introducing
-- deterministic noise in pure code, e.g. for slightly moving points around.
module Geometry.Chaotic (
    -- * RNG functions
      ChaosSource(..)
    , stdGen

    -- * Utilities
    , normals
    , gaussian
    , normalVecs
    , gaussianVecs
) where

import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Geometry
import qualified System.Random as R


-- | Types that can be turned into a random number generator easily, to yield pure chaotic output.
class ChaosSource a where
    -- | Add a value to the mix the 'R.StdGen' will be created from. Only used
    -- for writing new instances of 'ChaosSource'.
    --
    -- To use instances of this class, use 'stdGen'.
    perturb :: a -> Int

-- | Mix another 'Int' into the chaos source.
stir :: ChaosSource a => a -> Int -> Int
stir !x !y = perturb x `xor` Data.Bits.rotate 23 y
    -- 23 is prime so it’ll misalign a lot hence should provide decent mixing.
    -- This is very much not something I have thought about, it’s maybe best not
    -- to base anything but wiggly pictures on it.

-- | Create a 'R.StdGen' which can be used with "System.Random"’s functions,
-- based on a variety of inputs.
stdGen :: ChaosSource a => a -> R.StdGen
stdGen = R.mkStdGen . perturb

instance ChaosSource Integer where
    perturb = go 0
      where
        go !acc 0 = acc
        go acc i =
            let (rest, chunk) = quotRem i (fromIntegral (maxBound :: Word))
            in go (acc `stir` fromIntegral chunk) rest

-- | Perturb using an 'Integral' value by mapping it to an 'Int'. It’s basically
-- 'fromIntegral' and does very little the (seed!) randomness is introduced by
-- 'stir' when combining different values.
perturbIntegral :: Integral a => a -> Int
perturbIntegral x = perturb (fromIntegral x :: Int)

instance ChaosSource Int where perturb = id
instance ChaosSource Int8 where perturb = perturbIntegral
instance ChaosSource Int16 where perturb = perturbIntegral
instance ChaosSource Int32 where perturb = perturbIntegral
instance ChaosSource Int64 where perturb = perturbIntegral
instance ChaosSource Word8 where perturb = perturbIntegral
instance ChaosSource Word16 where perturb = perturbIntegral
instance ChaosSource Word32 where perturb = perturbIntegral
instance ChaosSource Word64 where perturb = perturbIntegral

instance (ChaosSource a, ChaosSource b) => ChaosSource (a, b) where
    perturb (a,b) = perturb a `stir` perturb b

instance (ChaosSource a, ChaosSource b, ChaosSource c) => ChaosSource (a, b, c) where
    perturb (a, b, c) = perturb a `stir` perturb b `stir` perturb c

instance (ChaosSource a, ChaosSource b, ChaosSource c, ChaosSource d) => ChaosSource (a, b, c, d) where
    perturb (a, b, c, d) = perturb a `stir` perturb b `stir` perturb c `stir` perturb d

instance (ChaosSource a, ChaosSource b, ChaosSource c, ChaosSource d, ChaosSource e) => ChaosSource (a, b, c, d, e) where
    perturb (a, b, c, d, e) = perturb a `stir` perturb b `stir` perturb c `stir` perturb d `stir` perturb e

instance ChaosSource Float where
    perturb = perturb . decodeFloat

instance ChaosSource Double where
    perturb = perturb . decodeFloat

instance ChaosSource Vec2 where
    perturb (Vec2 x y) = perturb (x,y)

instance ChaosSource Line where
    perturb (Line start end) = perturb (start, end)

instance ChaosSource a => ChaosSource [a] where
    perturb = foldl' (\acc x -> perturb x `stir` acc) 0

instance ChaosSource Polygon where
    perturb (Polygon corners) = perturb corners

instance ChaosSource vec => ChaosSource (Bezier vec) where
    perturb (Bezier a b c d) = perturb (a,b,c,d)

-- | Infinite list of normally distributed values.
normals :: ChaosSource seed => seed -> [Double]
normals seed
  = let go (u1:rest)
            | u1 <= 0 = go rest -- to avoid diverging on log(0)
        go (u1:u2:rest)
            = let root1 = sqrt (-2 * log u1)
                  pi2u2 = 2 * pi * u2
                  x = root1 * cos pi2u2
                  y = root1 * sin pi2u2
              in x : y : go rest
        go _ = bugError "Can’t happen, input is infinite"
    in go (R.randomRs (0, 1) (stdGen seed))

-- | Infinite list of Gaussian distributed values.
gaussian
    :: ChaosSource seed
    => Double -- ^ Mean
    -> Double -- ^ Standard deviation
    -> seed
    -> [Double]
gaussian mu sigma seed = [sigma*x + mu | x <- normals seed]

vecPair :: ChaosSource seed => (seed -> [Double]) -> seed -> [Vec2]
vecPair f seed = go (f seed)
  where
    go (x:y:rest) = Vec2 x y : go rest
    go _ = bugError "Can’t happen, input is infinite"

normalVecs :: ChaosSource seed => seed -> [Vec2]
normalVecs = vecPair normals

gaussianVecs :: ChaosSource seed => Double -> Double -> seed -> [Vec2]
gaussianVecs mu sigma = vecPair (gaussian mu sigma)
