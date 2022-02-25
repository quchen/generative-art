-- We can’t provide a type sig for 'initializeMwc' without adding another explicit
-- dependency, so we disable warnings for this module.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Functions that vary chaotically based on their input. Useful for introducing
-- deterministic noise in pure code, e.g. for slightly moving points around, in the
-- middle of pure code.
module Geometry.Chaotic (
    -- * MWC-Random chaos
      MwcChaosSource(..)
    , initializeMwc

    -- * STDGen chaos
    , ChaosSource(..)
    , stdGen

    -- ** Utilities
    , normals
    , gaussian
    , normalVecs
    , gaussianVecs
) where

import           Data.Bits
import           Data.Char
import           Data.Foldable
import           Data.Int
import qualified Data.Vector       as V
import           Data.Word
import qualified System.Random     as R
import qualified System.Random.MWC as MWC

import           Geometry
import qualified Geometry.Coordinates.Hexagonal as Hex



-- | Create a 'Word32' to initialize an MWC gen with. This is meant for simple and
-- convenient seed creation within pure code. Note that creating a generator to
-- produce a single value is probably much less efficient than using a worse
-- generator that is faster to seed.
class MwcChaosSource a where
    mwcChaos :: a -> Word32

instance MwcChaosSource Integer where
    mwcChaos = go 0
      where
        go :: Word32 -> Integer -> Word32
        go !acc 0 = acc
        go acc n | n < 0 = go (Data.Bits.rotate acc 17) (abs n)
        go acc n = let (d,m) = divMod n (fromIntegral (maxBound :: Word32))
                   in go (acc `xor` fromIntegral m) d

mwcChaosIntegral :: Integral a => a -> Word32
mwcChaosIntegral x = mwcChaos (fromIntegral x :: Integer)

instance MwcChaosSource Int where mwcChaos = mwcChaosIntegral
instance MwcChaosSource Int8 where mwcChaos = mwcChaosIntegral
instance MwcChaosSource Int16 where mwcChaos = mwcChaosIntegral
instance MwcChaosSource Int32 where mwcChaos = mwcChaosIntegral
instance MwcChaosSource Int64 where mwcChaos = mwcChaosIntegral
instance MwcChaosSource Word8 where mwcChaos = mwcChaosIntegral
instance MwcChaosSource Word16 where mwcChaos = mwcChaosIntegral
instance MwcChaosSource Word32 where mwcChaos = id
instance MwcChaosSource Word64 where mwcChaos = mwcChaosIntegral

instance MwcChaosSource () where
    mwcChaos _ = 0

instance (MwcChaosSource a, MwcChaosSource b) => MwcChaosSource (a, b) where
    mwcChaos (a,b) = mwcChaos [mwcChaos a, mwcChaos b]

instance (MwcChaosSource a, MwcChaosSource b, MwcChaosSource c) => MwcChaosSource (a, b, c) where
    mwcChaos (a, b, c) = mwcChaos [mwcChaos a, mwcChaos b, mwcChaos c]

instance (MwcChaosSource a, MwcChaosSource b, MwcChaosSource c, MwcChaosSource d) => MwcChaosSource (a, b, c, d) where
    mwcChaos (a, b, c, d) = mwcChaos [mwcChaos a, mwcChaos b, mwcChaos c, mwcChaos d]

instance (MwcChaosSource a, MwcChaosSource b, MwcChaosSource c, MwcChaosSource d, MwcChaosSource e) => MwcChaosSource (a, b, c, d, e) where
    mwcChaos (a, b, c, d, e) = mwcChaos [mwcChaos a, mwcChaos b, mwcChaos c, mwcChaos d, mwcChaos e]

instance MwcChaosSource Float where
    mwcChaos = mwcChaos . decodeFloat

instance MwcChaosSource Double where
    mwcChaos = mwcChaos . decodeFloat

instance MwcChaosSource Char where
    mwcChaos = mwcChaos . ord

instance MwcChaosSource Vec2 where
    mwcChaos (Vec2 x y) = mwcChaos (x,y)

instance MwcChaosSource Line where
    mwcChaos (Line start end) = mwcChaos (start, end)

instance MwcChaosSource a => MwcChaosSource [a] where
    mwcChaos = foldl' (\acc x -> x `xor` Data.Bits.rotate acc 23) 0 . map mwcChaos
    -- 23 is prime so it’ll misalign a lot hence should provide decent mixing. This
    -- is very much not something I have thought about too much, it’s maybe best
    -- not to base anything but wiggly pictures on it.

instance MwcChaosSource Polygon where
    mwcChaos (Polygon corners) = mwcChaos corners

instance MwcChaosSource Bezier where
    mwcChaos (Bezier a b c d) = mwcChaos (a,b,c,d)

instance MwcChaosSource BoundingBox where
    mwcChaos (BoundingBox a b) = mwcChaos (a, b)

instance MwcChaosSource Angle where
    mwcChaos = mwcChaos . getRad

instance MwcChaosSource Hex.Hex where
    mwcChaos (Hex.Hex q r s) = mwcChaos (q,r,s)

-- | Initialize an 'MWC.Gen' with anything 'MwcChaosSource'.
--
-- @
-- 'Control.Monad.runST' $ do
--     gen <- 'initializeMwc' ('Vec2' 3 3, 'Geometry.Shapes.regularPolygon' 3)
--     'MWC.randomRM' ('Vec2' 0 0, 'Vec2' 1 1) gen
-- @

-- No type signature as to not depend explicitly on more packages than necessary…
initializeMwc seed = MWC.initialize (V.singleton (mwcChaos seed))

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

instance ChaosSource () where
    perturb _ = 1

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

instance ChaosSource Char where
    perturb = perturb . ord

instance ChaosSource Vec2 where
    perturb (Vec2 x y) = perturb (x,y)

instance ChaosSource Line where
    perturb (Line start end) = perturb (start, end)

instance ChaosSource a => ChaosSource [a] where
    perturb = foldl' (\acc x -> perturb x `stir` acc) 0

instance ChaosSource Polygon where
    perturb (Polygon corners) = perturb corners

instance ChaosSource Bezier where
    perturb (Bezier a b c d) = perturb (a,b,c,d)

instance ChaosSource BoundingBox where
    perturb (BoundingBox a b) = perturb (a, b)

instance ChaosSource Angle where
    perturb = perturb . getRad

instance ChaosSource Hex.Hex where
    perturb (Hex.Hex q r s) = perturb (q,r,s)

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
