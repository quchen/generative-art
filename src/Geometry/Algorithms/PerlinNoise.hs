module Geometry.Algorithms.PerlinNoise (
      PerlinParameters(..)
    , perlin1
    , perlin2
    , perlin3
) where



import           Data.Default.Class
import           Data.Maybe
import           Geometry.Core
import qualified Math.Noise         as Perlin



-- | Named arguments for the @perlin*@ functions.
data PerlinParameters = PerlinParameters
    { _perlinFrequency :: Double
        -- ^ Frequency of the first octave, e.g. \(\frac1{2\text{width}}\) to span the whole width of the picture.
        --   'def'ault: 1.

    , _perlinLacunarity :: Double
        -- ^ Amplitude factor between octaves.
        --   'def'ault: 2.

    , _perlinOctaves :: Int
        -- ^ Number of octaves to generate.
        --   'def'ault: 6.

    , _perlinPersistence :: Double
        -- ^ Amplitude multiplier between octaves.
        --   'def'ault: 0.5.

    , _perlinSeed :: Int
        -- ^ RNG seed.
    } deriving (Eq, Ord, Show)

instance Default PerlinParameters where
    def = PerlinParameters
        { _perlinFrequency   = 1
        , _perlinLacunarity  = 2
        , _perlinOctaves     = 6
        , _perlinPersistence = 0.5
        , _perlinSeed        = 0
        }

marshalPP :: PerlinParameters -> Perlin.Perlin
marshalPP PerlinParameters{..} = Perlin.Perlin
    { perlinFrequency   = _perlinFrequency
    , perlinLacunarity  = _perlinLacunarity
    , perlinOctaves     = _perlinOctaves
    , perlinPersistence = _perlinPersistence
    , perlinSeed        = _perlinSeed
    }

-- | One-dimensional Perlin noise.
perlin1 :: PerlinParameters -> Double -> Double
perlin1 params x = fromMaybe 0 (Perlin.getValue (marshalPP params) (x, 0, 0))

-- | Two-dimensional Perlin noise.
perlin2 :: PerlinParameters -> Vec2 -> Double
perlin2 params (Vec2 x y) = fromMaybe 0 (Perlin.getValue (marshalPP params) (x, y, 0))

-- | Three-dimensional Perlin noise.
perlin3 :: PerlinParameters -> Double -> Double -> Double -> Double
perlin3 params x y z = fromMaybe 0 (Perlin.getValue (marshalPP params) (x, y, z))
