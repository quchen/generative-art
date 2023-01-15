module Geometry.Algorithms.Sampling.Vogel where



import Geometry.Core



data VogelSamplingParams = VogelSamplingParams
    { _vogelCenter :: Vec2
    -- ^ Center of the point cloud
    , _vogelDensity :: Double
    -- ^ Density (samples per unit area)
    , _vogelRadius :: Double
    -- ^ Radius of the point cloud
    }

-- | This sampling method produces quite evenly spaced points, but unlike
--   randomized sampling methods such as Poisson-Disc, it also exposes a
--   distinctive spiral structure that can result in interesting patterns.
--
-- The concept of using a Vogel spiral for sampling is inspired by
-- https://bugsmapsandmath.com/2017/03/01/field-sampling-on-a-fibonacci-spiral/
--
-- The nomenclature is not entirely clear:
-- * It looks like this particular spiral of points spaced by a golden angle is
--   called a Vogel spiral, although I could not find much documentation about
--   it.
-- * The underlying concept seems to be a Fermat spiral, i.e. a spiral where
--   the radius grows like the square root of the angle. So I suppose one could
--   say, the Vogel spiral is obtained by sampling points on a Fermat spiral
--   with an angular spacing of the golden angle.
-- * To complicate matters further, the Vogel spiral seems to be sometimes
--   referred to as a Fibonacci spiral (including the link above that inspired
--   me), but a Fibonacci spiral is actually a different thing.
vogel :: VogelSamplingParams -> [Vec2]
vogel VogelSamplingParams{..} =
    [ _vogelCenter +. polar theta r
    | i <- [0 .. _vogelRadius^2 * pi * _vogelDensity]
    , let r = sqrt (i / pi / _vogelDensity)
    , let theta = rad (2*pi*i/phi)
    ]
  where phi = (1 + sqrt 5) / 2
