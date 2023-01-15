module Geometry.Algorithms.Sampling.Vogel where



import Geometry.Core



data VogelSamplingParams = VogelSamplingParams
    { _vogelCenter :: Vec2
    -- ^ Center of the distribution
    , _vogelDensity :: Double
    -- ^ Density, measured in (approximate) points per unit square
    , _vogelRadius :: Double
    }

vogel :: VogelSamplingParams -> [Vec2]
vogel VogelSamplingParams{..} =
    [ _vogelCenter +. polar theta r
    | i <- [0 .. _vogelRadius^2 * pi * _vogelDensity]
    , let r = sqrt (i / pi / _vogelDensity)
    , let theta = rad (2*pi*i/phi)
    ]
  where phi = (1 + sqrt 5) / 2

  

  -- N = r^2 * pi * density
  -- A = r^2 * pi
