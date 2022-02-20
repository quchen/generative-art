-- | Functions often used to design fields.
module Numerics.Functions (
    -- * Ramps
    --
    -- | Ramps vary from 0 to 1 over their ramping interval.
      logisticRamp
    , smoothstep

    -- * Falloffs and bumps
    --
    -- | Falloffs are bounded functions with \(f(0)=1\) that asymptotically
    --   reach 0. Bumps are falloffs that reach 0 in finite distance.
    , gaussianFalloff
    , logisticFalloff
    , cauchyFalloff
    , smoothBump
) where



import Numerics.Interpolation
import Data.Ord.Extended



-- | Logistic sigmoid function, varying smoothly from 0 to 1 along the real axis.
--
-- Solution to the logistic equation \(y'(x) = -\frac1\beta y(x) \left(1-y(x)\right)\) shifted by \(\mu\).
--
-- \[
-- f_{\mu,\beta}(x) = \frac{1}{1+\exp\left(-\frac{x-\mu}\beta\right)}
-- \]
logisticRamp
    :: Double -- ^ Center \(\mu\)
    -> Double -- ^ Wideness \(\beta\) of the transition. Higher is wider.
    -> Double
    -> Double
logisticRamp center beta x = 1/(1+exp(-(x-center)/beta))

-- | Smoothstep function, varying symmetrically from 0 to 1 between its parameters.
-- It should be called smoothstepRamp to follow this module’s nomenclature, but the
-- name /smoothstep/ is very much standard.
--
-- https://en.wikipedia.org/wiki/Smoothstep
smoothstep
    :: Double -- ^ Start ramping here
    -> Double -- ^ Finish ramping here
    -> Double
    -> Double
smoothstep lo hi =
    let smoothstep01 x = x*x*x*(x*(x*6-15)+10)
    in smoothstep01 . linearInterpolate (lo, hi) (0, 1) . clamp lo hi

-- | Smooth bump function: nonzero between -1 and 1, smooth over all of \(\mathbb R\).
--
-- \[
-- f(x) =
-- \begin{cases}
--     \exp \left(-{\frac1{1-x^2}}\right) & x \in (-1,1) \\
--     0                                  & \text{otherwise}
-- \end{cases}
-- \]
smoothBump :: Double -> Double
smoothBump x
    | -1 < x && x < 1 = exp(-1/(1-x^2))
    | otherwise = 0

-- | Gaussian falloff. Equivalent to the Gaussian distribution, but normalized to
-- \(f(\mu)=1\), \(\int = \sigma\sqrt{2\pi}\).
--
-- \[
-- f_{\mu,\sigma}(x) = \exp\left(-\frac12\left(\frac{x-\mu}\sigma\right)^2\right)
-- \]
gaussianFalloff
    :: Double -- ^ Mean \(\mu\)
    -> Double -- ^ Standard deviation \(\sigma\)
    -> Double
    -> Double
gaussianFalloff mu sigma x = exp (-1/2*((x-mu)/sigma)^2)

-- | Logistic distribution, normalized to normalized to \(f(\mu)=1\), \(\int =
-- \beta\). Compared to a Gaussian falloff, this has a heavier tail (higher
-- kurtosis).
--
-- \[
-- f_{\mu,\beta}(x) = \frac{\exp\left(-\frac{x-\mu}\beta\right)}{\left(1+\exp\left(-\frac{x-\mu}\beta\right)\right)^2}
-- \]
logisticFalloff
    :: Double -- ^ Center \(\mu\)
    -> Double -- ^ Falloff parameter \(\beta\)
    -> Double
    -> Double
logisticFalloff center beta x =
    let expTerm = exp (-(x-center)/beta)
    in expTerm / (1 + expTerm)^2

-- | Cauchy falloff. This is a much gentler (quadratic) falloff than Gaussian
-- (exponential). Normalized to \(f(\mu)=1\), \(\int = \pi\gamma\). Its falloff is
-- in fact so slow that it does not have a standard deviation (or any higher
-- moments, for that matter).
--
-- \[
-- f_{\mu,\gamma}(x) = \frac1{1 + \left(\frac{x-\mu}\gamma\right)^2}
-- \]
cauchyFalloff
    :: Double -- ^ Mean \(\mu\)
    -> Double -- ^ Falloff parameter \(\gamma\)
    -> Double
    -> Double
cauchyFalloff mu gamma x = 1 / (1 + ((x-mu)/gamma)^2)
