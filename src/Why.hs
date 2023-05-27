-- | Not sure why I wanted these functions, but here they are.
module Why (
    fareyApproximate
  , divideOnIndex
  , bhattacharyyaDistance
  , bhattacharyyaCoefficient
  , hellingerDistance
) where



import           Data.Ratio
import           Data.Vector (Vector)
import qualified Data.Vector as V



-- | Approximate a rational number with one that has a maximum denominator.
--
-- This can be used to implement explitit rounding when building geometry with
-- rational numbers, which have the advantage of exact calculations, in contrast to
-- Double: the algorithm can be exact, and rounding can happen after the fact.
fareyApproximate
    :: Integer -- ^ Maximum denominator
    -> Ratio Integer -- ^ Number to approximate
    -> Ratio Integer
fareyApproximate maxD exact = go 0 (ceilRatio exact)
  where
    ceilRatio x = denominator x * (quot (numerator x) (denominator x) + 1) % 1
    mediant x y = (numerator x + numerator y) % (denominator x + denominator y)
    go x y
      = let m = mediant x y
            dx = denominator x
            dy = denominator y
        in case compare exact m of
            _ | dx > maxD -> y
              | dy < maxD -> x
            LT -> go m y
            GT -> go x m
            EQ | dx + dy <= denominator exact -> m
               | dy > dx -> y
               | otherwise -> x

-- | Split a 'V.Vector' at an index into the slice before, the value at the index, and the slice after.
--
-- >>> divideOnIndex 0 (V.fromList [0..10])
-- ([],0,[1,2,3,4,5,6,7,8,9,10])
--
-- >>> divideOnIndex 3 (V.fromList [0..10])
-- ([0,1,2],3,[4,5,6,7,8,9,10])
--
-- >>> divideOnIndex 10 (V.fromList [0..10])
-- ([0,1,2,3,4,5,6,7,8,9],10,[])
divideOnIndex :: Int -> V.Vector a -> (V.Vector a, a, V.Vector a)
divideOnIndex ix vec
    | ix >= V.length vec = error ("divideOnIndex: index out of bounds. " ++ "i = " ++ show ix ++ ", length = " ++ show (V.length vec))
divideOnIndex ix vec
  = let (before, after') = V.splitAt ix vec
        Just (pivot, after) = V.uncons after'
    in (before, pivot, after)

-- | The Bhattacharyya distance measures the similarity of two probability distributions.
--
-- I’ve added it here because I’m hoping to use it to implement a distance metric
-- between two pictures at some point.
--
-- \[
--   D_{B}(p,q)=-\ln \left(BC(p,q)\right) \\
--   BC(p,q)=\sum _{{x\in X}}{\sqrt {p(x)q(x)}}
-- \]
--
-- See https://en.wikipedia.org/wiki/Bhattacharyya_distance
bhattacharyyaDistance :: Vector Double -> Vector Double -> Double
bhattacharyyaDistance xs ys = - log (bhattacharyyaCoefficient xs ys)

-- | The Bhattacharyya coefficient is an approximate measurement of the amount of overlap between two statistical samples.
--
-- \[
--   BC(p,q)=\sum _{{x\in X}}{\sqrt {p(x)q(x)}}
-- \]
--
-- See https://en.wikipedia.org/wiki/Bhattacharyya_distance#Bhattacharyya_coefficient
bhattacharyyaCoefficient :: Vector Double -> Vector Double -> Double
bhattacharyyaCoefficient xs ys = V.foldl' (+) 0 (V.zipWith (\x y -> sqrt (x*y)) xs ys)

-- | The Hellinger distance measures the similarity of two probability distributions.
--
-- I’ve added it here because I’m hoping to use it to implement a distance metric
-- between two pictures at some point.
--
-- \[
--   H(p,q)={\sqrt {1-BC(p,q)}} \\
--   BC(p,q)=\sum _{{x\in X}}{\sqrt {p(x)q(x)}}
-- \]
--
-- See https://en.wikipedia.org/wiki/Hellinger_distance
hellingerDistance :: Vector Double -> Vector Double -> Double
hellingerDistance xs ys = sqrt (1 - bhattacharyyaCoefficient xs ys)
