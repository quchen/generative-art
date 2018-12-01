-- | Not sure why I wanted these functions, but here they are.
module Why where



import Data.Ratio



-- | Approximate a rational number with one that has a maximum denominator.
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
