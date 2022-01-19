-- | Not sure why I wanted these functions, but here they are.
module Why where



import           Control.Monad.ST
import           Data.Foldable
import           Data.Ratio
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VMut
import           System.Random.MWC



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

-- | Split a 'V.Vector' at an index into the slice before, the value at the index, and the slice after.
--
-- >>> divideOnIndex 0 (V.fromList [0..10])
-- ([],0,[1,2,3,4,5,6,7,8,9,10])
--
-- divideOnIndex 3 (V.fromList [0..10])
-- ([0,1,2],3,[4,5,6,7,8,9,10])
--
-- divideOnIndex 10 (V.fromList [0..10])
-- ([0,1,2,3,4,5,6,7,8,9],10, [])
divideOnIndex :: Int -> V.Vector a -> (V.Vector a, a, V.Vector a)
divideOnIndex ix vec
    | ix >= V.length vec = error ("divideOnIndex: index out of bounds. " ++ "i = " ++ show ix ++ ", length = " ++ show (V.length vec))
divideOnIndex ix vec
  = let (before, after') = V.splitAt ix vec
        Just (pivot, after) = V.uncons after'
    in (before, pivot, after)

fisherYatesShuffle
    :: GenST s
    -> Vector a
    -> ST s (Vector a)
fisherYatesShuffle gen vec = do
    let n = V.length vec
    v <- V.thaw vec
    for_ [0..n-2] $ \i -> do
        j <- uniformRM (i, n-1) gen
        VMut.swap v i j
    V.unsafeFreeze v
