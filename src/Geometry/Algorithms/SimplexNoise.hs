-- | Simplex noise functions in various dimensions.
--
-- Based on
-- [example Java code by Stefan Gustavson](https://weber.itn.liu.se/~stegu/simplexnoise/SimplexNoise.java).
-- Optimisations by Peter Eastman. Better rank ordering method for 4D by Stefan
-- Gustavson in 2012.
--
-- This code was placed in the public domain by its original author, Stefan
-- Gustavson. You may use it as you see fit, but attribution is appreciated.
--
-- <<docs/haddock/Geometry/Algorithms/SimplexNoise/simplex_noise.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/SimplexNoise/simplex_noise.svg" 500 500 $ do
-- let (w,h) = (500,500)
-- let (simplexNoiseColor, simplexNoiseVisibility) = runST $ MWC.withRng [] $ \gen -> do
--         c <- simplex2 gen def
--             { _simplexFrequency = 5/(2*w)
--             }
--         v <- simplex2 gen def
--             { _simplexFrequency = 10/(2*w)
--             }
--         pure (c,v)
--     cellSize = 5
--     hexes = hexagonsInRange 30 hexZero
--     fitToViewport = transformBoundingBox (map (hexagonPoly cellSize) hexes) (shrinkBoundingBox 10 [zero, Vec2 w h]) def
-- for_ hexes $ \hex -> do
--     let polygon = hexagonPoly cellSize hex
--         vec = toVec2 cellSize hex
--         color = icefire (lerp (-1,1) (0,1) (simplexNoiseColor vec))
--     sketch (transform fitToViewport (shrinkPolygon 1 polygon))
--     setColor (color `withOpacity` (lerp (-1,1) (0,1) (simplexNoiseVisibility vec)))
--     C.fillPreserve
--     setColor (color `withOpacity` (lerp (-1,1) (1,0.2) (simplexNoiseVisibility vec)))
--     C.stroke
-- :}
-- Generated file: size 1MB, crc32: 0x1e0dba00
module Geometry.Algorithms.SimplexNoise (
      SimplexParameters(..)
    , simplex1
    , simplex2
    , simplex3
    , simplex4
) where



import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Bits
import           Data.Default.Class
import           Data.List
import           Data.STRef
import           Data.Vector.Extended    (Vector, (!))
import qualified Data.Vector.Extended    as V
import qualified Data.Vector.Mutable     as VM
import qualified System.Random.MWC       as MWC

import Geometry.Core



-- $setup
-- >>> import           Draw
-- >>> import           Geometry.Coordinates.Hexagonal
-- >>> import qualified Graphics.Rendering.Cairo       as C
-- >>> import           Numerics.Interpolation
-- >>> import qualified System.Random.MWC.Extended     as MWC



sum' :: [Double] -> Double
sum' = foldl' (+) 0

data Grad3 = Grad3 !Double !Double !Double
    deriving (Eq, Ord, Show)

data Grad4 = Grad4 !Double !Double !Double !Double
    deriving (Eq, Ord, Show)

-- | To speed up gradient calculations. This is also used for the 2D case (ignoring
-- the 3rd component).
grad3 :: Vector Grad3
grad3 = V.fromList
    [ Grad3 1    1    0
    , Grad3 (-1) 1    0
    , Grad3 1    (-1) 0
    , Grad3 (-1) (-1) 0
    , Grad3 1    0    1
    , Grad3 (-1) 0    1
    , Grad3 1    0    (-1)
    , Grad3 (-1) 0    (-1)
    , Grad3 0    1    1
    , Grad3 0    (-1) 1
    , Grad3 0    1    (-1)
    , Grad3 0    (-1) (-1)
    ]

-- | To speed up gradient calculations
grad4 :: Vector Grad4
grad4 = V.fromList
    [ Grad4 0    1    1    1
    , Grad4 0    1    1    (-1)
    , Grad4 0    1    (-1) 1
    , Grad4 0    1    (-1) (-1)
    , Grad4 0    (-1) 1    1
    , Grad4 0    (-1) 1    (-1)
    , Grad4 0    (-1) (-1) 1
    , Grad4 0    (-1) (-1) (-1)
    , Grad4 1    0    1    1
    , Grad4 1    0    1    (-1)
    , Grad4 1    0    (-1) 1
    , Grad4 1    0    (-1) (-1)
    , Grad4 (-1) 0    1    1
    , Grad4 (-1) 0    1    (-1)
    , Grad4 (-1) 0    (-1) 1
    , Grad4 (-1) 0    (-1) (-1)
    , Grad4 1    1    0    1
    , Grad4 1    1    0    (-1)
    , Grad4 1    (-1) 0    1
    , Grad4 1    (-1) 0    (-1)
    , Grad4 (-1) 1    0    1
    , Grad4 (-1) 1    0    (-1)
    , Grad4 (-1) (-1) 0    1
    , Grad4 (-1) (-1) 0    (-1)
    , Grad4 1    1    1    0
    , Grad4 1    1    (-1) 0
    , Grad4 1    (-1) 1    0
    , Grad4 1    (-1) (-1) 0
    , Grad4 (-1) 1    1    0
    , Grad4 (-1) 1    (-1) 0
    , Grad4 (-1) (-1) 1    0
    , Grad4 (-1) (-1) (-1) 0
    ]

dot2 :: Grad3 -> Double -> Double -> Double
dot2 (Grad3 gx gy _) x y = gx*x + gy*y

dot3 :: Grad3 -> Double -> Double -> Double -> Double
dot3 (Grad3 gx gy gz) x y z = gx*x + gy*y + gz*z

dot4 :: Grad4 -> Double -> Double -> Double -> Double -> Double
dot4 (Grad4 gx gy gz gw) x y z w = gx*x + gy*y + gz*z + gw*w

-- | Raw 1D simplex noise, with ampltude 1 and frequency 1/px.
rawSimplexNoise1
    :: Vector Int -- ^ Permutation of [0..255], concatenated with itself to save us a modulo calculation.
    -> Vector Int -- ^ Permutation table mod 12.
    -> Double     -- ^ x
    -> Double     -- ^ \(\in [-1,1]\)
rawSimplexNoise1 perm permModX xin = rawSimplexNoise2 perm permModX xin 0

-- | Raw 2D simplex noise, with ampltude 1 and frequency 1/px.
rawSimplexNoise2
    :: Vector Int -- ^ Permutation of [0..255], concatenated with itself to save us a modulo calculation.
    -> Vector Int -- ^ Permutation table mod 12.
    -> Double     -- ^ x
    -> Double     -- ^ y
    -> Double     -- ^ \(\in [-1,1]\)
rawSimplexNoise2 perm permMod12 xin yin =
    let
        -- Skewing and unskewing factors for 2 dimensions
        f2, g2 :: Double
        f2 = 0.5*(sqrt 3-1)
        g2 = (3-sqrt 3)/6

        -- Skew the input space to determine which simplex cell we’re in
        i, j :: Int
        (i,j) = (floor (xin+s), floor (yin+s))
          where
            s = (xin+yin)*f2  -- Hairy factor for 2D

        -- The x,y distances from the cell origin
        x0, y0 :: Double
        (x0, y0) = (xin-xx0, yin-yy0)
          where
            t = fromIntegral (i+j)*g2
            xx0 = fromIntegral i-t -- Unskew the cell origin back to (x,y) space
            yy0 = fromIntegral j-t

        -- For the 2D case, the simplex shape is an equilateral triangle.
        -- Determine which simplex we are in.
        i1, j1 :: Int -- Offsets for second (middle) corner of simplex in (i,j) coords
        (i1, j1)
            | x0>y0 = (1,0) -- lower triangle, XY order: (0,0)->(1,0)->(1,1)
            | otherwise = (0,1) -- upper triangle, YX order: (0,0)->(0,1)->(1,1)

        -- A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
        -- a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where c = (3-sqrt 3)/6

        -- Offsets for middle corner in (x,y) unskewed coords
        x1, y1 :: Double
        (x1, y1) =
            ( x0 - fromIntegral i1 + g2
            , y0 - fromIntegral j1 + g2
            )

        -- Offsets for last corner in (x,y) unskewed coords
        x2, y2 :: Double
        (x2, y2) =
            ( x0 - 1 + 2 * g2
            , y0 - 1 + 2 * g2
            )

        -- Work out the hashed gradient indices of the three simplex corners
        gi0, gi1, gi2 :: Int
        (gi0, gi1, gi2) =
            ( permMod12 ! (ii+   (perm !  jj    ))
            , permMod12 ! (ii+i1+(perm ! (jj+j1)))
            , permMod12 ! (ii+1+ (perm ! (jj+1) )))
          where
            ii = i .&. 255
            jj = j .&. 255

        cornerContribution x y gi =
            let t1 = 0.5 - x*x - y*y;
                t2 = t1^2
            in if t1 < 0
                then 0
                else t2 * t2 * dot2 (grad3!gi) x y

        -- Noise contributions from the three corners
        cornerContributions :: [Double]
        cornerContributions@[_, _, _] = [cornerContribution x y gi | (x,y,gi) <- [(x0,y0,gi0), (x1,y1,gi1), (x2,y2,gi2)]]

    in
        -- Add contributions from each corner to get the final noise value.
        -- The result is scaled to return values in the interval [-1,1].
        70 * sum' cornerContributions

-- | Raw 3D simplex noise, with ampltude 1 and frequency 1/px.
rawSimplexNoise3
    :: Vector Int -- ^ Permutation of [0..255], concatenated with itself to save us a modulo calculation.
    -> Vector Int -- ^ Permutation table mod 12.
    -> Double     -- ^ x
    -> Double     -- ^ y
    -> Double     -- ^ z
    -> Double     -- ^ \(\in [-1,1]\)
rawSimplexNoise3 perm permMod12 xin yin zin =
    let
        -- Skewing and unskewing factors for 2 dimensions
        f3, g3 :: Double
        f3 = 1/3 -- Very nice and simple skew factor for 3D
        g3 = 1/6

        i, j, k :: Int
        (i,j,k) = (floor (xin+s), floor (yin+s), floor (zin+s))
          where
            s = (xin+yin+zin)*f3 -- Factor for 3D skewing

        -- The x,y,z distances from the cell origin
        x0, y0, z0 :: Double
        (x0, y0, z0) = (xin-xx0, yin-yy0, zin-zz0)
          where
            t = fromIntegral (i+j+k)*g3
            xx0 = fromIntegral i-t -- Unskew the cell origin back to (x,y,z) space
            yy0 = fromIntegral j-t
            zz0 = fromIntegral k-t


        -- For the 3D case, the simplex shape is a slightly irregular tetrahedron.
        -- Determine which simplex we are in.
        i1, j1, k1 :: Int -- Offsets for second corner of simplex in (i,j,k) coords
        i2, j2, k2 :: Int -- Offsets for third corner of simplex in (i,j,k) coords
        (i1, j1, k1, i2, j2, k2)
            | x0 >= y0 = if
                | y0 >= z0  -> (1, 0, 0, 1, 1, 0) -- X Y Z order
                | x0 >= z0  -> (1, 0, 0, 1, 0, 1) -- X Z Y order
                | otherwise -> (0, 0, 1, 1, 0, 1) -- Z X Y order
            | otherwise = if
                | y0 < z0   -> (0, 0, 1, 0, 1, 1) -- Z Y X order
                | x0 < z0   -> (0, 1, 0, 0, 1, 1) -- Y Z X order
                | otherwise -> (0, 1, 0, 1, 1, 0) -- Y X Z order

        -- A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
        -- a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
        -- a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
        -- c = 1/6.

        -- Offsets for second corner in (x,y,z) coords
        x1, y1, z1 :: Double
        (x1, y1, z1) =
            ( x0 - fromIntegral i1 + g3
            , y0 - fromIntegral j1 + g3
            , z0 - fromIntegral k1 + g3
            )

        -- Offsets for third corner in (x,y,z) coords
        x2, y2, z2 :: Double
        (x2, y2, z2) =
            ( x0 - fromIntegral i2 + 2*g3
            , y0 - fromIntegral j2 + 2*g3
            , z0 - fromIntegral k2 + 2*g3
            )

        -- Offsets for last corner in (x,y,z) coords
        x3, y3, z3 :: Double
        (x3, y3, z3) =
            ( x0 - 1 + 3*g3
            , y0 - 1 + 3*g3
            , z0 - 1 + 3*g3
            )

        -- Work out the hashed gradient indices of the four simplex corners
        gi0, gi1, gi2, gi3 :: Int
        (gi0, gi1, gi2, gi3) =
            ( permMod12 ! (ii+   (perm ! (jj+   (perm !  kk    ))))
            , permMod12 ! (ii+i1+(perm ! (jj+j1+(perm ! (kk+k1)))))
            , permMod12 ! (ii+i2+(perm ! (jj+j2+(perm ! (kk+k2)))))
            , permMod12 ! (ii+1+ (perm ! (jj+1+ (perm ! (kk+1 )))))
            )
          where
            ii = i .&. 255
            jj = j .&. 255
            kk = k .&. 255

        cornerContribution x y z gi =
            let t1 = 0.6 - x*x - y*y - z*z
                t2 = t1^2
            in if t1 < 0
                then 0
                else t2 * t2 * dot3 (grad3!gi) x y z

        cornerContributions :: [Double]
        cornerContributions@[_, _, _, _] = [cornerContribution x y z gi | (x,y,z,gi) <- [(x0,y0,z0,gi0), (x1,y1,z1,gi1), (x2,y2,z2,gi2), (x3,y3,z3,gi3)]]

    in
        -- Add contributions from each corner to get the final noise value.
        -- The result is scaled to stay just inside [-1,1]
        32 * sum' cornerContributions

-- | Raw 4D simplex noise, with ampltude 1 and frequency 1/px.
rawSimplexNoise4
    :: Vector Int -- ^ Permutation of [0..255], concatenated with itself to save us a modulo calculation.
    -> Vector Int -- ^ Permutation table mod 32.
    -> Double     -- ^ x
    -> Double     -- ^ y
    -> Double     -- ^ z
    -> Double     -- ^ w
    -> Double     -- ^ \(\in [-1,1]\)
rawSimplexNoise4 perm permMod32 xin yin zin win =
    let
        -- Skewing and unskewing factors for 4 dimensions
        f4, g4 :: Double
        f4 = (sqrt 5-1)/4
        g4 = (5-sqrt 5)/20

        -- Skew the (x,y,z,w) space to determine which cell of 24 simplices we’re in
        i, j, k, l :: Int
        (i, j, k, l) = (floor (xin + s), floor (yin + s), floor (zin + s), floor (win + s))
          where
            s = (xin + yin + zin + win) * f4 -- Factor for 4D skewing

        -- The x,y,z,w distances from the cell origin
        x0, y0, z0, w0 :: Double
        (x0, y0, z0, w0) =
            ( xin - xx0
            , yin - yy0
            , zin - zz0
            , win - ww0
            )
          where
            t = fromIntegral (i + j + k + l) * g4 -- Factor for 4D unskewing
            xx0 = fromIntegral i - t -- Unskew the cell origin back to (x,y,z,w) space
            yy0 = fromIntegral j - t
            zz0 = fromIntegral k - t
            ww0 = fromIntegral l - t

        -- For the 4D case, the simplex is a 4D shape I won't even try to describe.
        -- To find out which of the 24 possible simplices we’re in, we need to
        -- determine the magnitude ordering of x0, y0, z0 and w0. Six pair-wise
        -- comparisons are performed between each possible pair of the four
        -- coordinates, and the results are used to rank the numbers.

        -- (rankX, rankY, rankZ, rankW) is a 4-vector with the numbers 0, 1, 2 and 3
        -- in some order. We use a thresholding to set the coordinates in turn.
        (rankX, rankY, rankZ, rankW) = runST $ do
            let increment ref = modifySTRef' ref (+1)
            rankXRef <- newSTRef 0
            rankYRef <- newSTRef 0
            rankZRef <- newSTRef 0
            rankWRef <- newSTRef 0
            increment (if x0 > y0 then rankXRef else rankYRef)
            increment (if x0 > z0 then rankXRef else rankZRef)
            increment (if x0 > w0 then rankXRef else rankWRef)
            increment (if y0 > z0 then rankYRef else rankZRef)
            increment (if y0 > w0 then rankYRef else rankWRef)
            increment (if z0 > w0 then rankZRef else rankWRef)
            (,,,)
                <$> readSTRef rankXRef
                <*> readSTRef rankYRef
                <*> readSTRef rankZRef
                <*> readSTRef rankWRef

        if01 p = if p then 1 else 0


        i1, j1, k1, l1 :: Int -- The integer offsets for the second simplex corner
        -- Rank 3 denotes the largest coordinate.
        i1 = if01 (rankX >= 3)
        j1 = if01 (rankY >= 3)
        k1 = if01 (rankZ >= 3)
        l1 = if01 (rankW >= 3)

        i2, j2, k2, l2 :: Int -- The integer offsets for the third simplex corner
        -- Rank 2 denotes the second largest coordinate.
        i2 = if01 (rankX >= 2)
        j2 = if01 (rankY >= 2)
        k2 = if01 (rankZ >= 2)
        l2 = if01 (rankW >= 2)

        -- Rank 1 denotes the second smallest coordinate.
        i3, j3, k3, l3 :: Int -- The integer offsets for the fourth simplex corner
        i3 = if01 (rankX >= 1)
        j3 = if01 (rankY >= 1)
        k3 = if01 (rankZ >= 1)
        l3 = if01 (rankW >= 1)

        -- The fifth corner has all coordinate offsets = 1, so no need to compute that.

        -- Offsets for second corner in (x,y,z,w) coords
        x1, y1, z1, w1 :: Double
        x1 = x0 - fromIntegral i1 + g4;
        y1 = y0 - fromIntegral j1 + g4;
        z1 = z0 - fromIntegral k1 + g4;
        w1 = w0 - fromIntegral l1 + g4;

        -- Offsets for third corner in (x,y,z,w) coords
        x2, y2, z2, w2 :: Double
        x2 = x0 - fromIntegral i2 + 2*g4;
        y2 = y0 - fromIntegral j2 + 2*g4;
        z2 = z0 - fromIntegral k2 + 2*g4;
        w2 = w0 - fromIntegral l2 + 2*g4;

        -- Offsets for fourth corner in (x,y,z,w) coords
        x3, y3, z3, w3 :: Double
        x3 = x0 - fromIntegral i3 + 3*g4;
        y3 = y0 - fromIntegral j3 + 3*g4;
        z3 = z0 - fromIntegral k3 + 3*g4;
        w3 = w0 - fromIntegral l3 + 3*g4;

        -- Offsets for last corner in (x,y,z,w) coords
        x4, y4, z4, w4 :: Double
        x4 = x0 - 1 + 4*g4;
        y4 = y0 - 1 + 4*g4;
        z4 = z0 - 1 + 4*g4;
        w4 = w0 - 1 + 4*g4;

        -- Work out the hashed gradient indices of the five simplex corners
        gi0, gi1, gi2, gi3, gi4 :: Int
        (gi0, gi1, gi2, gi3, gi4) =
            ( permMod32 ! (ii+   perm ! (jj+   perm ! (kk+   perm !  ll    )))
            , permMod32 ! (ii+i1+perm ! (jj+j1+perm ! (kk+k1+perm ! (ll+l1))))
            , permMod32 ! (ii+i2+perm ! (jj+j2+perm ! (kk+k2+perm ! (ll+l2))))
            , permMod32 ! (ii+i3+perm ! (jj+j3+perm ! (kk+k3+perm ! (ll+l3))))
            , permMod32 ! (ii+1+ perm ! (jj+1+ perm ! (kk+1+ perm ! (ll+1 ))))
            )
          where
            ii = i .&. 255
            jj = j .&. 255
            kk = k .&. 255
            ll = l .&. 255

        cornerContribution x y z w gi =
            let t1 = 0.6 - x*x - y*y - z*z - w*w
                t2 = t1^2
            in if t1 < 0
                then 0
                else t2 * t2 * dot4 (grad4!gi) x y z w

        cornerContributions :: [Double]
        cornerContributions@[_, _, _, _, _] = [cornerContribution x y z w gi | (x,y,z,w,gi) <- [(x0,y0,z0,w0,gi0), (x1,y1,z1,w1,gi1), (x2,y2,z2,w2,gi2), (x3,y3,z3,w3,gi3), (x4,y4,z4,w4,gi4)]]

    in 27 * sum' cornerContributions

-- | Named arguments for 'simplex2', 'simplex3', 'simplex4'.
data SimplexParameters = SimplexParameters
    { _simplexFrequency :: Double
        -- ^ Frequency of the first octave, e.g. \(\frac1{2\text{width}}\) to span the whole width of the picture.
        --   'def'ault: 1.

    , _simplexLacunarity :: Double
        -- ^ Frequency multiplier between octaves.
        --   'def'ault: 2.

    , _simplexOctaves :: Int
        -- ^ Number of octaves to generate.
        --   'def'ault: 6.

    , _simplexPersistence :: Double
        -- ^ Amplitude multiplier between octaves.
        --   'def'ault: 0.5.
    } deriving (Eq, Ord, Show)

instance Default SimplexParameters where
    def = SimplexParameters
        { _simplexFrequency   = 1
        , _simplexLacunarity  = 2
        , _simplexOctaves     = 6
        , _simplexPersistence = 0.5
        }

createPermutationTable :: PrimMonad st => MWC.Gen (PrimState st) -> st (Vector Int)
createPermutationTable gen = do
    vecMut <- VM.generate 256 id
    V.fisherYatesShuffle gen vecMut
    vec <- V.unsafeFreeze vecMut
    pure (vec <> vec) -- To remove the need for index wrapping, double the permutation table length

-- | One-dimensional simplex noise. See 'simplex2' for a code example.
simplex1
    :: PrimMonad st
    => MWC.Gen (PrimState st) -- ^ To initialize the permutation table
    -> SimplexParameters
    -> st (Double -> Double)
simplex1 gen SimplexParameters{..} = do
    let frequencies = iterate (* _simplexLacunarity) _simplexFrequency
        amplitudes = iterate (* _simplexPersistence) 1
    perm <- createPermutationTable gen
    pure $ \x ->
        sum' (take _simplexOctaves
                   (zipWith (\freq amp ->
                                (*) amp
                                    (rawSimplexNoise1 perm
                                                      (fmap (`mod` 12) perm)
                                                      (x*freq)))
                            frequencies
                            amplitudes))

-- | Two-dimensional simplex noise.
--
-- @
-- noiseFunction = 'runST' $ do
--     gen <- 'MWC.create'
--     'simplex2' 'def' gen
-- 'for_' [1..10] $ \x ->
--     'for_' [1..10] $ \y ->
--         'print' ('noiseFunction' (Vec2 x y))
-- @
simplex2
    :: PrimMonad st
    => MWC.Gen (PrimState st) -- ^ To initialize the permutation table
    -> SimplexParameters
    -> st (Vec2 -> Double)
simplex2 gen SimplexParameters{..} = do
    let frequencies = iterate (* _simplexLacunarity) _simplexFrequency
        amplitudes = iterate (* _simplexPersistence) 1
    perm <- createPermutationTable gen
    pure $ \(Vec2 x y) ->
        sum' (take _simplexOctaves
                   (zipWith (\freq amp ->
                                (*) amp
                                    (rawSimplexNoise2 perm
                                                      (fmap (`mod` 12) perm)
                                                      (x*freq)
                                                      (y*freq)))
                            frequencies
                            amplitudes))

-- | Three-dimensional simplex noise. See 'simplex2' for a code example.
simplex3
    :: PrimMonad st
    => MWC.Gen (PrimState st) -- ^ To initialize the permutation table
    -> SimplexParameters
    -> st (Double -> Double -> Double -> Double) -- ^ \(\text{noise}(x,y,z)\)
simplex3 gen SimplexParameters{..} = do
    let frequencies = iterate (* _simplexLacunarity) _simplexFrequency
        amplitudes = iterate (* _simplexPersistence) 1
    perm <- createPermutationTable gen
    pure $ \x y z ->
        sum' (take _simplexOctaves
                   (zipWith (\freq amp ->
                                (*) amp
                                    (rawSimplexNoise3 perm
                                                      (fmap (`mod` 12) perm)
                                                      (x*freq)
                                                      (y*freq) (z*freq)))
                            frequencies
                            amplitudes))

-- | Four-dimensional simplex noise. See 'simplex2' for a code example.
simplex4
    :: PrimMonad st
    => MWC.Gen (PrimState st) -- ^ To initialize the permutation table
    -> SimplexParameters
    -> st (Double -> Double -> Double -> Double -> Double) -- ^ \(\text{noise}(x,y,z,w)\)
simplex4 gen SimplexParameters{..} = do
    let frequencies = iterate (* _simplexLacunarity) _simplexFrequency
        amplitudes = iterate (* _simplexPersistence) 1
    perm <- createPermutationTable gen
    pure $ \x y z w ->
        sum' (take _simplexOctaves
                   (zipWith (\freq amp ->
                                (*) amp
                                    (rawSimplexNoise4 perm
                                                      (fmap (`mod` 32) perm)
                                                      (x*freq)
                                                      (y*freq) (z*freq) (w*freq)))
                            frequencies
                            amplitudes))
