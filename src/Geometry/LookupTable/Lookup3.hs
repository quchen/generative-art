module Geometry.LookupTable.Lookup3 (
    -- * Function cache
      LookupTable3
    , Grid3(..)
    , createLookupTable3
    , lookupNearest3
    , lookupTrilinear

    -- * Technical utilities
    , IVec3(..)
    , CIVec3(..)
    , roundCIVec3
    , fromGrid3
    , toGrid3
    , valueTable3
) where



import           Control.DeepSeq
import           Control.Parallel.Strategies
import           Data.Ord.Extended
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V

import Geometry.Core
import Numerics.Interpolation



data LookupTable3 a = LookupTable3 Grid3 (Vector (Vector (Vector a)))
    deriving (Eq, Ord, Show)

instance NFData a => NFData (LookupTable3 a) where
    rnf (LookupTable3 grid vec) = withStrategy (parTraversable rdeepseq) vec `seq` rnf grid

createLookupTable3 :: Grid3 -> (Vec3 -> a) -> LookupTable3 a
createLookupTable3 grid f = LookupTable3 grid (valueTable3 grid f)

lookupNearest3 :: LookupTable3 Double -> Vec3 -> Double
lookupNearest3 (LookupTable3 grid@(Grid3 _ (iMax, jMax, kMax)) vec) (Vec3 x y z) =
    let CIVec3 iCont jCont kCont = toGrid3 grid (Vec3 x y z)
        i = clamp (0,iMax) (round iCont)
        j = clamp (0,jMax) (round jCont)
        k = clamp (0,kMax) (round kCont)
    in vec!i!j!k

lookupTrilinear :: LookupTable3 Double -> Vec3 -> Double
lookupTrilinear (LookupTable3 grid vec) xyz =
    let CIVec3 iCont jCont kCont = toGrid3 grid xyz
        iFloor = floor iCont
        jFloor = floor jCont
        kFloor = floor kCont
        iCeil  = ceiling iCont
        jCeil  = ceiling jCont
        kCeil  = ceiling kCont

        interpolateJ k i =
            let lutI = vec!i
            in if jFloor /= jCeil
                then lerp (fromIntegral jFloor, fromIntegral jCeil) (lutI!jFloor!k, lutI!jCeil!k) jCont
                else lutI!jFloor!k

        interpolateK i =
            if kFloor /= kCeil
                then lerp (fromIntegral kFloor, fromIntegral kCeil) (interpolateJ kFloor i, interpolateJ kCeil i) kCont
                else interpolateJ kFloor i

    in if iFloor /= iCeil
        then lerp (fromIntegral iFloor, fromIntegral iCeil) (interpolateK iFloor, interpolateK iCeil) iCont
        else interpolateK iFloor

data IVec3 = IVec3 !Int !Int !Int
    deriving (Eq, Ord, Show)

instance NFData IVec3 where rnf _ = ()

data CIVec3 = CIVec3 !Double !Double !Double
    deriving (Eq, Ord, Show)

instance NFData CIVec3 where rnf _ = ()

roundCIVec3 :: CIVec3 -> IVec3
roundCIVec3 (CIVec3 i j k) = IVec3 (round i) (round j) (round k)

data Grid3 = Grid3
    { _range3    :: (Vec3, Vec3)       -- ^ Range of continuous coordinates
    , _maxIndex3 :: (Int, Int, Int)    -- ^ Maximum index of the grid
    } deriving (Eq, Ord, Show)

instance NFData Grid3 where
    rnf (Grid3 (a,b) (c,d,e)) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

fromGrid3
    :: Grid3
    -> IVec3
    -> Vec3
fromGrid3 (Grid3 (Vec3 xMin yMin zMin, Vec3 xMax yMax zMax) (iMax, jMax, kMax)) (IVec3 i j k) =
    let x = lerp (0, fromIntegral iMax) (xMin, xMax) (fromIntegral i)
        y = lerp (0, fromIntegral jMax) (yMin, yMax) (fromIntegral j)
        z = lerp (0, fromIntegral kMax) (zMin, zMax) (fromIntegral k)
    in Vec3 x y z

toGrid3
    :: Grid3
    -> Vec3
    -> CIVec3
toGrid3 (Grid3 (Vec3 xMin yMin zMin, Vec3 xMax yMax zMax) (iMax, jMax, kMax)) (Vec3 x y z) =
    let iContinuous = clamp (0, fromIntegral iMax) (lerp (xMin, xMax) (0, fromIntegral iMax) x)
        jContinuous = clamp (0, fromIntegral jMax) (lerp (yMin, yMax) (0, fromIntegral jMax) y)
        kContinuous = clamp (0, fromIntegral kMax) (lerp (zMin, zMax) (0, fromIntegral kMax) z)
    in CIVec3 iContinuous jContinuous kContinuous

valueTable3 :: Grid3 -> (Vec3 -> a) -> Vector (Vector (Vector a))
valueTable3 grid@Grid3{_maxIndex3 = (is, js, ks)} f =
    V.generate (is+1) (\i ->
        V.generate (js+1) (\j ->
            V.generate (ks+1) (\k ->
                f (fromGrid3 grid (IVec3 i j k)))))
