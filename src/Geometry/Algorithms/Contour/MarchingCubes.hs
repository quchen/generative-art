module Geometry.Algorithms.Contour.MarchingCubes (
    module Geometry.Algorithms.Contour.MarchingCubes
) where



import           Control.DeepSeq
import           Control.Parallel.Strategies
import           Data.Bits                   (testBit, (.|.))
import qualified Data.IntMap.Strict          as IM
import           Data.Foldable
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import           System.IO

import Geometry.Core
import Geometry.LookupTable.Lookup3



data XO3 = X3 | O3
    deriving (Eq, Ord, Show)

instance NFData XO3 where
    rnf X3 = ()
    rnf O3 = ()

data CubeClassification = CubeClassification !XO3 !XO3 !XO3 !XO3 !XO3 !XO3 !XO3 !XO3
    deriving (Eq, Ord, Show)

instance NFData CubeClassification where
    rnf _ = ()

linewiseDeepseq3 :: NFData a => Vector (Vector (Vector a)) -> Vector (Vector (Vector a))
linewiseDeepseq3 = withStrategy (parTraversable (parTraversable rdeepseq))

applyThreshold3 :: Double -> Vector (Vector (Vector Double)) -> Vector (Vector (Vector XO3))
applyThreshold3 threshold = linewiseDeepseq3 . (fmap.fmap.fmap) xo
  where
    xo v | v <= threshold = X3
         | otherwise      = O3

ifor :: Vector a -> (Int -> a -> b) -> Vector b
ifor = flip V.imap

classifyCubes :: Vector (Vector (Vector XO3)) -> Vector (Vector (Vector CubeClassification))
classifyCubes xos = linewiseDeepseq3 $
    ifor (V.init xos) $ \i js ->
        ifor (V.init js) $ \j ks ->
            ifor (V.init ks) $ \k _ ->
                let v000 = xos!i!j!k
                    v100 = xos!(i+1)!j!k
                    v110 = xos!(i+1)!(j+1)!k
                    v010 = xos!i!(j+1)!k
                    v001 = xos!i!j!(k+1)
                    v101 = xos!(i+1)!j!(k+1)
                    v111 = xos!(i+1)!(j+1)!(k+1)
                    v011 = xos!i!(j+1)!(k+1)
                in CubeClassification v000 v100 v110 v010 v001 v101 v111 v011

cubeIndex :: CubeClassification -> Int
cubeIndex (CubeClassification c0 c1 c2 c3 c4 c5 c6 c7) =
    let bit b xo = if xo == X3 then b else 0
    in  bit 1   c0 .|. bit 2   c1 .|. bit 4   c2 .|. bit 8   c3
    .|. bit 16  c4 .|. bit 32  c5 .|. bit 64  c6 .|. bit 128 c7

data Edge3 = Edge3 !IVec3 !IVec3
    deriving (Eq, Ord, Show)

instance NFData Edge3 where
    rnf _ = ()

cubeEdges :: IVec3 -> V.Vector Edge3
cubeEdges (IVec3 i j k) = V.fromList
    [ Edge3 (IVec3 i j k)         (IVec3 (i+1) j k)
    , Edge3 (IVec3 (i+1) j k)     (IVec3 (i+1) (j+1) k)
    , Edge3 (IVec3 i (j+1) k)     (IVec3 (i+1) (j+1) k)
    , Edge3 (IVec3 i j k)         (IVec3 i (j+1) k)
    , Edge3 (IVec3 i j (k+1))     (IVec3 (i+1) j (k+1))
    , Edge3 (IVec3 (i+1) j (k+1)) (IVec3 (i+1) (j+1) (k+1))
    , Edge3 (IVec3 i (j+1) (k+1)) (IVec3 (i+1) (j+1) (k+1))
    , Edge3 (IVec3 i j (k+1))     (IVec3 i (j+1) (k+1))
    , Edge3 (IVec3 i j k)         (IVec3 i j (k+1))
    , Edge3 (IVec3 (i+1) j k)     (IVec3 (i+1) j (k+1))
    , Edge3 (IVec3 (i+1) (j+1) k) (IVec3 (i+1) (j+1) (k+1))
    , Edge3 (IVec3 i (j+1) k)     (IVec3 i (j+1) (k+1))
    ]

edgeTable :: VU.Vector Int
edgeTable = VU.fromList
    [ 0x0  , 0x109, 0x203, 0x30a, 0x406, 0x50f, 0x605, 0x70c
    , 0x80c, 0x905, 0xa0f, 0xb06, 0xc0a, 0xd03, 0xe09, 0xf00
    , 0x190, 0x99 , 0x393, 0x29a, 0x596, 0x49f, 0x795, 0x69c
    , 0x99c, 0x895, 0xb9f, 0xa96, 0xd9a, 0xc93, 0xf99, 0xe90
    , 0x230, 0x339, 0x33 , 0x13a, 0x636, 0x73f, 0x435, 0x53c
    , 0xa3c, 0xb35, 0x83f, 0x936, 0xe3a, 0xf33, 0xc39, 0xd30
    , 0x3a0, 0x2a9, 0x1a3, 0xaa , 0x7a6, 0x6af, 0x5a5, 0x4ac
    , 0xbac, 0xaa5, 0x9af, 0x8a6, 0xfaa, 0xea3, 0xda9, 0xca0
    , 0x460, 0x569, 0x663, 0x76a, 0x66 , 0x16f, 0x265, 0x36c
    , 0xc6c, 0xd65, 0xe6f, 0xf66, 0x86a, 0x963, 0xa69, 0xb60
    , 0x5f0, 0x4f9, 0x7f3, 0x6fa, 0x1f6, 0xff , 0x3f5, 0x2fc
    , 0xdfc, 0xcf5, 0xfff, 0xef6, 0x9fa, 0x8f3, 0xbf9, 0xaf0
    , 0x650, 0x759, 0x453, 0x55a, 0x256, 0x35f, 0x55 , 0x15c
    , 0xe5c, 0xf55, 0xc5f, 0xd56, 0xa5a, 0xb53, 0x859, 0x950
    , 0x7c0, 0x6c9, 0x5c3, 0x4ca, 0x3c6, 0x2cf, 0x1c5, 0xcc
    , 0xfcc, 0xec5, 0xdcf, 0xcc6, 0xbca, 0xac3, 0x9c9, 0x8c0
    , 0x8c0, 0x9c9, 0xac3, 0xbca, 0xcc6, 0xdcf, 0xec5, 0xfcc
    , 0xcc , 0x1c5, 0x2cf, 0x3c6, 0x4ca, 0x5c3, 0x6c9, 0x7c0
    , 0x950, 0x859, 0xb53, 0xa5a, 0xd56, 0xc5f, 0xf55, 0xe5c
    , 0x15c, 0x55 , 0x35f, 0x256, 0x55a, 0x453, 0x759, 0x650
    , 0xaf0, 0xbf9, 0x8f3, 0x9fa, 0xef6, 0xfff, 0xcf5, 0xdfc
    , 0x2fc, 0x3f5, 0xff , 0x1f6, 0x6fa, 0x7f3, 0x4f9, 0x5f0
    , 0xb60, 0xa69, 0x963, 0x86a, 0xf66, 0xe6f, 0xd65, 0xc6c
    , 0x36c, 0x265, 0x16f, 0x66 , 0x76a, 0x663, 0x569, 0x460
    , 0xca0, 0xda9, 0xea3, 0xfaa, 0x8a6, 0x9af, 0xaa5, 0xbac
    , 0x4ac, 0x5a5, 0x6af, 0x7a6, 0xaa , 0x1a3, 0x2a9, 0x3a0
    , 0xd30, 0xc39, 0xf33, 0xe3a, 0x936, 0x83f, 0xb35, 0xa3c
    , 0x53c, 0x435, 0x73f, 0x636, 0x13a, 0x33 , 0x339, 0x230
    , 0xe90, 0xf99, 0xc93, 0xd9a, 0xa96, 0xb9f, 0x895, 0x99c
    , 0x69c, 0x795, 0x49f, 0x596, 0x29a, 0x393, 0x99 , 0x190
    , 0xf00, 0xe09, 0xd03, 0xc0a, 0xb06, 0xa0f, 0x905, 0x80c
    , 0x70c, 0x605, 0x50f, 0x406, 0x30a, 0x203, 0x109, 0x0
    ]

triTable :: V.Vector (V.Vector Int)
triTable = V.fromList
    [ V.fromList []
    , V.fromList [0, 8, 3]
    , V.fromList [0, 1, 9]
    , V.fromList [1, 8, 3, 9, 8, 1]
    , V.fromList [1, 2, 10]
    , V.fromList [0, 8, 3, 1, 2, 10]
    , V.fromList [9, 2, 10, 0, 2, 9]
    , V.fromList [2, 8, 3, 2, 10, 8, 10, 9, 8]
    , V.fromList [3, 11, 2]
    , V.fromList [0, 11, 2, 8, 11, 0]
    , V.fromList [1, 9, 0, 2, 11, 3]
    , V.fromList [1, 11, 2, 1, 9, 11, 9, 8, 11]
    , V.fromList [3, 10, 1, 11, 10, 3]
    , V.fromList [0, 10, 1, 0, 8, 10, 8, 11, 10]
    , V.fromList [3, 9, 0, 3, 11, 9, 11, 10, 9]
    , V.fromList [9, 8, 10, 10, 8, 11]
    , V.fromList [4, 7, 8]
    , V.fromList [4, 3, 0, 7, 3, 4]
    , V.fromList [0, 1, 9, 8, 4, 7]
    , V.fromList [4, 1, 9, 4, 7, 1, 7, 3, 1]
    , V.fromList [1, 2, 10, 8, 4, 7]
    , V.fromList [3, 4, 7, 3, 0, 4, 1, 2, 10]
    , V.fromList [9, 2, 10, 9, 0, 2, 8, 4, 7]
    , V.fromList [2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4]
    , V.fromList [8, 4, 7, 3, 11, 2]
    , V.fromList [11, 4, 7, 11, 2, 4, 2, 0, 4]
    , V.fromList [9, 0, 1, 8, 4, 7, 2, 11, 3]
    , V.fromList [4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1]
    , V.fromList [3, 10, 1, 3, 11, 10, 7, 8, 4]
    , V.fromList [1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4]
    , V.fromList [4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3]
    , V.fromList [4, 7, 11, 4, 11, 9, 9, 11, 10]
    , V.fromList [9, 5, 4]
    , V.fromList [9, 5, 4, 0, 8, 3]
    , V.fromList [0, 5, 4, 1, 5, 0]
    , V.fromList [8, 5, 4, 8, 3, 5, 3, 1, 5]
    , V.fromList [1, 2, 10, 9, 5, 4]
    , V.fromList [3, 0, 8, 1, 2, 10, 4, 9, 5]
    , V.fromList [5, 2, 10, 5, 4, 2, 4, 0, 2]
    , V.fromList [2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8]
    , V.fromList [9, 5, 4, 2, 3, 11]
    , V.fromList [0, 11, 2, 0, 8, 11, 4, 9, 5]
    , V.fromList [0, 5, 4, 0, 1, 5, 2, 3, 11]
    , V.fromList [2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5]
    , V.fromList [10, 3, 11, 10, 1, 3, 9, 5, 4]
    , V.fromList [4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10]
    , V.fromList [5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3]
    , V.fromList [5, 4, 8, 5, 8, 10, 10, 8, 11]
    , V.fromList [9, 7, 8, 5, 7, 9]
    , V.fromList [9, 3, 0, 9, 5, 3, 5, 7, 3]
    , V.fromList [0, 7, 8, 0, 1, 7, 1, 5, 7]
    , V.fromList [1, 5, 3, 3, 5, 7]
    , V.fromList [9, 7, 8, 9, 5, 7, 10, 1, 2]
    , V.fromList [10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3]
    , V.fromList [8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2]
    , V.fromList [2, 10, 5, 2, 5, 3, 3, 5, 7]
    , V.fromList [7, 9, 5, 7, 8, 9, 3, 11, 2]
    , V.fromList [9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11]
    , V.fromList [2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7]
    , V.fromList [11, 2, 1, 11, 1, 7, 7, 1, 5]
    , V.fromList [9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11]
    , V.fromList [5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0]
    , V.fromList [11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0]
    , V.fromList [11, 10, 5, 7, 11, 5]
    , V.fromList [10, 6, 5]
    , V.fromList [0, 8, 3, 5, 10, 6]
    , V.fromList [9, 0, 1, 5, 10, 6]
    , V.fromList [1, 8, 3, 1, 9, 8, 5, 10, 6]
    , V.fromList [1, 6, 5, 2, 6, 1]
    , V.fromList [1, 6, 5, 1, 2, 6, 3, 0, 8]
    , V.fromList [9, 6, 5, 9, 0, 6, 0, 2, 6]
    , V.fromList [5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8]
    , V.fromList [2, 3, 11, 10, 6, 5]
    , V.fromList [11, 0, 8, 11, 2, 0, 10, 6, 5]
    , V.fromList [0, 1, 9, 2, 3, 11, 5, 10, 6]
    , V.fromList [5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11]
    , V.fromList [6, 3, 11, 6, 5, 3, 5, 1, 3]
    , V.fromList [0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6]
    , V.fromList [3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9]
    , V.fromList [6, 5, 9, 6, 9, 11, 11, 9, 8]
    , V.fromList [5, 10, 6, 4, 7, 8]
    , V.fromList [4, 3, 0, 4, 7, 3, 6, 5, 10]
    , V.fromList [1, 9, 0, 5, 10, 6, 8, 4, 7]
    , V.fromList [10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4]
    , V.fromList [6, 1, 2, 6, 5, 1, 4, 7, 8]
    , V.fromList [1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7]
    , V.fromList [8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6]
    , V.fromList [7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9]
    , V.fromList [3, 11, 2, 7, 8, 4, 10, 6, 5]
    , V.fromList [5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11]
    , V.fromList [0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6]
    , V.fromList [9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6]
    , V.fromList [8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6]
    , V.fromList [5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11]
    , V.fromList [0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7]
    , V.fromList [6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9]
    , V.fromList [10, 4, 9, 6, 4, 10]
    , V.fromList [4, 10, 6, 4, 9, 10, 0, 8, 3]
    , V.fromList [10, 0, 1, 10, 6, 0, 6, 4, 0]
    , V.fromList [8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10]
    , V.fromList [1, 4, 9, 1, 2, 4, 2, 6, 4]
    , V.fromList [3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4]
    , V.fromList [0, 2, 4, 4, 2, 6]
    , V.fromList [8, 3, 2, 8, 2, 4, 4, 2, 6]
    , V.fromList [10, 4, 9, 10, 6, 4, 11, 2, 3]
    , V.fromList [0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6]
    , V.fromList [3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10]
    , V.fromList [6, 4, 1, 6, 1, 10, 8, 1, 11, 8, 11, 2, 11, 1, 6]
    , V.fromList [9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3]
    , V.fromList [8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1]
    , V.fromList [3, 11, 6, 3, 6, 0, 0, 6, 4]
    , V.fromList [6, 4, 8, 11, 6, 8]
    , V.fromList [7, 10, 6, 7, 8, 10, 8, 9, 10]
    , V.fromList [0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10]
    , V.fromList [10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0]
    , V.fromList [10, 6, 7, 10, 7, 1, 1, 7, 3]
    , V.fromList [1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7]
    , V.fromList [2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9]
    , V.fromList [7, 8, 0, 7, 0, 6, 6, 0, 2]
    , V.fromList [7, 3, 2, 6, 7, 2]
    , V.fromList [2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7]
    , V.fromList [2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7]
    , V.fromList [1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11]
    , V.fromList [11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1]
    , V.fromList [8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6]
    , V.fromList [0, 9, 1, 11, 6, 7]
    , V.fromList [7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0]
    , V.fromList [7, 11, 6]
    , V.fromList [7, 6, 11]
    , V.fromList [3, 0, 8, 11, 7, 6]
    , V.fromList [0, 1, 9, 11, 7, 6]
    , V.fromList [8, 1, 9, 8, 3, 1, 11, 7, 6]
    , V.fromList [10, 1, 2, 6, 11, 7]
    , V.fromList [1, 2, 10, 3, 0, 8, 6, 11, 7]
    , V.fromList [2, 9, 0, 2, 10, 9, 6, 11, 7]
    , V.fromList [6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8]
    , V.fromList [7, 2, 3, 6, 2, 7]
    , V.fromList [7, 0, 8, 7, 6, 0, 6, 2, 0]
    , V.fromList [2, 7, 6, 2, 3, 7, 0, 1, 9]
    , V.fromList [1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6]
    , V.fromList [10, 7, 6, 10, 1, 7, 1, 3, 7]
    , V.fromList [10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8]
    , V.fromList [0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7]
    , V.fromList [7, 6, 10, 7, 10, 8, 8, 10, 9]
    , V.fromList [6, 8, 4, 11, 8, 6]
    , V.fromList [3, 6, 11, 3, 0, 6, 0, 4, 6]
    , V.fromList [8, 6, 11, 8, 4, 6, 9, 0, 1]
    , V.fromList [9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6]
    , V.fromList [6, 8, 4, 6, 11, 8, 2, 10, 1]
    , V.fromList [1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6]
    , V.fromList [4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9]
    , V.fromList [10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3]
    , V.fromList [8, 2, 3, 8, 4, 2, 4, 6, 2]
    , V.fromList [0, 4, 2, 4, 6, 2]
    , V.fromList [1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8]
    , V.fromList [1, 9, 4, 1, 4, 2, 2, 4, 6]
    , V.fromList [8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1]
    , V.fromList [10, 1, 0, 10, 0, 6, 6, 0, 4]
    , V.fromList [4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3]
    , V.fromList [10, 9, 4, 6, 10, 4]
    , V.fromList [4, 9, 5, 7, 6, 11]
    , V.fromList [0, 8, 3, 4, 9, 5, 11, 7, 6]
    , V.fromList [5, 0, 1, 5, 4, 0, 7, 6, 11]
    , V.fromList [11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5]
    , V.fromList [9, 5, 4, 10, 1, 2, 7, 6, 11]
    , V.fromList [6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5]
    , V.fromList [7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2]
    , V.fromList [3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6]
    , V.fromList [7, 2, 3, 7, 6, 2, 5, 4, 9]
    , V.fromList [9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7]
    , V.fromList [3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0]
    , V.fromList [6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8]
    , V.fromList [9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7]
    , V.fromList [1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4]
    , V.fromList [4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10]
    , V.fromList [7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10]
    , V.fromList [6, 9, 5, 6, 11, 9, 11, 8, 9]
    , V.fromList [3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5]
    , V.fromList [0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11]
    , V.fromList [6, 11, 3, 6, 3, 5, 5, 3, 1]
    , V.fromList [1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6]
    , V.fromList [0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10]
    , V.fromList [11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5]
    , V.fromList [6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3]
    , V.fromList [5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2]
    , V.fromList [9, 5, 6, 9, 6, 0, 0, 6, 2]
    , V.fromList [1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8]
    , V.fromList [1, 5, 6, 2, 1, 6]
    , V.fromList [1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6]
    , V.fromList [10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0]
    , V.fromList [0, 3, 8, 5, 6, 10]
    , V.fromList [10, 5, 6]
    , V.fromList [11, 5, 10, 7, 5, 11]
    , V.fromList [11, 5, 10, 11, 7, 5, 8, 3, 0]
    , V.fromList [5, 11, 7, 5, 10, 11, 1, 9, 0]
    , V.fromList [10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1]
    , V.fromList [11, 1, 2, 11, 7, 1, 7, 5, 1]
    , V.fromList [0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11]
    , V.fromList [9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7]
    , V.fromList [7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2]
    , V.fromList [2, 5, 10, 2, 3, 5, 3, 7, 5]
    , V.fromList [8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5]
    , V.fromList [9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2]
    , V.fromList [9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2]
    , V.fromList [1, 3, 5, 3, 7, 5]
    , V.fromList [0, 8, 7, 0, 7, 1, 1, 7, 5]
    , V.fromList [9, 0, 3, 9, 3, 5, 5, 3, 7]
    , V.fromList [9, 8, 7, 5, 9, 7]
    , V.fromList [5, 8, 4, 5, 10, 8, 10, 11, 8]
    , V.fromList [5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0]
    , V.fromList [0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5]
    , V.fromList [10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4]
    , V.fromList [2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8]
    , V.fromList [0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11]
    , V.fromList [0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5]
    , V.fromList [9, 4, 5, 2, 11, 3]
    , V.fromList [2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4]
    , V.fromList [5, 10, 2, 5, 2, 4, 4, 2, 0]
    , V.fromList [3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9]
    , V.fromList [5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2]
    , V.fromList [8, 4, 5, 8, 5, 3, 3, 5, 1]
    , V.fromList [1, 0, 5, 0, 4, 5]
    , V.fromList [8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5]
    , V.fromList [9, 4, 5]
    , V.fromList [4, 11, 7, 4, 9, 11, 9, 10, 11]
    , V.fromList [0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11]
    , V.fromList [1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11]
    , V.fromList [3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4]
    , V.fromList [4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2]
    , V.fromList [9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3]
    , V.fromList [11, 7, 4, 11, 4, 2, 2, 4, 0]
    , V.fromList [11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4]
    , V.fromList [2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9]
    , V.fromList [9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7]
    , V.fromList [3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10]
    , V.fromList [1, 10, 2, 8, 7, 4]
    , V.fromList [4, 9, 1, 4, 1, 7, 7, 1, 3]
    , V.fromList [4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1]
    , V.fromList [4, 0, 3, 7, 4, 3]
    , V.fromList [4, 8, 7]
    , V.fromList [9, 10, 8, 10, 11, 8]
    , V.fromList [3, 0, 9, 3, 9, 11, 11, 9, 10]
    , V.fromList [0, 1, 10, 0, 10, 8, 8, 10, 11]
    , V.fromList [3, 1, 10, 11, 3, 10]
    , V.fromList [1, 2, 11, 1, 11, 9, 9, 11, 8]
    , V.fromList [3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9]
    , V.fromList [0, 2, 11, 8, 0, 11]
    , V.fromList [3, 2, 11]
    , V.fromList [2, 3, 8, 2, 8, 10, 10, 8, 9]
    , V.fromList [9, 10, 2, 0, 9, 2]
    , V.fromList [2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8]
    , V.fromList [1, 10, 2]
    , V.fromList [1, 3, 8, 9, 1, 8]
    , V.fromList [0, 9, 1]
    , V.fromList [0, 3, 8]
    , V.fromList []
    ]

data Line3 = Line3 !Vec3 !Vec3

line3Length :: Line3 -> Double
line3Length (Line3 a b) = norm (b -. a)

edgeInterpolation
    :: Grid3
    -> (Vec3 -> Double)
    -> Double
    -> Edge3
    -> Double
    -> Vec3
edgeInterpolation grid f threshold (Edge3 iStart iEnd) tolerance =
    let start = fromGrid3 grid iStart
        end   = fromGrid3 grid iEnd
    in binarySearchRoot3 f start end threshold tolerance

binarySearchRoot3 :: (Vec3 -> Double) -> Vec3 -> Vec3 -> Double -> Double -> Vec3
binarySearchRoot3 f start end threshold tolerance
    | len <= tolerance = middle
    | signum fStart /= signum fMiddle = binarySearchRoot3 f start middle threshold tolerance
    | signum fMiddle /= signum fEnd = binarySearchRoot3 f middle end threshold tolerance
    | otherwise = middle
  where
    middle = (start +. end) /. 2
    len = norm (end -. start)
    fStart  = f start - threshold
    fMiddle = f middle - threshold
    fEnd    = f end - threshold

cubesToTriangles
    :: Grid3
    -> (Vec3 -> Double)
    -> Double
    -> Double
    -> Vector (Vector (Vector CubeClassification))
    -> [Triangle3]
cubesToTriangles grid f threshold tolerance classified =
    fold.fold.fold $ ifor classified $ \i jSlice ->
        ifor jSlice $ \j kSlice ->
            ifor kSlice $ \k classification ->
                let origin = IVec3 i j k
                    idx = cubeIndex classification
                in if idx == 0 || idx == 255
                    then []
                    else let edges = cubeEdges origin
                             edgeBits = edgeTable VU.! idx
                             triIndices = triTable V.! idx
                         in buildTriangles grid f threshold tolerance edges edgeBits triIndices

buildTriangles
    :: Grid3
    -> (Vec3 -> Double)
    -> Double
    -> Double
    -> V.Vector Edge3
    -> Int
    -> V.Vector Int
    -> [Triangle3]
buildTriangles grid f threshold tolerance edges edgeBits triIndices =
    let edgeVertexMap = V.imap (\ei edge ->
            if testBitMC edgeBits ei
                then Just (edgeInterpolation grid f threshold edge tolerance)
                else Nothing) edges
        go [] = []
        go (e0:e1:e2:rest) =
            case (edgeVertexMap V.! e0, edgeVertexMap V.! e1, edgeVertexMap V.! e2) of
                (Just v0, Just v1, Just v2) ->
                    let normal = computeNormal v0 v1 v2
                    in Triangle3 normal (v0, v1, v2) : go rest
                _ -> go rest
        go _ = []
    in go (V.toList triIndices)

testBitMC :: Int -> Int -> Bool
testBitMC = testBit

computeNormal :: Vec3 -> Vec3 -> Vec3 -> Vec3
computeNormal v0 v1 v2 =
    let e1 = v1 -. v0
        e2 = v2 -. v0
        n  = crossProduct3 e1 e2
        len = norm n
    in if len > 1e-12 then n /. len else Vec3 0 0 1

groupConnectedComponents :: [Triangle3] -> [[Triangle3]]
groupConnectedComponents [] = []
groupConnectedComponents triangles = finalize (go triangles IM.empty IM.empty)
  where
    go [] parent assigned = (parent, assigned)
    go (t:ts) parent assigned =
        let Triangle3 _ (v0, v1, v2) = t
            key0 = vec3Key v0
            key1 = vec3Key v1
            key2 = vec3Key v2
            roots = nubOrd [ findRoot parent k
                           | k <- [key0, key1, key2]
                           , k `IM.member` assigned
                           ]
            newAssigned = IM.insert key0 () $ IM.insert key1 () $ IM.insert key2 () assigned
        in case roots of
            [] ->
                let parent1 = IM.insert key0 key0 parent
                    parent2 = union parent1 key0 key1
                    parent3 = union parent2 key0 key2
                in go ts parent3 newAssigned
            [r] ->
                let parent1 = union parent r key0
                    parent2 = union parent1 r key1
                    parent3 = union parent2 r key2
                in go ts parent3 newAssigned
            (r:rs) ->
                let parent1 = foldl' (\p r' -> union p r r') parent rs
                    parent2 = union parent1 r key0
                    parent3 = union parent2 r key1
                    parent4 = union parent3 r key2
                in go ts parent4 newAssigned

    findRoot parent k = case IM.lookup k parent of
        Just p | p /= k    -> findRoot parent p
               | otherwise -> k
        Nothing -> k

    union parent a b
        | ra == rb = parent
        | otherwise = IM.insert ra rb parent
      where
        ra = findRoot parent a
        rb = findRoot parent b

    nubOrd = go' []
      where
        go' acc [] = reverse acc
        go' acc (x:xs) | x `elem` acc = go' acc xs
                       | otherwise    = go' (x:acc) xs

    finalize (parent, assigned) =
        let compMap = IM.foldlWithKey' (\m k _ ->
                        let r = findRoot parent k
                        in IM.insertWith (++) r [k] m
                    ) IM.empty assigned
        in [ [ t
             | t <- triangles
             , let Triangle3 _ (v0, _, _) = t
             , let k = vec3Key v0
             , k `IM.member` assigned
             , findRoot parent k == r
             ]
           | r <- IM.keys compMap ]

vec3Key :: Vec3 -> Int
vec3Key (Vec3 x y z) = round (x * 73856093) + round (y * 19349663) + round (z * 83492791)

isoSurfaces
    :: Grid3
    -> (Vec3 -> Double)
    -> Double
    -> [[Triangle3]]
isoSurfaces grid f =
    let table = valueTable3 grid f
    in \threshold ->
        let tableThresholded = applyThreshold3 threshold table
            classified = classifyCubes tableThresholded
            tolerance = 1e-3
            triangles = cubesToTriangles grid f threshold tolerance classified
            components = groupConnectedComponents triangles
        in components

writeSTL :: FilePath -> [[Triangle3]] -> IO ()
writeSTL filePath components = do
    h <- openFile filePath WriteMode
    hPutStrLn h "solid marching_cubes"
    for_ (zip [0..] components) $ \(i, tris) -> do
        hPutStrLn h $ "  solid component_" ++ show i
        for_ tris $ \(Triangle3 (Vec3 nx ny nz) (Vec3 x0 y0 z0, Vec3 x1 y1 z1, Vec3 x2 y2 z2)) -> do
            hPutStrLn h $ "  facet normal " ++ show nx ++ " " ++ show ny ++ " " ++ show nz
            hPutStrLn h "    outer loop"
            hPutStrLn h $ "      vertex " ++ show x0 ++ " " ++ show y0 ++ " " ++ show z0
            hPutStrLn h $ "      vertex " ++ show x1 ++ " " ++ show y1 ++ " " ++ show z1
            hPutStrLn h $ "      vertex " ++ show x2 ++ " " ++ show y2 ++ " " ++ show z2
            hPutStrLn h "    endloop"
            hPutStrLn h "  endfacet"
        hPutStrLn h $ "  endsolid component_" ++ show i
    hPutStrLn h "endsolid marching_cubes"
    hClose h
