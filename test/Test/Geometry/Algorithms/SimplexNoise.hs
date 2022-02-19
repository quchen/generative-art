module Test.Geometry.Algorithms.SimplexNoise (tests) where



import           Control.Monad.ST
import           Data.Foldable
import           Data.Traversable
import qualified Data.Vector                     as V
import           Graphics.Rendering.Cairo        as Cairo hiding
    (transform, translate, x, y)
import qualified Graphics.Rendering.Cairo        as Cairo
import           System.Random.MWC
import           System.Random.MWC.Distributions

import Draw
import Geometry
import Geometry.Shapes
import Util

import Test.TastyAll


import Geometry.Algorithms.SimplexNoise



tests :: TestTree
tests = testGroup "Simplex noise"
    [ -- testRefactoring
    ]

-- testRefactoring = localOption (QuickCheckTests 10000) $ testProperty "Refactoring changes nothing" $ \x y -> noise2 x y == noise2' x y
