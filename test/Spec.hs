module Main (main) where



import Control.Exception
import Data.Foldable
import System.Process

import qualified Test.BezierInterpolation
import qualified Test.Billard
import qualified Test.ConvexHull
import qualified Test.Cut
import qualified Test.IntersectionLL
import qualified Test.Mirror
import qualified Test.Penrose
import qualified Test.Properties
import qualified Test.RandomCut
import qualified Test.Reflection
import qualified Test.SimpleOperations
import qualified Test.Triangulate
import qualified Test.Voronoi

import Test.Tasty



main :: IO ()
main = finally (defaultMain (defaultOptions tests))
               runPostTestScripts

defaultOptions :: TestTree -> TestTree
defaultOptions = foldr (.) id
    [ localOption (Timeout 1000000 "1s") ]

tests :: TestTree
tests = testGroup "Test suite"
    [ Test.Properties.tests
    , testGroup "Visual tests"
        [ Test.BezierInterpolation.tests
        , Test.Billard.tests
        , Test.ConvexHull.tests
        , Test.Cut.tests
        , Test.IntersectionLL.tests
        , Test.Mirror.tests
        , Test.Penrose.tests
        , Test.RandomCut.tests
        , Test.Reflection.tests
        , Test.SimpleOperations.tests
        , Test.Triangulate.tests
        , Test.Voronoi.tests
        ]
    ]

runPostTestScripts :: IO ()
runPostTestScripts = do
    handles <- traverse runCommand
        [ "./test/out/generate_readme.sh"
        , "./test/out/generate_html.sh"
        ]
    traverse_ waitForProcess handles
