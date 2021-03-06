module Main (main) where



import Control.Exception
import Data.Foldable
import System.Process

import qualified Test.Billard
import qualified Test.ConvexHull
import qualified Test.Cut
import qualified Test.IntersectionLL
import qualified Test.Mirror
import qualified Test.Properties
import qualified Test.RandomCut
import qualified Test.Reflection
import qualified Test.SimpleOperations
import qualified Test.Triangulate

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
        [ Test.Billard.tests
        , Test.ConvexHull.tests
        , Test.Cut.tests
        , Test.IntersectionLL.tests
        , Test.Mirror.tests
        , Test.RandomCut.tests
        , Test.Reflection.tests
        , Test.SimpleOperations.tests
        , Test.Triangulate.tests
        ]
    ]

runPostTestScripts :: IO ()
runPostTestScripts = do
    handles <- traverse runCommand
        [ "./scripts/normalize_svg.sh"
        , "./test/out/generate_readme.sh"
        , "./test/out/generate_html.sh"
        ]
    traverse_ waitForProcess handles
