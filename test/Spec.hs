module Main (main) where



import Control.Exception
import System.Exit
import System.Process

import qualified Test.Billard
import qualified Test.ConvexHull
import qualified Test.Cut
import qualified Test.IntersectionLL
import qualified Test.Mirror
import qualified Test.Properties
import qualified Test.Reflection
import qualified Test.SimpleOperations

import Test.Tasty



main :: IO ()
main = catch (defaultMain tests)
             (\e -> do normalizeSvg
                       generateVisualTestReadme
                       throwIO (e :: ExitCode))

tests :: TestTree
tests = testGroup "Test suite"
    [ Test.Properties.tests
    , testGroup "Visual tests"
        [ Test.SimpleOperations.tests
        , Test.Billard.tests
        , Test.ConvexHull.tests
        , Test.Cut.tests
        , Test.IntersectionLL.tests
        , Test.Mirror.tests
        , Test.Reflection.tests
        ]
    ]

-- Cairo has nondeterministic SVG output, because one of the generated IDs are
-- regenerated arbitrarily between runs. This shows up in Git as a change every
-- time the testsuite is run, so we use this script to normalize the generated
-- files.
normalizeSvg :: IO ()
normalizeSvg = runCommand "./test/out/normalize_svg.sh" >> pure ()

generateVisualTestReadme :: IO ()
generateVisualTestReadme = runCommand "./test/out/generate_readme.sh" >> pure ()
