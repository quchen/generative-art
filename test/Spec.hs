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
import qualified Test.RandomCut
import qualified Test.Reflection
import qualified Test.SimpleOperations
import qualified Test.Triangulate

import Test.Tasty



main :: IO ()
main = catch (defaultMain (defaultOptions tests))
             (\e -> do normalizeSvg
                       generateVisualTestReadmeMarkdown
                       generateVisualTestReadmeHtml
                       throwIO (e :: ExitCode))

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

-- Cairo has nondeterministic SVG output, because one of the generated IDs are
-- regenerated arbitrarily between runs. This shows up in Git as a change every
-- time the testsuite is run, so we use this script to normalize the generated
-- files.
normalizeSvg :: IO ()
normalizeSvg = runCommand "./test/out/normalize_svg.sh" >> pure ()

generateVisualTestReadmeMarkdown :: IO ()
generateVisualTestReadmeMarkdown = runCommand "./test/out/generate_readme.sh" >> pure ()

generateVisualTestReadmeHtml :: IO ()
generateVisualTestReadmeHtml = runCommand "./test/out/generate_html.sh" >> pure ()
