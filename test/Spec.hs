{-# LANGUAGE RecordWildCards #-}

module Main (main) where



import System.Process
import System.Exit
import Control.Exception

import qualified Test.Properties
import qualified Test.Visual.Billard
import qualified Test.Visual.Cut
import qualified Test.Visual.IntersectionLL
import qualified Test.Visual.Mirror
import qualified Test.Visual.Reflection
import qualified Test.Visual.SimpleOperations

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
        [ Test.Visual.SimpleOperations.tests
        , Test.Visual.Billard.tests
        , Test.Visual.IntersectionLL.tests
        , Test.Visual.Mirror.tests
        , Test.Visual.Reflection.tests
        , Test.Visual.Cut.tests
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
