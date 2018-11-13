{-# LANGUAGE RecordWildCards #-}

module Main (main) where



import qualified Test.Properties
import qualified Test.Visual.Billard
import qualified Test.Visual.IntersectionLL
import qualified Test.Visual.Mirror
import qualified Test.Visual.Reflection
import qualified Test.Visual.SimpleOperations

import Test.Tasty



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test suite"
    [ Test.Properties.tests
    , testGroup "Visual tests"
        [ Test.Visual.SimpleOperations.tests
        , Test.Visual.Billard.tests
        , Test.Visual.IntersectionLL.tests
        , Test.Visual.Mirror.tests
        , Test.Visual.Reflection.tests
        ]
    ]
