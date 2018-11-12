{-# LANGUAGE RecordWildCards #-}

module Main (main) where



import qualified Visual.Billard
import qualified Visual.IntersectionLL
import qualified Visual.Mirror
import qualified Visual.Reflection
import qualified Visual.SimpleOperations

import Test.Tasty



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Visual tests"
    [ Visual.SimpleOperations.tests
    , Visual.Billard.tests
    , Visual.IntersectionLL.tests
    , Visual.Mirror.tests
    , Visual.Reflection.tests
    ]
