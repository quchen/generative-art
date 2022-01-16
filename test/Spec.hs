{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Control.Exception
import           System.FilePath

import qualified Test.Bezier
import qualified Test.Billard
import qualified Test.ConvexHull
import qualified Test.Cut
import qualified Test.Delaunay
import qualified Test.DifferentialEquation
import qualified Test.IntersectionLL
import qualified Test.Mirror
import qualified Test.Penrose
import qualified Test.Properties
import qualified Test.RandomCut
import qualified Test.Reflection
import qualified Test.Sampling
import qualified Test.SimpleOperations
import qualified Test.Trajectory
import qualified Test.Triangulate
import qualified Test.Voronoi

import Test.Tasty
import qualified VisualOutput.NormalizeSvg as Normalize
import qualified VisualOutput.TestsuiteGenerator as Visual
import qualified VisualOutput.FileSystem as FileSystem



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
        [ Test.Bezier.tests
        , Test.Billard.tests
        , Test.ConvexHull.tests
        , Test.Cut.tests
        , Test.DifferentialEquation.tests
        , Test.IntersectionLL.tests
        , Test.Mirror.tests
        , Test.Penrose.tests
        , Test.RandomCut.tests
        , Test.Reflection.tests
        , Test.Sampling.tests
        , Test.SimpleOperations.tests
        , Test.Trajectory.tests
        , Test.Triangulate.tests
        , Test.Delaunay.tests
        , Test.Voronoi.tests
        ]
    ]

runPostTestScripts :: IO ()
runPostTestScripts = do
    svgs <- do
        paths <- FileSystem.listAllFiles "docs"
        pure (filter (\path -> takeExtension path == ".svg") paths)
    let actions
            = Visual.generateMarkdown "test/out/README.md" svgs
            : Visual.generateHtml "test/out/README.html" svgs
            : map Normalize.normalizeSvgFile svgs
    sequence_ actions
