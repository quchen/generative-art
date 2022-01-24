{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import Control.Concurrent.Async
import Control.Exception
import Data.Foldable
import System.FilePath

import qualified Test.Bezier
import qualified Test.Billard
import qualified Test.Color
import qualified Test.ConvexHull
import qualified Test.Cut
import qualified Test.Delaunay
import qualified Test.DifferentialEquation
import qualified Test.IntersectionLL
import qualified Test.Mirror
import qualified Test.Penrose
import qualified Test.Properties
import qualified Test.Reflection
import qualified Test.Sampling
import qualified Test.SimpleOperations
import qualified Test.Trajectory
import qualified Test.Triangulate
import qualified Test.Voronoi
import qualified Test.Geometry.Coordinates.Hexagonal

import           Test.Tasty
import qualified VisualOutput.FileSystem         as FileSystem
import qualified VisualOutput.NormalizeSvg       as Normalize
import qualified VisualOutput.TestsuiteGenerator as Visual



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
        , Test.Color.tests
        , Test.Cut.tests
        , Test.DifferentialEquation.tests
        , Test.IntersectionLL.tests
        , Test.Mirror.tests
        , Test.Penrose.tests
        , Test.Geometry.Coordinates.Hexagonal.tests
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

    normalizeAsyncs <- do
        putStrLn "Normalize SVG files for reproducible test output"
        traverse (async . Normalize.normalizeSvgFile) svgs

    mdAsync <- do
        putStrLn "Generate visual testsuite file (Markdown)"
        async (Visual.generateMarkdown "test/out/README.md" svgs)

    htmlAsync <- do
        putStrLn "Generate visual testsuite file (HTML)"
        async (Visual.generateHtml "test/out/README.html" svgs)

    traverse_ wait (mdAsync : htmlAsync : normalizeAsyncs)
