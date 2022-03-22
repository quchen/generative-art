{-# LANGUAGE OverloadedStrings #-}

module Main (main, test) where



import Control.Concurrent.Async
import Control.Exception
import System.FilePath
import System.FilePath.Glob

import qualified Test.Data.Tree.Extended
import qualified Test.Draw
import qualified Test.Draw.Color
import qualified Test.Geometry.Algorithms.Contour
import qualified Test.Geometry.Algorithms.Cut
import qualified Test.Geometry.Algorithms.Delaunay
import qualified Test.Geometry.Algorithms.Sampling
import qualified Test.Geometry.Algorithms.Triangulate
import qualified Test.Geometry.Algorithms.Voronoi
import qualified Test.Geometry.Coordinates.Hexagonal
import qualified Test.Geometry.LookupTable.Lookup2
import qualified Test.Geometry.Processes.ApollonianGasket
import qualified Test.Geometry.Processes.Billard
import qualified Test.Geometry.Processes.Penrose
import qualified Test.Uncategorized.Bezier
import qualified Test.Uncategorized.ConvexHull
import qualified Test.Uncategorized.DifferentialEquation
import qualified Test.Uncategorized.IntersectionLL
import qualified Test.Uncategorized.Mirror
import qualified Test.Uncategorized.Properties
import qualified Test.Uncategorized.Reflection
import qualified Test.Uncategorized.SimpleOperations
import qualified Test.Uncategorized.SvgParser.PathParser
import qualified Test.Uncategorized.Trajectory

import           Test.Tasty
import           Test.Tasty.Runners
import qualified VisualOutput.FileSystem         as FileSystem
import qualified VisualOutput.TestsuiteGenerator as Visual



main :: IO ()
main = finally (defaultMain (localOption (Timeout (10^7) "10s") tests))
               runPostTestScripts

-- Useful in GHCi: no timeout
test :: String -> IO ()
test pattern = case parseTestPattern pattern of
    Nothing -> error "Pattern parse error"
    Just testPattern -> defaultMain (localOption testPattern tests)

tests :: TestTree
tests = testGroup "Test suite"
    [ Test.Data.Tree.Extended.tests
    , Test.Draw.tests
    , Test.Draw.Color.tests
    , Test.Geometry.Algorithms.Contour.tests
    , Test.Geometry.Algorithms.Cut.tests
    , Test.Geometry.Algorithms.Delaunay.tests
    , Test.Geometry.Algorithms.Sampling.tests
    , Test.Geometry.Algorithms.Triangulate.tests
    , Test.Geometry.Algorithms.Voronoi.tests
    , Test.Geometry.Coordinates.Hexagonal.tests
    , Test.Geometry.LookupTable.Lookup2.tests
    , Test.Geometry.Processes.ApollonianGasket.tests
    , Test.Geometry.Processes.Billard.tests
    , Test.Geometry.Processes.Penrose.tests
    , Test.Uncategorized.Bezier.tests
    , Test.Uncategorized.ConvexHull.tests
    , Test.Uncategorized.DifferentialEquation.tests
    , Test.Uncategorized.IntersectionLL.tests
    , Test.Uncategorized.Mirror.tests
    , Test.Uncategorized.Properties.tests
    , Test.Uncategorized.Reflection.tests
    , Test.Uncategorized.SimpleOperations.tests
    , Test.Uncategorized.SvgParser.PathParser.tests
    , Test.Uncategorized.Trajectory.tests
    ]

runPostTestScripts :: IO ()
runPostTestScripts = do
    files <- do
        paths <- FileSystem.listAllFiles "docs"
        let p path
                | match "**/colors/schemes/continuous/**/*" path = takeExtension path == ".png"
                | otherwise = takeExtension path == ".svg"
        pure (filter p paths)

    mdAsync <- do
        putStrLn "Generate visual testsuite file (Markdown)"
        async (Visual.generateMarkdown "test/testsuite/out/README.md" files)

    htmlAsync <- do
        putStrLn "Generate visual testsuite file (HTML)"
        async (Visual.generateHtml "test/testsuite/out/README.html" files)

    wait mdAsync
    wait htmlAsync
