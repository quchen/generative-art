{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import Control.Concurrent.Async
import Control.Exception
import Data.Foldable
import System.FilePath
import Data.List

import qualified Test.Bezier
import qualified Test.ConvexHull
import qualified Test.Delaunay
import qualified Test.DifferentialEquation
import qualified Test.Draw.Color
import qualified Test.Geometry.Algorithms.Contour
import qualified Test.Geometry.Algorithms.Cut
import qualified Test.Geometry.Algorithms.Triangulate
import qualified Test.Geometry.Coordinates.Hexagonal
import qualified Test.Geometry.Processes.Billard
import qualified Test.IntersectionLL
import qualified Test.Mirror
import qualified Test.Penrose
import qualified Test.Properties
import qualified Test.Reflection
import qualified Test.Sampling
import qualified Test.SimpleOperations
import qualified Test.Trajectory
import qualified Test.Voronoi

import           Test.Tasty
import qualified VisualOutput.FileSystem         as FileSystem
import qualified VisualOutput.NormalizeSvg       as Normalize
import qualified VisualOutput.TestsuiteGenerator as Visual



main :: IO ()
main = finally (defaultMain (defaultOptions tests))
               runPostTestScripts

defaultOptions :: TestTree -> TestTree
defaultOptions = foldr (.) id
    [ localOption (Timeout (10^7) "10s") ]

tests :: TestTree
tests = testGroup "Test suite"
    [ Test.Properties.tests
    , testGroup "Visual tests"
        [ Test.Bezier.tests
        , Test.ConvexHull.tests
        , Test.Delaunay.tests
        , Test.DifferentialEquation.tests
        , Test.Draw.Color.tests
        , Test.Geometry.Algorithms.Contour.tests
        , Test.Geometry.Algorithms.Cut.tests
        , Test.Geometry.Algorithms.Triangulate.tests
        , Test.Geometry.Coordinates.Hexagonal.tests
        , Test.Geometry.Processes.Billard.tests
        , Test.IntersectionLL.tests
        , Test.Mirror.tests
        , Test.Penrose.tests
        , Test.Reflection.tests
        , Test.Sampling.tests
        , Test.SimpleOperations.tests
        , Test.Trajectory.tests
        , Test.Voronoi.tests
        ]
    ]

isSubstring :: Eq char => [char] -> [char] -> Bool
isSubstring search str = case findIndex (isPrefixOf search) (tails str) of
                Just _ -> True
                Nothing -> False

runPostTestScripts :: IO ()
runPostTestScripts = do
    files <- do
        paths <- FileSystem.listAllFiles "docs"
        let p path = or [ takeExtension path == ".svg" && not (isSubstring "schemes" path)
                        , takeExtension path == ".png" &&      isSubstring "schemes" path
                        ]
        pure (filter p paths)

    normalizeAsyncs <- do
        putStrLn "Normalize SVG files for reproducible test output"
        traverse (async . Normalize.normalizeSvgFile) files

    mdAsync <- do
        putStrLn "Generate visual testsuite file (Markdown)"
        async (Visual.generateMarkdown "test/out/README.md" files)

    htmlAsync <- do
        putStrLn "Generate visual testsuite file (HTML)"
        async (Visual.generateHtml "test/out/README.html" files)

    traverse_ wait (mdAsync : htmlAsync : normalizeAsyncs)
