{-# LANGUAGE OverloadedStrings #-}

module Main (main, test) where



import qualified Test.Data.Tree.Extended
import qualified Test.Draw
import qualified Test.Draw.Color
import qualified Test.Draw.Plotting
import qualified Test.Draw.Text
import qualified Test.Geometry.Algorithms.Clipping
import qualified Test.Geometry.Algorithms.Contour
import qualified Test.Geometry.Algorithms.Delaunay.Delaunator
import qualified Test.Geometry.Algorithms.Triangulate
import qualified Test.Geometry.Coordinates.Hexagonal
import qualified Test.Geometry.Core
import qualified Test.Geometry.LookupTable.Lookup2
import qualified Test.Geometry.Processes.Billard
import qualified Test.Geometry.Processes.Penrose
import qualified Test.Geometry.SvgParser.PathParser
import qualified Test.Geometry.SvgParser.SimpleShapes
import qualified Test.Physics
import qualified Test.Uncategorized.Bezier
import qualified Test.Uncategorized.ConvexHull
import qualified Test.Uncategorized.DifferentialEquation
import qualified Test.Uncategorized.GrowPolygon
import qualified Test.Uncategorized.IntersectionLL
import qualified Test.Uncategorized.Mirror
import qualified Test.Uncategorized.Properties
import qualified Test.Uncategorized.Reflection
import qualified Test.Uncategorized.SimpleOperations
import qualified Test.Uncategorized.Trajectory

import Test.Tasty
import Test.Tasty.Runners



main :: IO ()
main = defaultMain (localOption (Timeout (10^7) "10s") tests)

-- Useful in GHCi: no timeout
test :: String -> IO ()
test pattern = case parseTestPattern pattern of
    Nothing -> error "Pattern parse error"
    Just testPattern -> defaultMain (localOption testPattern tests)

tests :: TestTree
tests = testGroup "Test suite"
    [ Test.Data.Tree.Extended.tests
    , Test.Draw.Color.tests
    , Test.Draw.Plotting.tests
    , Test.Draw.tests
    , Test.Draw.Text.tests
    , Test.Geometry.Algorithms.Clipping.tests
    , Test.Geometry.Algorithms.Contour.tests
    , Test.Geometry.Algorithms.Delaunay.Delaunator.tests
    , Test.Geometry.Algorithms.Triangulate.tests
    , Test.Geometry.Coordinates.Hexagonal.tests
    , Test.Geometry.Core.tests
    , Test.Geometry.LookupTable.Lookup2.tests
    , Test.Geometry.Processes.Billard.tests
    , Test.Geometry.Processes.Penrose.tests
    , Test.Geometry.SvgParser.PathParser.tests
    , Test.Geometry.SvgParser.SimpleShapes.tests
    , Test.Physics.tests
    , Test.Uncategorized.Bezier.tests
    , Test.Uncategorized.ConvexHull.tests
    , Test.Uncategorized.DifferentialEquation.tests
    , Test.Uncategorized.GrowPolygon.tests
    , Test.Uncategorized.IntersectionLL.tests
    , Test.Uncategorized.Mirror.tests
    , Test.Uncategorized.Properties.tests
    , Test.Uncategorized.Reflection.tests
    , Test.Uncategorized.SimpleOperations.tests
    , Test.Uncategorized.Trajectory.tests
    ]
