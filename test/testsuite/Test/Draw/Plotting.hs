{-# LANGUAGE OverloadedStrings #-}

module Test.Draw.Plotting (tests) where



import           Control.Monad
import           Data.Colour.Names
import           Data.Default.Class
import           Data.List
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C
import           Text.Printf

import Draw.Plotting
import Geometry               as G
import Numerics.Interpolation

import Test.TastyAll



tests :: TestTree
tests = testGroup "GCode penplottingPlotting"
    [
    ]
