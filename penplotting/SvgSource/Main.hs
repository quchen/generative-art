module Main (main) where



import           Control.Monad
import qualified Graphics.Rendering.Cairo        as C
import           System.Random.MWC
import           System.Random.MWC.Distributions
import qualified Data.Text.Lazy.IO                   as T
import Data.List


import           Draw
import           Geometry                        as G
import           Geometry.Algorithms.PerlinNoise
import qualified Geometry.Processes.FlowField    as ODE
import           Graphics.Rendering.Cairo
import           Numerics.DifferentialEquation
import           Numerics.Functions
import           Numerics.VectorAnalysis
import           Draw.GCode
import           Geometry                            as G
