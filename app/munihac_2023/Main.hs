module Main (main) where

import Control.Monad
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC

import Geometry
import Draw

-- | You can generate the output files using either:
--
-- `stack build munihac2023 --file-watch --exec munihac-2023`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is munihac2023' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
main :: IO ()
main = do
    render "munihac2023/shatter.png" 150 210 $ do
        setColor white
        Cairo.paint
