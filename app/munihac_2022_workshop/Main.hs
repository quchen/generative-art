module Main (main) where

import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC

import Geometry
import Draw
import Draw.Plotting

-- | Draw a 'Render' to PNG and SVG files.
drawToFiles
    :: FilePath -- ^ Output location, will store .svg and .png versions
    -> Int -- ^ Width
    -> Int -- ^ Height
    -> Cairo.Render () -- ^ Picture, parameterized by width/height
    -> IO ()
drawToFiles filename w h drawing = do
    render (filename ++ ".png") w h drawing
    render (filename ++ ".svg") w h drawing

-- | You can generate the output files using either:
--
-- `stack build munihac2022 --file-watch --exec munihac-2022-workshop`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is munihac2022' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
main :: IO ()
main = drawToFiles "munihac2022/shatter" 1000 1000 $ do
    setColor white
    Cairo.paint
