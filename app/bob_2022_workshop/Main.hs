module Main (main) where

import Draw
import qualified Graphics.Rendering.Cairo as Cairo
import Steps.A.HelloWorld

-- | Draw a 'Render' to PNG and SVG files.
drawToFiles
    :: FilePath -- ^ Output location, will store .svg and .png versions
    -> Int -- ^ Width
    -> Int -- ^ Height
    -> (Int -> Int -> Cairo.Render a) -- ^ Picture, parameterized by width/height
    -> IO ()
drawToFiles filename w h drawing = do
    _ <- withSurface PNG (filename ++ ".png") w h (\surface -> Cairo.renderWith surface (drawing w h))
    _ <- withSurface SVG (filename ++ ".svg") w h (\surface -> Cairo.renderWith surface (drawing w h))
    pure ()

main :: IO ()
main = do
    drawToFiles "bob2022/helloworld" 300 110 (Steps.A.HelloWorld.hello)
