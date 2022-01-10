module Main (main) where

import Draw
import qualified Graphics.Rendering.Cairo as Cairo
import Steps.A.HelloWorld
import Steps.B.Squares
import Steps.C.FlowFields

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
    drawToFiles "bob2022/a_helloworld" 300 110 Steps.A.HelloWorld.hello
    drawToFiles "bob2022/b_squares" 400 400 Steps.B.Squares.squares
    drawToFiles "bob2022/c_simpleVectorField" 400 400 Steps.C.FlowFields.drawSimpleVectorField
    drawToFiles "bob2022/c_fieldLines" 400 400 Steps.C.FlowFields.drawFieldLines
    drawToFiles "bob2022/c_fieldLinesWithRandomness" 400 400 Steps.C.FlowFields.drawFieldLinesWithRandomness
