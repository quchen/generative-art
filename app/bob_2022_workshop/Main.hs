module Main (main) where

import Draw
import qualified Graphics.Rendering.Cairo as Cairo
import Steps.A.HelloWorld
import Steps.B.Squares
import Steps.C.FlowFields
import Steps.D.Shatter

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

-- | You can generate the output files using either:
--
-- `stack build --file-watch --exec bob-2022-workshop`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is generative-art:bob-2022-workshop' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
main :: IO ()
main = do
    drawToFiles "bob2022/a_helloworld" 300 110 Steps.A.HelloWorld.hello

    drawToFiles "bob2022/b_squares" 400 400 Steps.B.Squares.squares

    drawToFiles "bob2022/c1_simpleVectorField"        400 400 Steps.C.FlowFields.drawSimpleVectorField
    drawToFiles "bob2022/c2_fieldLines"               400 400 Steps.C.FlowFields.drawFieldLines
    drawToFiles "bob2022/c3_fieldLinesWithRandomness" 400 400 Steps.C.FlowFields.drawFieldLinesWithRandomness

    drawToFiles "bob2022/d1_square"           400 400 Steps.D.Shatter.drawSquare
    drawToFiles "bob2022/d2_square_cut"       400 400 Steps.D.Shatter.drawSquareCut
    drawToFiles "bob2022/d3_square_shards"    400 400 Steps.D.Shatter.drawSquareShards
    drawToFiles "bob2022/d4_square_shattered" 400 400 Steps.D.Shatter.drawSquareShattered
