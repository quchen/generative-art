module Main (main) where

import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC

import Geometry
import Draw

-- | Draw a 'Render' to PNG and SVG files.
drawToFiles
    :: FilePath -- ^ Output location, will store .svg and .png versions
    -> Int -- ^ Width
    -> Int -- ^ Height
    -> Cairo.Render a -- ^ Picture, parameterized by width/height
    -> IO ()
drawToFiles filename w h drawing = do
    _ <- withSurface PNG (filename ++ ".png") w h (\surface -> Cairo.renderWith surface drawing)
    _ <- withSurface SVG (filename ++ ".svg") w h (\surface -> Cairo.renderWith surface drawing)
    pure ()

-- | You can generate the output files using either:
--
-- `stack build --file-watch --exec bob-2022-workshop`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is generative-art:bob-2022-workshop' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
main :: IO ()
main = drawToFiles "bob2022/shatter" 1000 1000 $ do
    setColor white
    Cairo.paint
    Cairo.setLineWidth 5

    gen <- Cairo.liftIO $ initialize (V.fromList [2])

    let square = Polygon
            [ Vec2 100 100
            , Vec2 100 900
            , Vec2 900 900
            , Vec2 900 100 ]
    shards <- randomCut gen square
    shards' <- concat <$> traverse (randomCut gen) shards

    for_ shards' $ \shard -> do
        sketch shard
        setColor (black `withOpacity` 0.5)
        Cairo.fillPreserve
        setColor black
        Cairo.stroke

randomCut :: GenIO -> Polygon -> Cairo.Render [Polygon]
randomCut gen polygon = Cairo.liftIO $ do
    let BoundingBox pMin pMax = boundingBox polygon
    randomPoint <- uniformRM (pMin, pMax) gen
    randomAngle <- uniformM gen
    pure $ cutPolygon
        (angledLine randomPoint randomAngle 1)
        polygon
