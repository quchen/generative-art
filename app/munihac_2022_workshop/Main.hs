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
    Cairo.setLineWidth 5

    let rectangle = Polygon
            [ Vec2 100 100
            , Vec2 100 900
            , Vec2 900 900
            , Vec2 900 100
            ]
    gen <- Cairo.liftIO $ initialize (V.fromList [1])
    shards <- shatter 10 gen [rectangle]

    for_ shards $ \shard -> do
        angle <- Cairo.liftIO $ deg <$> uniformRM (-10, 10) gen
        displacement <- Cairo.liftIO $ uniformRM (Vec2 (-20) (-20), Vec2 20 20) gen
        sketch (transform (rotateAround (polygonCentroid shard) angle <> translate displacement) shard)
        colorCoord <- Cairo.liftIO $ uniformRM (0, 100) gen
        let color = mathematica97 colorCoord
        setColor (color `withOpacity` 0.5)
        Cairo.fillPreserve
        setColor color
        Cairo.stroke

randomCut :: GenIO -> Polygon -> Cairo.Render [Polygon]
randomCut gen poly = Cairo.liftIO $ do
    let BoundingBox pMin pMax = boundingBox poly
    p <- uniformRM (pMin, pMax) gen
    angle <- uniformM gen
    pure (cutPolygon (angledLine p angle 1) poly)

shatter :: Int -> GenIO -> [Polygon] -> Cairo.Render [Polygon]
shatter 0 _ polys = pure polys
shatter n gen polys = do
    shards <- traverse (randomCut gen) polys
    shatter (n-1) gen (concat shards)
