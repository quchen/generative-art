{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Char          (ord)
import Data.Maybe         (fromMaybe)
import Data.Vector        (fromList)
import Math.Noise         (Perlin (..), getValue, perlin)
import Prelude            hiding ((**))
import System.Random.MWC  (initialize)
import Options.Applicative

import Geometry.Algorithms.Delaunay
import Draw
import Geometry
import Geometry.Shapes          (haskellLogo)
import Graphics.Rendering.Cairo hiding (transform)
import Geometry.Algorithms.Sampling
import qualified Util.RTree as RT
import Geometry.Algorithms.Voronoi

picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 720

main :: IO ()
main = mainHaskellLogo


data Options = Options
    { _count :: Int
    , _file :: FilePath
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = Options
        <$> option auto (
               long "count"
            <> short 'c'
            <> metavar "INT"
            <> value 1000
            <> help "Number of cells"
            <> showDefault)
        <*> strOption (
               long "file"
            <> short 'f'
            <> metavar "FILE"
            <> value "out/haskell_logo_voronoi.png"
            <> help "Output filename"
            <> showDefault)
    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Voronoi Haskell Logo"
     <> header "A Haskell logo made out of Voronoi cells" )

mainHaskellLogo :: IO ()
mainHaskellLogo = do
    Options {_count=count, _file=file} <- commandLineOptions
    gen <- initialize (fromList (map (fromIntegral . ord) (show count)))
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDisc { width = picWidth, height = picHeight, radius = adaptiveRadius, k = 4, ..}
    points <- poissonDisc samplingProps
    ditheringPoints <- RT.fromList <$> poissonDisc samplingProps { radius = adaptiveRadius / 4 }
    print (length points)
    let voronoi = toVoronoi (bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) points)
        voronoiColorized = mapWithRegion (colorizePolygon ditheringPoints) voronoi

    withSurfaceAuto file picWidth picHeight $ \surface -> renderWith surface $ for_ (cells voronoiColorized) drawCell

haskellLogoWithColors :: [(Polygon, Color Double)]
haskellLogoWithColors = zip haskellLogoCentered haskellLogoColors
  where
    haskellLogoCentered = transform (Geometry.translate (Vec2 (picWidth/2 - 480) (picHeight/2 - 340)) <> Geometry.scale 680) haskellLogo
    haskellLogoColors = fmap parseRGBHex [ "453a62", "5e5086", "8f4e8b", "8f4e8b" ]


findPointsInPolygon :: RT.RTree Vec2 -> Polygon -> [Vec2]
findPointsInPolygon points poly = filter (`pointInPolygon` poly) (RT.lookupRange (boundingBox poly) points)

colorizePolygon :: RT.RTree Vec2 -> Polygon -> () -> Color Double
colorizePolygon ditheringPoints voronoiRegion _ = average $ colorizePoint <$> ditheringPointsInRegion
  where
    ditheringPointsInRegion = findPointsInPolygon ditheringPoints voronoiRegion
    colorizePoint p =
        let color = case find (pointInPolygon p . fst) haskellLogoWithColors of
                Just (_, c) -> c
                Nothing     -> darkGrey
        in adjustHsl id id (+ (0.1 * noise2d p)) color
    noise = perlin { perlinFrequency = 40/picWidth, perlinSeed = 12345}
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)

drawCell :: VoronoiCell (Color Double) -> Render ()
drawCell Cell{..} = drawPoly region props

drawPoly :: Polygon -> Color Double -> Render ()
drawPoly (Polygon []) _ = pure ()
drawPoly poly color = do
    let fillColor = color
        lineColor = blend 0.1 white color
    polygonSketch poly
    setColor fillColor
    fillPreserve
    setColor lineColor
    setLineWidth 1
    stroke

darkGrey :: Color Double
darkGrey = hsv 0 0 0.1
