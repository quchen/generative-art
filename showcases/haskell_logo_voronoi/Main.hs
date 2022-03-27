module Main (main) where



import           Data.Char
import           Data.Maybe
import qualified Data.Vector         as V
import           Math.Noise          (Perlin (..), getValue, perlin)
import           Options.Applicative
import           Prelude             hiding ((**))
import           System.Random.MWC

import           Draw
import           Geometry                     as G
import           Geometry.Algorithms.Delaunay
import           Geometry.Algorithms.Sampling
import           Geometry.Algorithms.Voronoi
import           Geometry.Shapes              (haskellLogo)
import           Graphics.Rendering.Cairo     as C
import qualified Util.RTree                   as RT



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
    gen <- initialize (V.fromList (map (fromIntegral . ord) (show count)))
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonWidth  = picWidth
            , _poissonHeight = picHeight
            , _poissonRadius = adaptiveRadius
            , _poissonK      = 4
            }
    points <- poissonDisc gen samplingProps
    ditheringPoints <- RT.fromList <$> poissonDisc gen samplingProps{ _poissonRadius = adaptiveRadius / 4 }
    print (length points)
    let voronoi = toVoronoi (bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) points)
        voronoiColorized = mapWithMetadata (\_seed polygon ann -> colorizePolygon ditheringPoints polygon ann) voronoi

    render file picWidth picHeight $ for_ (cells voronoiColorized) drawCell

haskellLogoWithColors :: [(Polygon, Color Double)]
haskellLogoWithColors = zip haskellLogoCentered haskellLogoColors
  where
    haskellLogoCentered = G.transform (G.translate (Vec2 (picWidth/2 - 480) (picHeight/2 - 340)) <> G.scale 680) haskellLogo
    haskellLogoColors = [haskell 0, haskell 1, haskell 2, haskell 2]


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
    sketch poly
    setColor fillColor
    fillPreserve
    setColor lineColor
    setLineWidth 1
    stroke

darkGrey :: Color Double
darkGrey = hsv 0 0 0.1
