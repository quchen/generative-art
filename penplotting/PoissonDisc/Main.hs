module Main (main) where



import qualified Data.Vector         as V
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TL
import           Prelude             hiding ((**))
import           System.Random.MWC

import           Draw
import           Draw.Plotting
import           Geometry                     as G
import           Geometry.Algorithms.SimplexNoise
import           Graphics.Rendering.Cairo     as C
import           PoissonDisc



picWidth, picHeight :: Num a => a
picWidth = 500
picHeight = 500

main :: IO ()
main = do

    gen <- initialize (V.fromList [1237])
    let center = Vec2 (picWidth / 2) (picHeight / 2)
        bb = boundingBox (Vec2 50 50, Vec2 (picWidth - 50) (picHeight - 50))
        count = 600
        -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = bb
            , _poissonRadius = adaptiveRadius
            , _poissonK = 8
            }
    samples <- poissonDisc gen samplingProps

    render "out/poisson-disc.png" picWidth picHeight $ do
        setColor white
        C.paint
        setColor black
        for_ samples $ drawSample (adaptiveRadius/2)

drawSample :: Double -> (Vec2, Vec2) -> Render ()
drawSample radius (sample, parent) = do
    sketch (Line parent sample)
    C.stroke
    sketch (Circle sample radius)
    C.stroke
