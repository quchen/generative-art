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
    noise <- simplex2 def { _simplexFrequency = 1/150 , _simplexOctaves = 2 } gen

    let bb = boundingBox (Vec2 50 50, Vec2 (picWidth - 50) (picHeight - 50))
        r0 = 25
        samplingProps = PoissonDiscParams
            { _poissonShape = bb
            , _poissonRadius = \p -> r0 * (1 + 0.4 * noise p)
            , _poissonK = 100
            }
    samples <- poissonDisc gen samplingProps

    let drawing = do
            setColor white
            C.paint
            setColor black
            C.setLineWidth 0.5
            for_ samples drawSample

    render "out/poisson-disc.svg" picWidth picHeight drawing
    render "out/poisson-disc.png" picWidth picHeight drawing

drawSample :: (Vec2, Vec2, Double) -> Render ()
drawSample (sample, parent, radius) = do
    sketch (Line parent sample)
    C.stroke
    sketch (Circle sample (radius/2))
    C.stroke
