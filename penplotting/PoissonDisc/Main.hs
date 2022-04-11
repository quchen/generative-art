module Main (main) where



import qualified Data.Set            as S
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TL
import qualified Data.Vector         as V
import           Prelude             hiding ((**))
import           System.Random.MWC

import           Draw
import           Draw.Plotting
import           Geometry                     as G
import           Geometry.Algorithms.SimplexNoise
import           Graphics.Rendering.Cairo     as C
import           PoissonDisc



picWidth, picHeight :: Num a => a
picWidth = 420
picHeight = 420

main :: IO ()
main = do

    gen <- initialize (V.fromList [1237])
    let center = Vec2 (picWidth / 2) (picHeight / 2)
        bb = boundingBox (Vec2 50 50, Vec2 (picWidth - 50) (picHeight - 50))
        count = 100
        -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = bb
            , _poissonRadius = \p -> adaptiveRadius / (1 + 0.01 * norm (p -. center))
            , _poissonK = 80
            }
    samples <- poissonDisc gen samplingProps

    let drawingCairo = do
            setColor white
            C.paint
            setColor black
            for_ samples drawSample

    render "out/poisson-disc.svg" picWidth picHeight drawingCairo
    render "out/poisson-disc.png" picWidth picHeight drawingCairo

    let circles = fmap (\(center, _, radius) -> Circle center radius) samples
        connectingLines = fmap Polyline $ minimizePenHovering $ S.fromList $ (\(to, from, _) -> [from, to]) <$> samples
        drawingPlot = do
            comment "0.8mm pen for circles"
            repositionTo zero
            for_ circles plot
            withDrawingHeight 0 $ do
                repositionTo zero
                penDown
                pause PauseUserConfirm
                comment "0.3mm pen for lines"
                penUp
            for_ connectingLines plot
        settings = def
            { _feedrate = Just 12000
            , _zTravelHeight = 5
            , _zDrawingHeight = -2
            , _canvasBoundingBox = Just $ BoundingBox zero (Vec2 420 420)
            }

    TL.writeFile "poisson-disc.g" $ runPlot settings drawingPlot

drawSample :: (Vec2, Vec2, Double) -> Render ()
drawSample (sample, parent, radius) = do
    sketch (Line parent sample)
    C.setLineWidth 0.3
    C.stroke
    sketch (Circle sample (radius/2))
    C.setLineWidth 0.8
    C.stroke
