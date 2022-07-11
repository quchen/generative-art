module Main (main) where



import qualified Data.ByteString.Lazy       as BSL
import           Data.Csv
import           Data.Ord.Extended
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified Data.Vector.Algorithms.Tim as Tim
import           Draw
import           Geometry                   as G
import           Graphics.Rendering.Cairo   as C hiding (x, y)
import           Numerics.Interpolation



main :: IO ()
main = do
    pulsarData <- getPulsarData
    let w,h :: Num a => a
        w = 600
        h = 800

        graphHeight = 300

        resizeToWidth = G.transform (transformBoundingBox pulsarData (Vec2 0 0, Vec2 w graphHeight) (TransformBBSettings FitWidthHeight IgnoreAspect FitAlignCenter))

        layoutEvenlySpaced space xs = flip V.imap xs $ \i points -> G.transform (G.translate (Vec2 0 (fromIntegral i*space))) points

        layoutEvenlySpacedToCanvasSize =
            let go spaceLo spaceHi dataset =
                    let middle = (spaceLo + spaceHi) / 2
                        spaced = layoutEvenlySpaced middle dataset
                    in case boundingBoxSize spaced of
                        (_bbW, drawnHeight)
                            | abs (drawnHeight - h) <= 1e3 -> spaced
                            | drawnHeight > h              -> go spaceLo middle dataset
                            | otherwise                    -> go middle spaceHi dataset
            in go 0 h

        fitToCanvas geo = G.transform (G.transformBoundingBox geo (zero +. Vec2 0 0, Vec2 w h -. Vec2 0 0) (TransformBBSettings FitWidthHeight IgnoreAspect FitAlignCenter)) geo

        pulsarDataScaled = fitToCanvas (layoutEvenlySpacedToCanvasSize (resizeToWidth pulsarData))

        drawing = V.iforM_ pulsarDataScaled $ \i signal -> do
            cairoScope $ do
                sketch (Polyline (V.singleton (Vec2 0 h) <> signal <> V.singleton (Vec2 w h)))
                closePath
                let colorValue = lerpID (0, length pulsarData-1) (0.2,0.8) i
                setColor (inferno colorValue)
                fill
            cairoScope $ do
                setLineWidth 1
                sketch (Polyline signal)
                setColor black
                stroke

    render "showcases/pulsar_cp1919.svg" w h drawing
    -- render "out/pulsar.png" w h $ do
    --     cairoScope $ do
    --         setColor white
    --         paint
    --     render


getPulsarData :: IO (Vector (Vector Vec2))
getPulsarData = do
    -- Source: https://github.com/coolbutuseless/CP1919
    contents <- BSL.readFile "showcases/pulsar/cp1919.csv"
    case decode HasHeader contents of
        Left err -> error ("Error loading CSV: " ++ show err)
        Right r -> pure (groupLines r)

groupLines :: Vector (Int, Double, Double) -> Vector (Vector Vec2)
groupLines
    = alignOnZero
    . rescaleTime
    . toVec2
    . alignBaselines
    . invertY
    . dropLineNumber
    . groupByLine
    . sortByLineAndTime
  where
    sortByLineAndTime = sortVecBy (comparing (\(line, _, _) -> line) <> comparing (\(_, t, _) -> t))
    groupByLine = groupVecOn (\(line, _, _) -> line)
    dropLineNumber = (fmap.fmap) (\(_line, t, y) -> (t,y))
    invertY = (fmap.fmap) (\(t, y) -> (t, -y))
    alignBaselines =
        let yMisalignment xs =
                let firstAndLastPoints n = V.take n xs <> V.drop (V.length xs - n) xs
                    yValues = fmap (\(_, y) -> y) (firstAndLastPoints 3)
                in mean yValues
        in fmap (\signal -> fmap (\(t, y) -> (t, y - yMisalignment signal)) signal)
    toVec2 = (fmap.fmap) (uncurry Vec2)
    rescaleTime xs =
        let MinMax tMin tMax = (foldMap.foldMap) (\(Vec2 t _) -> MinMax t t) xs
            tSignalMin signal = let Vec2 t _ = V.head signal in t
            tSignalMax signal = let Vec2 t _ = V.last signal in t
        in V.map (\signal -> V.map (\(Vec2 t y) -> Vec2 (lerp (tSignalMin signal, tSignalMax signal) (tMin, tMax) t) y) signal) xs
    alignOnZero vec =
        let BoundingBox topLeft _bottomRight = boundingBox vec
        in G.transform (G.translate (negateV topLeft)) vec

mean :: Vector Double -> Double
mean vec = sum vec / fromIntegral (length vec)

sortVecBy :: (a -> a -> Ordering) -> Vector a -> Vector a
sortVecBy cmp vec = V.create $ do
    mvec <- V.thaw vec
    Tim.sortBy cmp mvec
    pure mvec

groupVecOn :: Ord b => (a -> b) -> Vector a -> Vector (Vector a)
groupVecOn p = V.fromList . go
  where
    go v = case v V.!? 0 of
        Nothing -> []
        Just x ->
            let (chunk, rest) = V.span (\y -> p x == p y) v
            in chunk : go rest
