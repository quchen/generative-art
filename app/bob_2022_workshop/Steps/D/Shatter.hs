module Steps.D.Shatter (
      drawSquare
    , drawSquareCut
    , drawSquareShards
    , drawSquareShattered
) where



import           Control.Monad.ST
import           Data.Traversable         (for)
import qualified Graphics.Rendering.Cairo as Cairo
import           System.Random.MWC

import Draw
import Geometry



-- | We start with a simple square.
drawSquare :: Int -> Int -> Cairo.Render ()
drawSquare w h = do
    let scaleFactor = fromIntegral (max w h) / 1000
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ setColor white >> Cairo.paint
    Cairo.setLineWidth 5

    let square = Polygon
            [ Vec2 100 100
            , Vec2 100 900
            , Vec2 900 900
            , Vec2 900 100
            ]

    sketch square
    setColor (mathematica97 0 `withOpacity` 0.5)
    Cairo.fillPreserve
    setColor (mathematica97 0)
    Cairo.stroke

-- | Basic polygon cut: Given a polygon and a line, cut the polygon along the
-- line.
drawSquareCut :: Int -> Int -> Cairo.Render ()
drawSquareCut w h = do
    let scaleFactor = fromIntegral (max w h) / 1000
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ setColor white >> Cairo.paint
    Cairo.setLineWidth 5

    let square = Polygon
            [ Vec2 100 100
            , Vec2 100 900
            , Vec2 900 900
            , Vec2 900 100
            ]
        crack = angledLine (Vec2 500 500) (deg 30) 1
        shards = cutPolygon crack square

    for_ (zip [0..] shards) $ \(i, shard) -> do
        sketch shard
        setColor (mathematica97 i `withOpacity` 0.5)
        Cairo.fillPreserve
        setColor (mathematica97 i)
        Cairo.stroke

-- | Now we can apply cuts recursively:
-- Take a polygon, cut it along a random line, and repeat with the resulting
-- polygons.
--
-- Two parameters control the entire process: The process should stop at some
-- point, e.g. for a certain minimum shard size; and to make the result look
-- appealing, we will not accept any random cut, but select the cuts by
-- criteria such as "the shards should roughly have the same size", or "the
-- angles of the shard should not be too acute".
drawSquareShards :: Int -> Int -> Cairo.Render ()
drawSquareShards w h = do
    let scaleFactor = fromIntegral (max w h) / 1000
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ setColor white >> Cairo.paint
    Cairo.setLineWidth 5

    let square = Polygon
            [ Vec2 100 100
            , Vec2 100 900
            , Vec2 900 900
            , Vec2 900 100
            ]

    let shards = runST $ do
            gen <- create
            shatterProcess
                gen
                (\shard -> polygonArea shard > 10000)
                (\cutResult -> minimum (polygonArea <$> cutResult) / maximum (polygonArea <$> cutResult) > 0.5)
                square

    for_ (zip [0..] shards) $ \(i, shard) -> do
        sketch shard
        setColor (mathematica97 i)
        Cairo.fill

shatterProcess
    :: GenST s
    -> (Polygon -> Bool)   -- ^ Recursively subdivide the current polygon?
    -> ([Polygon] -> Bool) -- ^ Accept the cut result, or retry with a different random cut line?
    -> Polygon             -- ^ Initial polygon, cut only if the recursion predicate applies
    -> ST s [Polygon]
shatterProcess _ recurse _ polygon
    | not (recurse polygon) = pure [polygon]
shatterProcess gen recurse acceptCut polygon = do
    cutPieces <- randomCut gen polygon
    let triangulated = concatMap triangulate cutPieces
    if acceptCut triangulated
        then do
            subcuts <- traverse (shatterProcess gen recurse acceptCut) triangulated
            pure (concat subcuts)
        else shatterProcess gen recurse acceptCut polygon

randomCut
    :: Gen s
    -> Polygon -- ^ Initial polygon, cut only if the recursion predicate applies
    -> ST s [Polygon]
randomCut gen polygon = do
    let BoundingBox vMin vMax = boundingBox polygon
    p <- uniformRM (vMin, vMax) gen
    angle <- uniformM gen
    let scissors = angledLine p angle 1
    pure (cutPolygon scissors polygon)

-- | Now let's add some randomness: The square should look somewhat "exploded",
-- meaning that we move and rotate the individual shards a little in some
-- random direction. You can make the amount and angle of the movement
-- dependent on the distance from the center, to make it look more realistic.
--
-- Feel free to experiment!
drawSquareShattered :: Int -> Int -> Cairo.Render ()
drawSquareShattered w h = do
    let scaleFactor = fromIntegral (max w h) / 1000
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ setColor white >> Cairo.paint
    Cairo.setLineWidth 5

    let square = Polygon
            [ Vec2 100 100
            , Vec2 100 900
            , Vec2 900 900
            , Vec2 900 100
            ]

    let shards = runST $ do
            gen <- create
            rawShards <- shatterProcess
                gen
                (\shard -> polygonArea shard > 10000)
                (\cutResult -> minimum (polygonArea <$> cutResult) / maximum (polygonArea <$> cutResult) > 0.5)
                square
            for rawShards $ \shard -> do
                let centroid = polygonCentroid shard
                    origin = Vec2 500 500
                    distanceFromOrigin = norm (centroid -. origin) / 500
                offset <- uniformRM (-0.1, 0.3) gen
                angle <- distanceFromOrigin *. deg <$> uniformRM (-20, 20) gen
                pure (transform (translate (offset *. (centroid -. origin)) <> rotateAround centroid angle) shard)

    for_ (zip [0..] shards) $ \(i, shard) -> do
        sketch shard
        setColor (mathematica97 i)
        Cairo.fill
