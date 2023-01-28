{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where



import Control.Monad.Primitive
import Data.List ( sortOn, (\\) )
import Data.Maybe ( fromMaybe, isJust, catMaybes )
import Data.Ord ( comparing )
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as C
import System.Random.MWC ( initialize, uniformRM, GenIO, create, Gen )

import Draw
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Voronoi
import Geometry.Algorithms.Sampling
import Control.Monad (replicateM, guard)
import Control.Applicative (Applicative(liftA2))
import Debug.Trace
import Numerics.VectorAnalysis (grad, divergence)
import Numerics.DifferentialEquation (rungeKuttaAdaptiveStep)
import Geometry.Algorithms.Voronoi (Voronoi(_voronoiCells))
import Data.List.Extended (nubOrd)
import Control.Monad.ST
import Data.Traversable



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

canvas :: BoundingBox
canvas = boundingBox [zero, Vec2 picWidth picHeight]

file :: FilePath
file = "out/warped-tiles.png"

main :: IO ()
main = render file picWidth picHeight $ do
    cairoScope (setColor white >> C.paint)
    setColor black
    C.setLineWidth 1
    let iso = isoLines GridSpec { _range = (zero, Vec2 picWidth picHeight), _maxIndex = (256, 144)} potential
        potentialLines = 
            [ Polyline isoline
            | z <- [-50, -49.7 .. 20]
            , isoline@(p:_) <- iso z
            , and [ norm (p -. p') > 20 | (p', _) <- charges ]
            ]
        fieldLines = sampleFieldLines
        intersectionPoints = nubOrd $ do
            pl <- potentialLines
            fl <- fieldLines
            lineIntersections pl fl
        triangles = bowyerWatson canvas (intersectionPoints ++ (fst <$> charges))
        cells = _voronoiCells $ toVoronoi triangles
    --for_ potentialLines $ \l -> do
    --    sketch l
    --    stroke
    --for_ fieldLines $ \l -> cairoScope $ do
    --    setColor (black `withOpacity` 0.3)
    --    sketch l
    --    stroke
    for_ (getPolygons triangles) $ \poly -> do
        sketch poly
        C.stroke
    -- for_ cells $ \VoronoiCell{..} -> do
    --     let area = polygonArea _voronoiRegion
    --     sketch $ chaikin 0.25 $ chaikin 0.25 $ chaikin 0.1 $ growPolygon (-0.1 * sqrt area) _voronoiRegion
    --     C.fill

chaikin :: Double -> Polygon -> Polygon
chaikin _ (Polygon []) = Polygon []
chaikin lambda (Polygon ps@(p:_)) = Polygon $ concat
    [ [c, b]
    | (a, d) <- zip ps (tail ps ++ [p])
    , let b = lambda *. a +. (1-lambda) *. d
    , let c = (1-lambda) *. a +. lambda *. d
    ]

charges :: [(Vec2, Double)]
charges = traceShowId $ runST $ do
    gen <- create
    ps <- poissonDisc gen PoissonDiscParams { _poissonK = 10, _poissonShape = canvas, _poissonRadius = 800 }
    for ps $ \p -> do
        q <- pick gen [-1, -0.5, 0.5, 1]
        pure (p, q)

pick :: PrimMonad m => Gen (PrimState m) -> [a] -> m a
pick gen xs = do
    i <- uniformRM (0, length xs - 1) gen
    pure (xs !! i)

potential :: Vec2 -> Double
potential p = sum [ q * log (norm (p -. p')) | (p', q) <- charges ]

vectorField :: Vec2 -> Vec2
vectorField = grad potential

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = takeWhile (not . p)

lineIntersections :: Polyline [] -> Polyline [] -> [Vec2]
lineIntersections (Polyline xs) (Polyline ys) = 
    [ p
    | l1 <- zipWith Line xs (tail xs)
    , l2 <- zipWith Line ys (tail ys)
    , p <- case intersectionLL l1 l2 of
        IntersectionReal p -> [p]
        _otherwise -> []
    ]

sampleFieldLines :: [Polyline []]
sampleFieldLines = go charges [] []
  where
    go :: [(Vec2, Double)] -> [((Vec2, Double), Polyline [])] -> [Polyline []] -> [Polyline []]
    go [] _ fls = fls
    go ((p, q):cs) sourcePoints fls' =
        go cs' (catMaybes targetPoints ++ sourcePoints) (fls' ++ fls)
      where
        deltaPhi = 24 / abs q
        psis =
            [ angleOfLine (Line p' (last fl'))
            | ((p', _), Polyline fl') <- sourcePoints
            , p' == p
            ]
        phi_0 = case psis of
            [] -> deg 0
            _  -> deg (sum (fmap ((`fmod` deltaPhi) . getDeg) psis) / fromIntegral (length psis))
        (fls, targetPoints) = unzip
            [ (fl, fmap (, fl) maybeP')
            | phi <- (phi_0 +.) . deg <$> [0, deltaPhi .. 360]
            , let startingPoint = p +. polar phi 10
            , let vf = if q > 0 then vectorField else negateV vectorField
            , let (fl, maybeP') = fieldLine vf startingPoint
            , and [ case normalizeAngle zero (phi -. psi) of
                    alpha | getDeg alpha < 5 -> False
                          | getDeg alpha > 355 -> False
                          | otherwise -> True
                  | psi <- psis
                  ]
            ]
        cs' = case fst <$> catMaybes targetPoints of
            (p', q'):_ | (p', q') `elem` cs -> (p', q') : (cs \\ [(p', q')])
            _otherwise -> cs


fmod :: Double -> Double -> Double
a `fmod` b = let c = a / b in (c - fromIntegral (floor c)) * b


fieldLine :: (Vec2 -> Vec2) -> Vec2 -> (Polyline [], Maybe (Vec2, Double))
fieldLine vf p = (Polyline (fst <$> fl), snd (last fl))
  where
    trajectory = snd <$> rungeKuttaAdaptiveStep (const vf) p 0 0.1 normSquare 0.0001
    fl = takeWhile ((`insideBoundingBox` canvas) . fst) $ takeUntil1 (isJust . snd) $ dropWhile (isJust . snd) $ fmap (\p -> (p, closeToCharge p)) trajectory

takeUntil1 :: (a -> Bool) -> [a] -> [a]
takeUntil1 _ [] = []
takeUntil1 p (x:xs)
    | p x = [x]
    | otherwise = x : takeUntil1 p xs

closeToCharge :: Vec2 -> Maybe (Vec2, Double)
closeToCharge p = find (\(p', _) -> norm (p -. p') < 10) charges
