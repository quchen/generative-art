module Main (main) where



import           Control.Monad.ST
import           Data.Foldable
import           Data.Traversable
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as Cairo hiding (x, y)
import qualified System.Random.MWC        as MWC

import Draw
import Geometry               as G
import Geometry.Shapes
import Numerics.Interpolation
import qualified Geometry.Chaotic as Chaos



main :: IO ()
main = do
    withSurfaceAuto "out/haskell_logo_shattered.svg" picWidth picHeight (\surface -> renderWith surface renderDrawing)
    withSurfaceAuto "out/haskell_logo_shattered.png" picWidth picHeight $ \surface -> renderWith surface $ do
        cairoScope $ do
            setColor white
            paint
        renderDrawing
  where
    picWidth = 1000
    picHeight = 720
    shattered = runST $ do
        let recurse polygon = minMaxAreaRatio (polygon : haskellLogo) >= 1/128
            acceptCut polygons = minMaxAreaRatio polygons >= 1/3
        gen <- MWC.initialize (V.fromList [1])
        cutUpLogoParts <- for haskellLogo (shatterProcess gen recurse acceptCut)
        let BoundingBox (Vec2 xMin _) (Vec2 xMax _) = boundingBox cutUpLogoParts
        for (concat cutUpLogoParts) $ \polygon -> do
            let Vec2 x _ = polygonCenter polygon
                wiggleAmount = linearInterpolate (xMin, xMax) (0, 40) x
            angle <- fmap deg (MWC.uniformRM (-wiggleAmount, wiggleAmount) gen)
            pure (G.transform (G.rotateAround (polygonCenter polygon) angle) polygon)

    renderDrawing = do
        gen <- liftIO $ Chaos.initializeMwc shattered
        let fitToCanvas = G.transform (transformBoundingBox shattered (Vec2 10 10, Vec2 (fromIntegral picWidth-10) (fromIntegral picHeight-10)) def)
        drawing gen (fitToCanvas shattered)

shatterProcess
    :: MWC.GenST s
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

polygonCenter :: Polygon -> Vec2
polygonCenter (Polygon corners) = foldl' (+.) zero corners /. fromIntegral (length corners)

randomCut
    :: MWC.Gen s
    -> Polygon -- ^ Initial polygon, cut only if the recursion predicate applies
    -> ST s [Polygon]
randomCut gen polygon = do
    let BoundingBox vMin vMax = boundingBox polygon
    p <- MWC.uniformRM (vMin, vMax) gen
    angle <- MWC.uniformM gen
    let scissors = angledLine p angle 1
    pure (cutPolygon scissors polygon)

-- | Calculate the min/max ratio of the areas of a list of polygons. Useful to
-- build cutoff predicates with, e.g.
--
-- @
-- \polys -> 'minMaxAreaRatio' polys >= 1/3
-- @
minMaxAreaRatio :: [Polygon] -> Double
minMaxAreaRatio cutResult
  = let cutResultAreas = map polygonArea cutResult
        minA = minimum cutResultAreas
        maxA = maximum cutResultAreas
    in minA / maxA

drawing :: MWC.GenIO -> [Polygon] -> Render ()
drawing gen shattered = do
    setLineWidth 0.5
    cairoScope $ for_ shattered $ \polygon -> do
        let hue = 30
        saturation <- liftIO $ MWC.uniformRM (0.2, 0.7) gen
        let value = 1
        let alpha = 1
        setColor (hsva hue saturation value alpha)
        polygonSketch polygon
        fillPreserve
        setColor black
        stroke
