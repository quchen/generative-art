module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo  as Cairo hiding (x, y)
import qualified System.Random.MWC as MWC
import Control.Monad.ST
import qualified Data.Vector as V

import Draw
import Geometry as G
import Geometry.Shapes



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 720

haskellLogo' :: [Polygon]
haskellLogo' = G.transform (G.scale 340) haskellLogo

main :: IO ()
main = withSurfaceAuto "out/haskell_logo_shattered.svg" picWidth picHeight renderDrawing
  where
    renderDrawing surface = renderWith surface $ do
        Cairo.scale 2 2
        let recurse polygon = minMaxAreaRatio (polygon : haskellLogo') >= 1/64
            acceptCut polygons = minMaxAreaRatio polygons >= 1/3
            shattered = runST $ do
                gen <- MWC.initialize (V.fromList [])
                runShatterProcess gen recurse acceptCut haskellLogo'

        gen <- liftIO $ MWC.initialize (V.fromList [])
        drawing gen shattered

shatterProcessS
    :: MWC.GenST s
    -> (Polygon -> Bool)   -- ^ Recursively subdivide the current polygon?
    -> ([Polygon] -> Bool) -- ^ Accept the cut result, or retry with a different random cut line?
    -> Polygon             -- ^ Initial polygon, cut only if the recursion predicate applies
    -> ST s [Polygon]
shatterProcessS gen recurse acceptCut polygon
    | recurse polygon = do
        cutPieces <- randomCutS gen acceptCut polygon
        let triangulated = concatMap triangulate cutPieces
        if acceptCut triangulated
            then fmap concat (traverse (shatterProcessS gen recurse acceptCut) triangulated)
            else shatterProcessS gen recurse acceptCut polygon
    | otherwise = pure [polygon]

runShatterProcess
    :: MWC.GenST s
    -> (Polygon -> Bool)
    -> ([Polygon] -> Bool)
    -> [Polygon]
    -> ST s [Polygon]
runShatterProcess gen recurse acceptCut initialPolygon
  = fmap concat (traverse (shatterProcessS gen recurse acceptCut) initialPolygon)

polygonCenter :: Polygon -> Vec2
polygonCenter (Polygon corners) = foldl' (+.) (Vec2 0 0) corners /. fromIntegral (length corners)

randomCutS
    :: MWC.Gen s
    -> ([Polygon] -> Bool) -- ^ Accept the cut result, or retry with a different random cut line?
    -> Polygon             -- ^ Initial polygon, cut only if the recursion predicate applies
    -> ST s [Polygon]
randomCutS gen acceptCut polygon = findGoodCut
  where
    BoundingBox vMin vMax = boundingBox polygon

    findGoodCut = do
        p <- MWC.uniformRM (vMin, vMax) gen
        angle <- fmap deg (MWC.uniformRM (0, 360) gen)
        let scissors = angledLine p angle (Distance 1)
            cutResult = cutPolygon scissors polygon
        if acceptCut cutResult
            then pure cutResult
            else findGoodCut

-- | Calculate the min/max ratio of the areas of a list of polygons. Useful to
-- build cutoff predicates with, e.g.
--
-- @
-- \polys -> 'minMaxAreaRatio' polys >= 1/3
-- @
minMaxAreaRatio :: [Polygon] -> Double
minMaxAreaRatio cutResult
  = let cutResultAreas = map polygonArea cutResult
        Area minA = minimum cutResultAreas
        Area maxA = maximum cutResultAreas
    in minA / maxA

drawing :: Foldable t => MWC.GenIO -> t Polygon -> Render ()
drawing gen shattered = do
    Cairo.translate 10 10
    setLineWidth 0.5
    cairoScope $ for_ shattered $ \polygon -> do
        let hue = 30
        saturation <- liftIO $ MWC.uniformRM (0.2, 0.7) gen
        let value = 1
        let alpha = 1
        angle <- liftIO $ fmap deg (MWC.uniformRM (-15, 15) gen)
        setColor (hsva hue saturation value alpha)
        polygonSketch (G.transform (G.rotateAround (polygonCenter polygon) angle) polygon)
        fillPreserve
        setColor black
        stroke
