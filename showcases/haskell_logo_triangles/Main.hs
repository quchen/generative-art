module Main (main) where



import Control.Monad.Trans.State
import Data.Foldable
import Graphics.Rendering.Cairo  as Cairo hiding (x, y)
import System.Random

import Draw
import Geometry
import Geometry.Processes.RandomCut
import Geometry.Shapes



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 720

haskellLogo' :: [Polygon]
haskellLogo' = Geometry.transform (Geometry.scale 340) haskellLogo

main :: IO ()
main = withSurfaceAuto "out/haskell_logo_triangles.svg" picWidth picHeight renderDrawing
  where
    renderDrawing surface = renderWith surface $ do
        Cairo.scale 2 2
        drawing

shatterProcessS
    :: (Polygon -> Bool)   -- ^ Recursively subdivide the current polygon?
    -> ([Polygon] -> Bool) -- ^ Accept the cut result, or retry with a different random cut line?
    -> Polygon             -- ^ Initial polygon, cut only if the recursion predicate applies
    -> State StdGen [Polygon]
shatterProcessS recurse acceptCut polygon
    | recurse polygon = do
        cutPieces <- randomCutS acceptCut polygon
        let triangulated = concatMap triangulate cutPieces
        if acceptCut triangulated
            then fmap concat (traverse (shatterProcessS recurse acceptCut) triangulated)
            else shatterProcessS recurse acceptCut polygon
    | otherwise = pure [polygon]

runShatterProcess
    :: (Polygon -> Bool)
    -> ([Polygon] -> Bool)
    -> [Polygon]
    -> StdGen
    -> [Polygon]
runShatterProcess recurse acceptCut initialPolygon gen
  = evalState (fmap concat (traverse (shatterProcessS recurse acceptCut) initialPolygon)) gen

drawing :: Render ()
drawing = do
    let recurse polygon = minMaxAreaRatio (polygon : haskellLogo') >= 1/64
        acceptCut polygons = minMaxAreaRatio polygons >= 1/3
        shattered = runShatterProcess recurse acceptCut haskellLogo' (mkStdGen 16)
    Cairo.translate 10 10
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    cairoScope $ for_ shattered $ \polygon -> do
        let gen = let Area a = polygonArea polygon
                      (x,y) = decodeFloat a
                  in mkStdGen (fromIntegral x + y)
            (hue, gen1) = randomR (200, 215) gen
            (saturation, gen2) = randomR (0.6, 0.9) gen1
            (value, _gen3) = randomR (0.6, 1.0) gen2
            alpha = 1
        setColor $ hsva hue saturation value alpha
        polygonSketch polygon
        fillPreserve
        stroke
    cairoScope $ for_ haskellLogo' $ \polygon -> do
        polygonSketch polygon
        setLineJoin LineJoinRound
        setColor $ hsva 0 0 0 1
        stroke
