module Main (main) where



import Control.Monad.Trans.State
import Data.Foldable
import Graphics.Rendering.Cairo  as Cairo hiding (x, y)
import System.Random

import Draw
import Geometry as G
import Geometry.Processes.RandomCut
import Geometry.Shapes



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 720

haskellLogo' :: [Polygon]
haskellLogo' = G.transform (G.scale 340) haskellLogo

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
    setLineWidth 0.5
    cairoScope $ for_ shattered $ \polygon -> do
        let gen = let (x,y) = decodeFloat (polygonArea polygon)
                  in mkStdGen (fromIntegral x + y)
            hue = 30
            (saturation, gen1) = randomR (0.2, 0.7) gen
            value = 1
            alpha = 1
            (displacementX, gen2) = randomR (-1, 1) gen1
            (displacementY, gen3) = randomR (-1, 1) gen2
            displacement = Vec2 displacementX displacementY
            (rotateDegrees, _gen4) = randomR (-15, 15) gen3
        setColor (hsva hue saturation value alpha)
        polygonSketch (G.transform (G.translate displacement <> G.rotateAround (polygonCenter polygon) (deg rotateDegrees)) polygon)
        fillPreserve
        setColor black
        stroke

polygonCenter :: Polygon -> Vec2
polygonCenter (Polygon corners) = foldl' (+.) (Vec2 0 0) corners /. fromIntegral (length corners)
