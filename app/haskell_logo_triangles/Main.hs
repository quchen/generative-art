module Main (main) where



import Control.Monad.Trans.State
import Data.Foldable
import Graphics.Rendering.Cairo  hiding (x, y)
import System.Random

import Comparison
import Draw
import Geometry.Core
import Geometry.Processes.RandomCut
import Geometry.Triangulate



picWidth, picHeight :: Num a => a
picWidth = 502
picHeight = 360

main :: IO ()
main = png >> svg
  where
    png = do
        surface <- createImageSurface FormatARGB32 picWidth picHeight
        renderWith surface drawing
        surfaceWriteToPNG surface "out/haskell_logo_triangles.png"
    svg = withSVGSurface "out/haskell_logo_triangles.svg" picWidth picHeight (\surface -> renderWith surface drawing)

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

haskellLogo :: [Polygon]
haskellLogo = [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625]
    lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625]
    upper  = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625]
    lower  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312]

drawing :: Render ()
drawing = do
    let recurse polygon = minMaxAreaRatio (polygon : haskellLogo) >= 1/64
        acceptCut polygons = minMaxAreaRatio polygons >= 1/3
        shattered = runShatterProcess recurse acceptCut haskellLogo (mkStdGen 6)
    translate 10 10
    setLineCap LineCapRound
    setLineJoin LineJoinRound
    restoreStateAfter $ for_ shattered $ \polygon -> do
        let gen = let Area a = polygonArea polygon
                      (x,y) = decodeFloat a
                  in mkStdGen (fromIntegral x + y)
            (hue, gen1) = randomR (200, 215) gen
            (saturation, gen2) = randomR (0.6, 0.9) gen1
            (value, _gen3) = randomR (0.6, 1.0) gen2
            alpha = 1
        hsva hue saturation value alpha
        polygonSketch polygon
        fillPreserve
        stroke
    restoreStateAfter $ for_ haskellLogo $ \polygon -> do
        polygonSketch polygon
        setLineJoin LineJoinRound
        hsva 0 0 0 1
        stroke
