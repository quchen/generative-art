module Main (main) where



import Control.Monad
import Data.Foldable
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import Text.Printf

import Draw
import Geometry
import Geometry.Algorithms.Culling
import Geometry.Chaotic



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 600

scaleFactor :: Double
scaleFactor = 1

resolution :: Int
resolution = 100

ball :: Double -> Vec3 -> Vec3 -> Double
ball radius center q = (radius^2 / normSquare (center -. q))**1.7

main :: IO ()
main = do
    let count = 100
        seed = 0 :: Int
    gen <- initializeMwc seed
    centers <- replicateM count (uniformRM (Vec3 (-300) (-300) (-300), Vec3 300 300 300) gen)
    radii <- replicateM count (uniformRM (25, 75) gen)

    let metaballField :: Vec3 -> Double
        metaballField q
            | norm q > 200 = 0
            | otherwise    = vsum (zipWith ball radii centers) q
        file = printf "out/marching_cubes_%03d.svg" seed
        scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)
        grid = Grid3 (negateV (Vec3 300 300 300), Vec3 300 300 300) (resolution, resolution, resolution)
        components = isoSurfaces grid metaballField 1
        normal = Vec3 1 1 1
        resultingPolys = occlusionCullingProjection normal (components >>= backfaceCulling normal)

    putStrLn $ "Components: " ++ show (length components)
    putStrLn $ "Total triangles: " ++ show (length resultingPolys)

    render file scaledWidth scaledHeight $ do
        C.scale scaleFactor scaleFactor
        C.translate 300 300
        cairoScope (setColor (magma 0.05) >> C.paint)

        for_ resultingPolys $ \tri -> cairoScope $ do
            sketch tri
            setColor (inferno 0.75)
            C.setLineWidth 0.5
            C.stroke
    writeSTL (printf "out/marching_cubes_%03d.stl" seed) components
