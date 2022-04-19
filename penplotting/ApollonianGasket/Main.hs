module Main (main) where



import           Data.Coerce
import           Data.Default.Class
import           Data.Foldable
import qualified Data.Text.Lazy.IO                   as T
import           Data.Tree
import           Draw.Plotting
import           Geometry                            as G
import           Geometry.Processes.ApollonianGasket



minRadius :: Double
minRadius = 0.5

gen0L, gen0R, gen0B :: Circle
gen0L = Circle (Vec2 100 100) 50
gen0R = Circle (Vec2 200 100) 50
gen0B = Circle (G.transform (G.rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 50

unsafelyTransform :: Transformation -> Tree Circle -> Tree Circle
unsafelyTransform t = coerce . (fmap (transform t) :: Tree UnsafeTransformCircle -> Tree UnsafeTransformCircle) . coerce

gasket :: Tree Circle
gasket = createGasket minRadius gen0L gen0R gen0B

pageWidth, pageHeight, margin :: Double
pageWidth = 210
pageHeight = 291
margin = 10

gasketScaled :: Tree Circle
gasketScaled = unsafelyTransform (G.transformBoundingBox (foldMap boundingBox gasket) (Vec2 margin margin, Vec2 pageWidth pageHeight -. Vec2 margin margin) def) gasket

plotterSettings :: PlottingSettings
plotterSettings = def { _feedrate = Just 1000 }

main :: IO ()
main = T.putStrLn (renderGCode (_plotGCode (runPlot plotterSettings drawing)))
  where
    drawing = plot (toList gasketScaled)
