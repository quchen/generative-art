module Main (main) where



import           Data.Foldable
import qualified Data.Text.Lazy.IO                   as T
import           Data.Tree
import           Draw.GCode
import           Geometry                            as G
import           Geometry.Processes.ApollonianGasket



scaleFactor :: Double
scaleFactor = 0.5

gen0L, gen0R, gen0B :: Circle
gen0L = Circle (Vec2 (100*scaleFactor) (100*scaleFactor)) (50*scaleFactor)
gen0R = Circle (Vec2 (200*scaleFactor) (100*scaleFactor)) (50*scaleFactor)
gen0B = Circle (G.transform (G.rotateAround (Vec2 (100*scaleFactor) (100*scaleFactor)) (deg 60)) (Vec2 (200*scaleFactor) (100*scaleFactor))) (50*scaleFactor)

gasket :: Tree Circle
gasket = createGasket 1 gen0L gen0R gen0B

main :: IO ()
main = T.putStrLn (renderGCode (toGCode (toList gasket)))
