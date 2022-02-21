{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform)

import Draw
import Geometry
import Geometry.Shapes



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 720

main :: IO ()
main = withSurfaceAuto "out/haskell_logo_billard.svg" picWidth picHeight renderDrawing
  where
    renderDrawing surface = renderWith surface $ do
        Cairo.scale 2 2
        drawing

data BillardSpec = BillardSpec
    { steps :: Int
    , table :: [Line]
    , startPos :: Vec2
    , startAngle :: Angle
    }

runBillardSpec :: BillardSpec -> [Vec2]
runBillardSpec BillardSpec{..} =
    take steps (drop 1 (billard table (angledLine startPos startAngle 10)))

drawing :: Render ()
drawing = do
    let [left, lambda, upper, lower] = transform (Geometry.scale 340) haskellLogo
        billardLeft   = BillardSpec{ steps = 256, table = polygonEdges left,   startPos = Vec2 10  10,  startAngle = deg 40 }
        billardLambda = BillardSpec{ steps = 400, table = polygonEdges lambda, startPos = Vec2 230 175, startAngle = deg 40 }
        billardUpper  = BillardSpec{ steps = 120, table = polygonEdges upper,  startPos = Vec2 400 120, startAngle = deg 20 }
        billardLower  = BillardSpec{ steps = 120, table = polygonEdges lower,  startPos = Vec2 450 220, startAngle = deg 40 }
        billardSketch :: BillardSpec -> (Double -> AlphaColor Double) -> Render ()
        billardSketch spec color = do
            let points = runBillardSpec spec
                billardLines = zipWith Line points (tail points)
            let lengths = map lineLength billardLines
                (meanLength, sigmaLength) = meanStddev lengths
            for_ billardLines (\line ->
                let alpha = let d = lineLength line
                            in min 1 (max 0.4 (abs (d - meanLength) / (3*sigmaLength)))
                in setColor (color alpha) >> lineSketch line >> stroke)

    Cairo.translate 10 10

    setLineWidth 1
    billardSketch billardLeft (hsva 257 0.40 0.38) >> stroke
    setColor $ hsva 257 0.40 0.38 1
    sketch left >> stroke

    billardSketch billardLambda (hsva 256 0.40 0.50)
    setColor $ hsva 256 0.40 0.50 1
    sketch lambda >> stroke

    billardSketch billardUpper (hsva 304 0.45 0.56)
    billardSketch billardLower (hsva 304 0.45 0.56)
    setColor $ hsva 304 0.45 0.56 1
    sketch upper >> stroke
    sketch lower >> stroke

-- | Mean and standard deviation, calculated in a single pass. 😎
meanStddev :: [Double] -> (Double, Double)
meanStddev xs = (mu, sigma)
  where
    mu = total / count
    sigma = sqrt ((totalSquares - total*mu) / (count - 1))
    (count, total, totalSquares) = foldl'
        (\(!countAcc, !totalAcc, !totalSquaresAcc) x
            -> (countAcc+1, totalAcc+x, totalSquaresAcc + x*x))
        (0, 0, 0)
        xs
