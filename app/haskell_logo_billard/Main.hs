{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo hiding (transform)

import Draw
import Geometry
import Geometry.Shapes



picWidth, picHeight :: Num a => a
picWidth = 502
picHeight = 360

main :: IO ()
main = png >> svg
  where
    png = withPNGSurface "out/haskell_logo_billard.png" picWidth picHeight renderDrawing
    svg = withSVGSurface "out/haskell_logo_billard.svg" picWidth picHeight renderDrawing
    renderDrawing surface = renderWith surface drawing

data BillardSpec = BillardSpec
    { steps :: Int
    , table :: [Line]
    , startPos :: Vec2
    , startAngle :: Angle
    }

runBillardSpec :: BillardSpec -> [Vec2]
runBillardSpec BillardSpec{..} =
    take steps (drop 1 (billardProcess table (angledLine startPos startAngle (Distance 10))))

drawing :: Render ()
drawing = do
    let [left, lambda, upper, lower] = transform (scale' 340 340) haskellLogo
        billardLeft   = BillardSpec{ steps = 256, table = polygonEdges left,   startPos = Vec2 10  10,  startAngle = deg 40 }
        billardLambda = BillardSpec{ steps = 400, table = polygonEdges lambda, startPos = Vec2 230 175, startAngle = deg 40 }
        billardUpper  = BillardSpec{ steps = 120, table = polygonEdges upper,  startPos = Vec2 400 120, startAngle = deg 20 }
        billardLower  = BillardSpec{ steps = 120, table = polygonEdges lower,  startPos = Vec2 450 220, startAngle = deg 40 }
        billardSketch :: BillardSpec -> (Double -> Render ()) -> Render ()
        billardSketch spec setColor = do
            let points = runBillardSpec spec
                billardLines = zipWith Line points (tail points)
            let lengths = [d | Distance d <- map lineLength billardLines]
                (meanLength, sigmaLength) = meanStddev lengths
            for_ billardLines (\line ->
                let alpha = case lineLength line of
                        Distance d -> min 1 (max 0.4 (abs (d - meanLength) / (3*sigmaLength)))
                in setColor alpha >> lineSketch line >> stroke)

    translate 10 10

    setLineWidth 1
    billardSketch billardLeft (hsva 257 0.40 0.38) >> stroke
    hsva 257 0.40 0.38 1
    polygonSketch left >> stroke

    billardSketch billardLambda (hsva 256 0.40 0.50)
    hsva 256 0.40 0.50 1
    polygonSketch lambda >> stroke

    billardSketch billardUpper (hsva 304 0.45 0.56)
    billardSketch billardLower (hsva 304 0.45 0.56)
    hsva 304 0.45 0.56 1
    polygonSketch upper >> stroke
    polygonSketch lower >> stroke

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
