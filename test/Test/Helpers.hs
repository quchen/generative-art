{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Helpers (
      GaussianVec(..)
    , LotsOfGaussianPoints(..)

    , EqApprox(..)
    , Tolerance(..)
    , (~==)

    , assertThrowsError
) where



import Data.List
import System.Random
import Data.Colour.RGBSpace as Colour
import Data.Colour.SRGB as Colour
import Control.Exception

import Geometry
import Draw.Color
import Geometry.Coordinates.Hexagonal as Hex

import Test.Tasty.QuickCheck
import Test.Tasty.HUnit



instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y) = [ Vec2 x' y' | x' <- [0,1,-1], y' <- [0,1,-1] ]
                     ++ [ Vec2 x' y' | (x', y') <- shrink (x, y) ]

instance Arbitrary Angle where
    arbitrary = fmap deg (choose (-360, 720))

newtype GaussianVec = GaussianVec Vec2
    deriving (Eq, Ord, Show)

instance Arbitrary GaussianVec where
    arbitrary = do
        u1 <- choose (0,1)
        if u1 == 0 -- to avoid diverging on log(0)
            then arbitrary
            else do
                u2 <- choose (0, 2*pi)
                let root1 = sqrt (-2 * log u1)
                    x = root1 * cos u2
                    y = root1 * sin u2
                pure (GaussianVec (Vec2 x y))
    shrink (GaussianVec vec) = map GaussianVec (shrink vec)

instance Arbitrary Hex where
    arbitrary = do
        q <- arbitrary
        r <- arbitrary
        pure (Hex q r (-q-r))
    shrink (Hex 0 0 0) = []
    shrink hex =
        let r = Hex.distance hexZero hex
        in ring (r-1) hexZero

newtype LotsOfGaussianPoints = LotsOfGaussianPoints [Vec2]
    deriving (Eq, Ord, Show)

instance Arbitrary LotsOfGaussianPoints where
    arbitrary = do
        seed <- arbitrary
        numPoints <- choose (10, 100)
        (pure . LotsOfGaussianPoints . take numPoints . gaussianVecs) seed
    shrink (LotsOfGaussianPoints xs)
      = let -- Some sufficiently chaotic seed function :-)
            seed = foldl' (\s vec -> round (normSquare vec) + s*s) 0 xs
        in map (\numPoints -> LotsOfGaussianPoints (take numPoints (gaussianVecs seed)))
               [3 .. length xs-1]

gaussianVecs :: Int -> [Vec2]
gaussianVecs seed
  = let go (u1:rest)
            | u1 <= 0 = go rest -- to avoid diverging on log(0)
        go (u1:u2:rest)
            = let root1 = sqrt (-2 * log u1)
                  pi2u2 = 2 * pi * u2
                  x = root1 * cos pi2u2
                  y = root1 * sin pi2u2
              in Vec2 x y : go rest
        go _ = error "Can’t happen, input is infinite"
    in go (randomRs (0, 1) (mkStdGen seed))
newtype Tolerance = Tolerance Double

class EqApprox a where
    approxEqualTolerance :: Tolerance -> a -> a -> Bool

infix 4 ~==
(~==) :: EqApprox a => a -> a -> Bool
(~==) = approxEqualTolerance (Tolerance 1e-10)

instance (EqApprox a, EqApprox b) => EqApprox (a,b) where
    approxEqualTolerance (Tolerance tol) (a1, b1) (a2, b2) = and
        [ approxEqualTolerance (Tolerance tol) a1 a2
        , approxEqualTolerance (Tolerance tol) b1 b2 ]

instance (EqApprox a, EqApprox b, EqApprox c) => EqApprox (a,b,c) where
    approxEqualTolerance (Tolerance tol) (a1, b1, c1) (a2, b2, c2) = and
        [ approxEqualTolerance (Tolerance tol) a1 a2
        , approxEqualTolerance (Tolerance tol) b1 b2
        , approxEqualTolerance (Tolerance tol) c1 c2 ]

instance (EqApprox a, EqApprox b, EqApprox c, EqApprox d) => EqApprox (a,b,c,d) where
    approxEqualTolerance (Tolerance tol) (a1, b1, c1, d1) (a2, b2, c2, d2) = and
        [ approxEqualTolerance (Tolerance tol) a1 a2
        , approxEqualTolerance (Tolerance tol) b1 b2
        , approxEqualTolerance (Tolerance tol) c1 c2
        , approxEqualTolerance (Tolerance tol) d1 d2 ]

instance (EqApprox a, EqApprox b, EqApprox c, EqApprox d, EqApprox e) => EqApprox (a,b,c,d,e) where
    approxEqualTolerance (Tolerance tol) (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) = and
        [ approxEqualTolerance (Tolerance tol) a1 a2
        , approxEqualTolerance (Tolerance tol) b1 b2
        , approxEqualTolerance (Tolerance tol) c1 c2
        , approxEqualTolerance (Tolerance tol) d1 d2
        , approxEqualTolerance (Tolerance tol) e1 e2 ]

instance EqApprox Double where
    approxEqualTolerance (Tolerance tol) reference value
      = abs (reference - value) <= tol

instance EqApprox Vec2 where
    approxEqualTolerance (Tolerance tol) v1 v2
      = norm (v2 -. v1) <= tol

instance EqApprox Angle where
    approxEqualTolerance tol x y
      = let x' = getRad x
            y' = getRad y
            -- If x and y are just around 0°/360° within the tolerance interval,
            -- the angles above will be apart by 2π, so we check the π rotated
            -- version here as well, avoiding the instability around 0.
            x'' = getRad (x +. rad pi)
            y'' = getRad (y +. rad pi)
        in approxEqualTolerance tol x' y' || approxEqualTolerance tol x'' y''

instance EqApprox Transformation where
    approxEqualTolerance tol
        (Transformation a1 b1 c1 d1 e1 f1)
        (Transformation a2 b2 c2 d2 e2 f2)
      = all (\(x,y) -> approxEqualTolerance tol x y)
            [ (a1, a2)
            , (b1, b2)
            , (c1, c2)
            , (d1, d2)
            , (e1, e2)
            , (f1, f2) ]

instance (Real a, EqApprox a) => EqApprox (Colour a) where
    approxEqualTolerance tol s t =
        let toTuple = uncurryRGB (\r g b -> (r,g,b::Double)) . toSRGB . colourConvert
        in approxEqualTolerance tol (toTuple s) (toTuple t)


instance (Real a, Floating a, EqApprox a) => EqApprox (AlphaColour a) where
    approxEqualTolerance tol s t =
        let toTuple :: (Real a, Floating a) => AlphaColor a -> (a,a,a,a)
            toTuple color =
                let alpha = alphaChannel color
                in uncurryRGB (\r g b -> (r,g,b, realToFrac alpha)) (toSRGB (colourConvert (dissolve (1/alpha) color `over` black)))
        in approxEqualTolerance tol (toTuple s) (toTuple t)

assertThrowsError :: (String -> Bool) -> a -> IO ()
assertThrowsError p x = handle
    (\(ErrorCallWithLocation err _loc) -> if p err
        then pure ()
        else assertFailure ("An ErrorCall was raised, but not with the right contents. Received: " ++ err)
        )
    (evaluate x >> assertFailure "Expected ErrorCall, but none was raised")
