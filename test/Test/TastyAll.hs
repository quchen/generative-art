module Test.TastyAll (
    -- * HUnit extensions
      assertThrowsError
    , Expected(..)
    , ExpectedWithin(..)
    , Actual(..)
    , assertEqual
    , assertApproxEqual
    , (~==)
    , approxEqual
    , Tolerance(..)

    -- * Rendering pictures
    , renderPng
    , renderSvg
    , renderAllFormats
    , testVisual

    -- * Reexports
    , module Test.Arbitrary
    , module Test.Tasty
    , module Test.Tasty.HUnit
    , module Test.Tasty.QuickCheck
) where



import Control.Exception
import Data.Colour.RGBSpace     as Colour
import Data.Colour.SRGB         as Colour
import Data.List
import Draw
import Graphics.Rendering.Cairo as C hiding (x, y)

import Geometry

import           Test.Arbitrary
import           Test.Tasty
import           Test.Tasty.HUnit      hiding (assertEqual)
import qualified Test.Tasty.HUnit      as HUnit
import           Test.Tasty.QuickCheck



assertThrowsError
    :: (String -> Bool) -- ^ Predicate on the error message (e.g. check for substring)
    -> a                -- ^ Value whose WHNF raises an 'error'
    -> IO ()            -- ^ Succeeds iff the value raises an 'error' with a certain content
assertThrowsError p x = handle
    (\(ErrorCallWithLocation err _loc) -> if p err
        then pure ()
        else assertFailure ("An ErrorCall was raised, but not with the right contents. Received: " ++ err)
        )
    (evaluate x >> assertFailure "Expected ErrorCall, but none was raised")

-- | Type-safe wrapper for 'assertEqual'
newtype Expected a = Expected a
    deriving (Eq, Ord)

instance Show a => Show (Expected a) where
    show (Expected x) = "Expected: " ++ show x

-- | Type-safe wrapper for 'assertEqual', 'assertApproxEqual'
newtype Actual a = Actual a
    deriving (Eq, Ord)

instance Show a => Show (Actual a) where
    show (Actual x) = "Actual: " ++ show x

-- | Type-safe wrapper for 'assertApproxEqual'
data ExpectedWithin a = ExpectedWithin !Double a
    deriving (Eq, Ord)

instance Show a => Show (ExpectedWithin a) where
    show (ExpectedWithin tol x) = "Expected within tolerance " ++ show tol ++ ": " ++ show x

-- | Type-safe version of 'HUnit.assertEqual', because it’s too easy to mix up the arguments.
assertEqual :: (Eq a, Show a) => String -> Expected a -> Actual a -> Assertion
assertEqual errMsg (Expected e) (Actual a) = HUnit.assertEqual errMsg e a

assertApproxEqual
    :: (EqApprox a, Show a)
    => String           -- ^ Error message
    -> ExpectedWithin a -- ^ Expected value, within a tolerance
    -> Actual a         -- ^ Actual value
    -> Assertion
assertApproxEqual errMsg (ExpectedWithin tol expected) (Actual actual) = HUnit.assertBool expectedButGot (approxEqual (Tolerance tol) expected actual)
  where
    preface = if null errMsg then Nothing else Just errMsg
    expectedButGot = intercalate "\n" . maybe id (:) preface $ ["expected (within tolerance " ++ show tol ++ "): " ++ show expected, " but got: " ++ show actual]

renderPng :: Int -> Int -> FilePath -> Render () -> IO ()
renderPng picWidth picHeight filename drawing = withSurface PNG filename picWidth picHeight $ \surface ->
    renderWith surface $ do
        cairoScope $ do
            rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
            setColor black
            setLineWidth 0
            fill
        drawing

renderSvg :: Int -> Int -> FilePath -> Render () -> IO ()
renderSvg picWidth picHeight filename drawing
  = withSurface SVG filename w h (\surface -> renderWith surface drawing)
  where
    w = fromIntegral picWidth
    h = fromIntegral picHeight

renderAllFormats :: Int -> Int -> FilePath -> Render () -> IO ()
renderAllFormats w h filename drawing = do
    renderPng w h (filename ++ ".png") $ do
        cairoScope $ do
            setColor white
            paint
        drawing
    renderSvg w h (filename ++ ".svg") drawing

newtype Tolerance = Tolerance Double

class EqApprox a where
    approxEqual :: Tolerance -> a -> a -> Bool

-- | Useful shorthand for 'approxEqual' with a default tolerance.
(~==) :: EqApprox a => a -> a -> Bool
(~==) = approxEqual (Tolerance 1e-10)
infix 4 ~==

instance (EqApprox a, EqApprox b) => EqApprox (a,b) where
    approxEqual (Tolerance tol) (a1, b1) (a2, b2) = and
        [ approxEqual (Tolerance tol) a1 a2
        , approxEqual (Tolerance tol) b1 b2 ]

instance (EqApprox a, EqApprox b, EqApprox c) => EqApprox (a,b,c) where
    approxEqual (Tolerance tol) (a1, b1, c1) (a2, b2, c2) = and
        [ approxEqual (Tolerance tol) a1 a2
        , approxEqual (Tolerance tol) b1 b2
        , approxEqual (Tolerance tol) c1 c2 ]

instance (EqApprox a, EqApprox b, EqApprox c, EqApprox d) => EqApprox (a,b,c,d) where
    approxEqual (Tolerance tol) (a1, b1, c1, d1) (a2, b2, c2, d2) = and
        [ approxEqual (Tolerance tol) a1 a2
        , approxEqual (Tolerance tol) b1 b2
        , approxEqual (Tolerance tol) c1 c2
        , approxEqual (Tolerance tol) d1 d2 ]

instance (EqApprox a, EqApprox b, EqApprox c, EqApprox d, EqApprox e) => EqApprox (a,b,c,d,e) where
    approxEqual (Tolerance tol) (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) = and
        [ approxEqual (Tolerance tol) a1 a2
        , approxEqual (Tolerance tol) b1 b2
        , approxEqual (Tolerance tol) c1 c2
        , approxEqual (Tolerance tol) d1 d2
        , approxEqual (Tolerance tol) e1 e2 ]

instance EqApprox Double where
    approxEqual (Tolerance tol) reference value
      = abs (reference - value) <= tol

instance EqApprox Vec2 where
    approxEqual (Tolerance tol) v1 v2
      = norm (v2 -. v1) <= tol

instance EqApprox Angle where
    approxEqual tol x y
      = let x' = getRad x
            y' = getRad y
            -- If x and y are just around 0°/360° within the tolerance interval,
            -- the angles above will be apart by 2π, so we check the π rotated
            -- version here as well, avoiding the instability around 0.
            x'' = getRad (x +. rad pi)
            y'' = getRad (y +. rad pi)
        in approxEqual tol x' y' || approxEqual tol x'' y''

instance EqApprox Transformation where
    approxEqual tol
        (Transformation a1 b1 c1 d1 e1 f1)
        (Transformation a2 b2 c2 d2 e2 f2)
      = all (\(x,y) -> approxEqual tol x y)
            [ (a1, a2)
            , (b1, b2)
            , (c1, c2)
            , (d1, d2)
            , (e1, e2)
            , (f1, f2) ]

instance (Real a, EqApprox a) => EqApprox (Colour a) where
    approxEqual tol s t =
        let toTuple = uncurryRGB (\r g b -> (r,g,b::Double)) . toSRGB . colourConvert
        in approxEqual tol (toTuple s) (toTuple t)

instance (Real a, Floating a, EqApprox a) => EqApprox (AlphaColour a) where
    approxEqual tol s t =
        let toTuple :: (Real a, Floating a) => AlphaColor a -> (a,a,a,a)
            toTuple color =
                let alpha = alphaChannel color
                in uncurryRGB (\r g b -> (r,g,b, realToFrac alpha)) (toSRGB (colourConvert (dissolve (1/alpha) color `over` black)))
        in approxEqual tol (toTuple s) (toTuple t)

testVisual
    :: TestName
    -> Int                       -- ^ Output width
    -> Int                       -- ^ Output height
    -> FilePath                  -- ^ Output file
    -> (Int -> Int -> Render ()) -- ^ Renderer, given width/height
    -> TestTree
testVisual testName w h filePath render = testCase testName (renderAllFormats w h filePath (render w h))
