module Test.TastyAll (
    -- * HUnit extensions
      assertThrowsError
    , Expected(..)
    , ExpectedWithin(..)
    , Actual(..)
    , assertEqual
    , assertApproxEqual
    , (~==)
    , (~===)
    , approxEqual
    , Tolerance(..)

    -- * Rendering pictures
    , renderAllFormats
    , testVisual

    -- * Reexports
    , module Control.Applicative
    , module Control.Monad
    , module Test.Arbitrary
    , module Test.Tasty
    , module Test.Tasty.HUnit
    , module Test.Tasty.QuickCheck
) where



import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Colour.RGBSpace     as Colour
import Data.Colour.SRGB         as Colour
import Data.List
import Graphics.Rendering.Cairo as C hiding (x, y)

import Draw
import Draw.NormalizeSvg
import Geometry

import           Geometry.SvgParser.SimpleShapes
import           Test.Arbitrary
import           Test.Tasty
import           Test.Tasty.HUnit                hiding (assertEqual)
import qualified Test.Tasty.HUnit                as HUnit
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

renderAllFormats :: Int -> Int -> FilePath -> Render () -> IO ()
renderAllFormats w h filename drawing = do
    render (filename ++ ".png") w h $ do
        cairoScope $ do
            setColor white
            paint
        drawing

    let svgFilename = filename ++ ".svg"
    render svgFilename w h drawing
    normalizeSvgFile svgFilename

newtype Tolerance = Tolerance Double

class EqApprox a where
    approxEqual :: Tolerance -> a -> a -> Bool

-- | Useful shorthand for 'approxEqual' with a default tolerance.
(~==) :: EqApprox a => a -> a -> Bool
(~==) = approxEqual (Tolerance 1e-10)
infix 4 ~==

-- | Approximate version of Quickcheck’s counterexample-printing '==='.
(~===) :: (Show a, EqApprox a) => a -> a -> Property
x ~=== y = counterexample
    (show x ++ "\nis not approximately\n" ++ show y)
    (approxEqual (Tolerance 1e-10) x y)
infix 4 ~===

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

instance EqApprox Line where
    approxEqual tol (Line a b) (Line c d) = approxEqual tol (a,b) (c,d)

instance EqApprox BoundingBox where
    approxEqual tol (BoundingBox a b) (BoundingBox c d) = approxEqual tol (a,b) (c,d)

instance EqApprox Circle where
    approxEqual tol (Circle c1 r1) (Circle c2 r2) = approxEqual tol (c1,r1) (c2,r2)

instance EqApprox Ellipse where
    approxEqual tol (Ellipse e1) (Ellipse e2) = approxEqual tol e1 e2

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
        (Transformation (Mat2 a1 b1 d1 e1) (Vec2 c1 f1))
        (Transformation (Mat2 a2 b2 d2 e2) (Vec2 c2 f2))
      = all (\(x,y) -> approxEqual tol x y)
            [ (a1, a2)
            , (b1, b2)
            , (c1, c2)
            , (d1, d2)
            , (e1, e2)
            , (f1, f2) ]

instance EqApprox SimpleShape  where
    approxEqual tol (SvgLine a) (SvgLine b) = approxEqual tol a b
    approxEqual tol (SvgCircle a) (SvgCircle b) = approxEqual tol a b
    approxEqual tol (SvgEllipse a) (SvgEllipse b) = approxEqual tol a b
    approxEqual _ _ _ = False

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
    :: Num a
    => TestName
    -> Int                   -- ^ Output width
    -> Int                   -- ^ Output height
    -> FilePath              -- ^ Output file
    -> ((a, a) -> Render ()) -- ^ Renderer, given width/height
    -> TestTree
testVisual testName w h filePath actions = testCase testName (renderAllFormats w h filePath (actions (fromIntegral w,fromIntegral h)))
