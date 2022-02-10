module Test.Geometry.LookupTable.Lookup2 (tests) where



import Geometry as G
import Geometry.LookupTable.Lookup2

import Test.TastyAll

import Debug.Trace



tests :: TestTree
tests = testGroup "2D lookup tables"
    [ fromGridTests
    , valueTableTests
    , testGroup "2D function lookup table"
        [ toGridTest
        , lookupOnGridPointsYieldsFunctionValuesTest
        ]
    ]

fromGridTests :: TestTree
fromGridTests = testGroup "Convert grid coordinates to continuous"
    [ testGroup "Square continuous, square discrete"
        [testCase "Start" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 11)
            assertBool "xxx" $ fromGrid grid (IVec2 0 0) ~== Vec2 0 0
        , testCase "Middle" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 11)
            assertBool "xxx" $ fromGrid grid (IVec2 11 11) ~== Vec2 1 1
        , testCase "End" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (10, 10)
            assertBool "xxx" $ fromGrid grid (IVec2 5 5) ~== Vec2 0.5 0.5
        ]
    , testGroup "Square continuous, rectangular discrete"
        [testCase "Start" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 9)
            assertBool "xxx" $ fromGrid grid (IVec2 0 0) ~== Vec2 0 0
        , testCase "Middle" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 9)
            assertBool "xxx" $ fromGrid grid (IVec2 11 9) ~== Vec2 1 1
        , testCase "End" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (10, 8)
            assertBool "xxx" $ fromGrid grid (IVec2 5 4) ~== Vec2 0.5 0.5
        ]
    ]

valueTableTests :: TestTree
valueTableTests = testGroup "Value table creation"
    [ testProperty "Dimension of created table" $
        let gen = do
                iSize <- choose (0, 10)
                jSize <- choose (0, 10)
                let gridRange = (Vec2 0 0, Vec2 1 1)
                pure $ Grid gridRange (iSize, jSize)
        in forAll gen $ \grid@Grid{_numCells = (iSize, jSize)} ->
            let vt = valueTable grid (const ())
            in all (\v -> length v == jSize+1) vt
               &&
               length vt == iSize+1
    ]

pointInGridRange :: Grid -> Gen Vec2
pointInGridRange (Grid (Vec2 xMin yMin, Vec2 xMax yMax) _) = do
    x <- choose (xMin, xMax)
    y <- choose (yMin, yMax)
    pure (Vec2 x y)

toGridTest :: TestTree
toGridTest = testProperty "toGrid" $
    let vecMin = Vec2 0 0
        vecMax = Vec2 100 100
        grid = Grid (vecMin, vecMax) (100, 100)
    in forAll (pointInGridRange grid) $ \v -> toGrid grid v === v

lookupOnGridPointsYieldsFunctionValuesTest :: TestTree
lookupOnGridPointsYieldsFunctionValuesTest = testProperty "Lookup on the grid points yields original function values" $
    let f (Vec2 x y) = sin (x^2 + y^3)
        vecMin = Vec2 0 0
        vecMax = Vec2 100 127 -- Works with 100 100. :-|
        grid = Grid (vecMin, vecMax) (100, 100)
        lut = lookupTable2 grid f
        gen = do
            Vec2 i' j' <- pointInGridRange grid
            pure (fromGrid grid (IVec2 (round i') (round j')))
    in forAll gen $ \v -> lookupBilinear lut v ~== f v
