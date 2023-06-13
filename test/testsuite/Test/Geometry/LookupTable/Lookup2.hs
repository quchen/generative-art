module Test.Geometry.LookupTable.Lookup2 (tests) where



import Control.DeepSeq
import Geometry
import Geometry.LookupTable.Lookup2

import Test.TastyAll



tests :: TestTree
tests = localOption (QuickCheckTests 1000) $ testGroup "2D lookup tables"
    [ valueTableTests
    , testGroup "Grid"
        [ fromGridTests
        , testGroup "toGrid . fromGrid = id"
            [ gridInverseTest_simple
            , gridInverseTest_random
            ]
        ]
    , testGroup "2D function lookup table"
        [ testGroup "lookup lut x == f x"
            [ lookupOnGridMatchesFunction_hardcoded
            , lookupOnGridMatchesFunction_random
            , testPointOutsideDoesNotCrash_lookupNearest
            , testPointOutsideDoesNotCrash_lookupBilinear
            ]
        ]
    ]

fromGridTests :: TestTree
fromGridTests = testGroup "Convert grid coordinates to continuous"
    [ testGroup "Square continuous, square discrete"
        [ testCase "Start" $ do
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
        [ testCase "Start" $ do
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
        in forAll gen $ \grid@Grid{_maxIndex = (iSize, jSize)} ->
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

discreteGridPoint :: Grid -> Gen IVec2
discreteGridPoint (Grid _ (iMax, jMax)) = do
    i <- choose (0, iMax)
    j <- choose (0, jMax)
    pure (IVec2 i j)

-- | A reasonably sized grid for its 'Arbitrary' instance.
newtype ReasonableGrid = ReasonableGrid Grid

instance Arbitrary ReasonableGrid where
    arbitrary = do
        xMin <- choose (-100, 100)
        yMin <- choose (-100, 100)
        let vMin = Vec2 xMin yMin

        xOffset <- choose (1, 100)
        yOffset <- choose (1, 100)
        let vMax = vMin +. Vec2 xOffset yOffset

        iMax <- choose (1, 100)
        jMax <- choose (1, 100)

        pure (ReasonableGrid (Grid (vMin, vMax) (iMax, jMax)))

gridInverseTest_simple :: TestTree
gridInverseTest_simple = testProperty "Simple square grid" $
    let vecMin = Vec2 0 0
        vecMax = Vec2 100 100
        grid = Grid (vecMin, vecMax) (100, 100)
    in forAll (discreteGridPoint grid) $ \iVec ->
        let vec = fromGrid grid iVec
            ciVec = toGrid grid vec
        in roundCIVec2 ciVec === iVec

gridInverseTest_random :: TestTree
gridInverseTest_random = testProperty "Random grid" $
    let gen = do
            ReasonableGrid grid <- arbitrary
            iVec <- discreteGridPoint grid
            pure (grid, iVec)
    in forAll gen $ \(grid, iVec) ->
        let vec = fromGrid grid iVec
            ciVec = toGrid grid vec
        in roundCIVec2 ciVec === iVec

lookupOnGridMatchesFunction_hardcoded :: TestTree
lookupOnGridMatchesFunction_hardcoded = testProperty "With hardcoded grid/function" $
    let f (Vec2 x y) = sin (x^2 + y^3)
        vecMin = Vec2 (-10) 0
        vecMax = Vec2 100 127
        grid = Grid (vecMin, vecMax) (100, 120)
        lut = createLookupTable2 grid f
        gen = do
            v <- pointInGridRange grid
            let gridVec = roundCIVec2 (toGrid grid v)
            pure (fromGrid grid gridVec)
    in forAll gen $ \v -> lookupBilinear lut v ~=== f v

lookupOnGridMatchesFunction_random :: TestTree
lookupOnGridMatchesFunction_random = testProperty "With random grid/function" $
    let gen = do
            ReasonableGrid grid <- arbitrary
            gridVec <- do
                v <- pointInGridRange grid
                pure (roundCIVec2 (toGrid grid v))
            f <- do
                sinCosId <- elements [sin, cos, id]
                e1 <- elements [0..2]
                s1 <- elements [1, -1]
                e2 <- elements [0..2]
                s2 <- elements [1, -1]
                pure (\(Vec2 x y) -> sinCosId (s1*x^e1 + s2*y^e2))
            let lut = createLookupTable2 grid f
            pure (lut, Blind f, fromGrid grid gridVec)
    in forAll gen $ \(lut, Blind f, v) -> lookupBilinear lut v ~=== f v



testPointOutsideDoesNotCrash_lookupNearest :: TestTree
testPointOutsideDoesNotCrash_lookupNearest = pointOutsideDoesNotCrash "Nearest neighbour: OOB point does not throw" lookupNearest

testPointOutsideDoesNotCrash_lookupBilinear :: TestTree
testPointOutsideDoesNotCrash_lookupBilinear = pointOutsideDoesNotCrash "Bilinear: OOB point does not throw" lookupBilinear

pointOutsideDoesNotCrash
    :: (NFData b, Num a)
    => TestName
    -> (LookupTable2 a -> Vec2 -> b)
    -> TestTree
pointOutsideDoesNotCrash testName lookupFunction = testCase testName $ do
    let grid = Grid (Vec2 0 0, Vec2 1 1) (100, 100)
        f _ = 0
        table = createLookupTable2 grid f
        values =
            [ Vec2 10 10
            , Vec2 (-1) 0.5
            , Vec2 0.5 (-1)
            ]
        lookups = map (lookupFunction table) values
    rnf lookups `seq` pure ()
