module Test.Geometry.LookupTable.Lookup2 (tests) where



import Geometry
import Geometry.LookupTable.Lookup2
import Control.DeepSeq

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
            let gridSpec = GridSpec (Vec2 0 0, Vec2 1 1) (11, 11)
            assertBool "xxx" $ fromGrid gridSpec (IVec2 0 0) ~== Vec2 0 0
        , testCase "Middle" $ do
            let gridSpec = GridSpec (Vec2 0 0, Vec2 1 1) (11, 11)
            assertBool "xxx" $ fromGrid gridSpec (IVec2 11 11) ~== Vec2 1 1
        , testCase "End" $ do
            let gridSpec = GridSpec (Vec2 0 0, Vec2 1 1) (10, 10)
            assertBool "xxx" $ fromGrid gridSpec (IVec2 5 5) ~== Vec2 0.5 0.5
        ]
    , testGroup "Square continuous, rectangular discrete"
        [ testCase "Start" $ do
            let gridSpec = GridSpec (Vec2 0 0, Vec2 1 1) (11, 9)
            assertBool "xxx" $ fromGrid gridSpec (IVec2 0 0) ~== Vec2 0 0
        , testCase "Middle" $ do
            let gridSpec = GridSpec (Vec2 0 0, Vec2 1 1) (11, 9)
            assertBool "xxx" $ fromGrid gridSpec (IVec2 11 9) ~== Vec2 1 1
        , testCase "End" $ do
            let gridSpec = GridSpec (Vec2 0 0, Vec2 1 1) (10, 8)
            assertBool "xxx" $ fromGrid gridSpec (IVec2 5 4) ~== Vec2 0.5 0.5
        ]
    ]

valueTableTests :: TestTree
valueTableTests = testGroup "Value table creation"
    [ testProperty "Dimension of created table" $
        let gen = do
                iSize <- choose (0, 10)
                jSize <- choose (0, 10)
                let gridRange = (Vec2 0 0, Vec2 1 1)
                pure $ GridSpec gridRange (iSize, jSize)
        in forAll gen $ \gridSpec@GridSpec{_maxIndex = (iSize, jSize)} ->
            let vt = valueTable gridSpec (const ())
            in all (\v -> length v == jSize+1) vt
               &&
               length vt == iSize+1
    ]

pointInGridRange :: GridSpec -> Gen Vec2
pointInGridRange (GridSpec (Vec2 xMin yMin, Vec2 xMax yMax) _) = do
    x <- choose (xMin, xMax)
    y <- choose (yMin, yMax)
    pure (Vec2 x y)

discreteGridPoint :: GridSpec -> Gen IVec2
discreteGridPoint (GridSpec _ (iMax, jMax)) = do
    i <- choose (0, iMax)
    j <- choose (0, jMax)
    pure (IVec2 i j)

-- | A reasonably sized grid for its 'Arbitrary' instance.
newtype ReasonableGrid = ReasonableGrid GridSpec

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

        pure (ReasonableGrid (GridSpec (vMin, vMax) (iMax, jMax)))

gridInverseTest_simple :: TestTree
gridInverseTest_simple = testProperty "Simple square grid" $
    let vecMin = Vec2 0 0
        vecMax = Vec2 100 100
        gridSpec = GridSpec (vecMin, vecMax) (100, 100)
    in forAll (discreteGridPoint gridSpec) $ \iVec ->
        let vec = fromGrid gridSpec iVec
            ciVec = toGrid gridSpec vec
        in roundCIVec2 ciVec === iVec

gridInverseTest_random :: TestTree
gridInverseTest_random = testProperty "Random grid" $
    let gen = do
            ReasonableGrid gridSpec <- arbitrary
            iVec <- discreteGridPoint gridSpec
            pure (gridSpec, iVec)
    in forAll gen $ \(gridSpec, iVec) ->
        let vec = fromGrid gridSpec iVec
            ciVec = toGrid gridSpec vec
        in roundCIVec2 ciVec === iVec

lookupOnGridMatchesFunction_hardcoded :: TestTree
lookupOnGridMatchesFunction_hardcoded = testProperty "With hardcoded grid/function" $
    let f (Vec2 x y) = sin (x^2 + y^3)
        vecMin = Vec2 (-10) 0
        vecMax = Vec2 100 127
        gridSpec = GridSpec (vecMin, vecMax) (100, 120)
        lut = createLookupTable2 gridSpec f
        gen = do
            v <- pointInGridRange gridSpec
            let gridVec = roundCIVec2 (toGrid gridSpec v)
            pure (fromGrid gridSpec gridVec)
    in forAll gen $ \v -> lookupBilinear lut v ~=== f v

lookupOnGridMatchesFunction_random :: TestTree
lookupOnGridMatchesFunction_random = testProperty "With random grid/function" $
    let gen = do
            ReasonableGrid gridSpec <- arbitrary
            gridVec <- do
                v <- pointInGridRange gridSpec
                pure (roundCIVec2 (toGrid gridSpec v))
            f <- do
                sinCosId <- elements [sin, cos, id]
                e1 <- elements [0..2]
                s1 <- elements [1, -1]
                e2 <- elements [0..2]
                s2 <- elements [1, -1]
                pure (\(Vec2 x y) -> sinCosId (s1*x^e1 + s2*y^e2))
            let lut = createLookupTable2 gridSpec f
            pure (lut, Blind f, fromGrid gridSpec gridVec)
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
    let gridSpec = GridSpec (Vec2 0 0, Vec2 1 1) (100, 100)
        f _ = 0
        table = createLookupTable2 gridSpec f
        values =
            [ Vec2 10 10
            , Vec2 (-1) 0.5
            , Vec2 0.5 (-1)
            ]
        lookups = map (lookupFunction table) values
    rnf lookups `seq` pure ()
