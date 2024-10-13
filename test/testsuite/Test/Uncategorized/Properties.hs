{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Uncategorized.Properties (tests) where



import Text.Printf

import Geometry as G

import Test.TastyAll



tests :: TestTree
tests = testGroup "Properties"
    [ angleBetweenTest
    , areaTest
    , intersectionLLTest
    , lengthOfAngledLineTest
    , crossTest
    , dotProductTest
    , polygonInstancesTest
    , transformationTest
    , distanceFromLineTest
    ]

angleBetweenTest :: TestTree
angleBetweenTest = testProperty "Angle between two lines"
    (\angle1 angle2 ->
        let line1 = angledLine (Vec2 0 0) angle1 1
            line2 = angledLine (Vec2 0 0) angle2 1
            actualAngle = normalizeAngle zero (angleBetween line1 line2)
            expectedAngle = normalizeAngle zero (angle2 -. angle1)
        in counterexample
            (unlines ["Counterexample:"
                     , "  Line 1: " ++ show line1 ++ " (angle: " ++ show angle1 ++ ")"
                     , "  Line 2: " ++ show line2 ++ " (angle: " ++ show angle2 ++ ")"
                     , "  Δ actual:   " ++ show actualAngle
                     , "  Δ expected: " ++ show expectedAngle
                     ])
            (expectedAngle ~== actualAngle) )

areaTest :: TestTree
areaTest = testGroup "Area"
    [ parallelogram
    , square ]
  where
    coord = choose (-100, 100 :: Double)
    vec2 = liftA2 Vec2 coord coord

    square = testProperty "Square" (forAll
        ((,,,,) <$> vec2 <*> vec2 <*> vec2 <*> vec2 <*> arbitrary)
        (\(Vec2 x1 y1, Vec2 x2 y2, center, moveVec, angle) ->
            let poly = (transform (translate moveVec <> rotateAround center angle) . Polygon)
                           [Vec2 x1 y1, Vec2 x1 y2, Vec2 x2 y2, Vec2 x2 y1]
                actual = polygonArea poly
                expected = abs ((x2-x1) * (y2-y1))
            in actual ~== expected ))

    parallelogram = testProperty "Parallelogram" (forAll
        ((,) <$> vec2 <*> vec2)
        (\(v1, v2) ->
            let actual = polygonArea (Polygon [zero, v1, v1 +. v2, v2])
                rawAngleBetween = angleBetween (Line zero v1) (Line zero v2)
                v1norm = norm v1
                v2norm = norm v2
                expected = abs (v1norm * v2norm * sin (getRad rawAngleBetween))
            in actual ~== expected ))

intersectionLLTest :: TestTree
intersectionLLTest = testProperty "Line-line intersection" (forAll
    ((,,,,) <$> vec2 <*> vec2 <*> dist <*> vec2 <*> dist)
    (\(start, end1, dist1, end2, dist2) ->
        let line1 = Line start end1
            line1' = transform (translate (polar (angleOfLine line1) dist1)) line1
            line2 = Line start end2
            line2' = transform (translate (polar (angleOfLine line2) dist2)) line2
            Just expectedIntersection = intersectionPoint (intersectionLL line1' line2')
        in approxEqual (Tolerance 1e-8) start expectedIntersection ))
  where
    coord = choose (-100, 100 :: Double)
    vec2 = liftA2 Vec2 coord coord
    dist = choose (-100, 100)

lengthOfAngledLineTest :: TestTree
lengthOfAngledLineTest = testProperty "Length of angled line" (forAll
    ((,,) <$> vec2 <*> arbitrary <*> arbitrary)
    (\(start, angle, Positive len) ->
        let actual = lineLength (angledLine start angle len)
            expected = len
        in actual ~== expected ))
  where
    coord = choose (-100, 100 :: Double)
    vec2 = liftA2 Vec2 coord coord

crossTest :: TestTree
crossTest = testGroup "Determinant/cross product"
    [ testCase "Positive" (assertBool "Positive" (cross (Vec2 1 0) (Vec2 0   1)  > 0))
    , testCase "Negative" (assertBool "Negative" (cross (Vec2 1 0) (Vec2 0 (-1)) < 0))
    , testProperty "Antisymmetric" $ \v1 v2 ->
        let Vec2 x1 y1 = v1
            Vec2 x2 y2 = v2
            detA  = cross v1 v2
            detA' = cross v2 v1
        in counterexample (printf " det (%f,%f) (%f,%f) =  %f,\n-det (%f,%f) (%f,%f) = %f"
                                        x1 y1   x2 y2    detA        x2 y2   x1 y1    detA')
                          (detA ~== -detA')
    , testProperty "Invariant: transposition" $ \x1 y1 x2 y2 ->
        let detA  = cross (Vec2 x1 y1) (Vec2 x2 y2)
            detAT = cross (Vec2 x1 x2) (Vec2 y1 y2)
        in counterexample (printf "det (%f,%f) (%f,%f) = %f,\ndet (%f,%f) (%f,%f) = %f"
                                        x1 y1   x2 y2    detA      x1 x2   y1 y2    detAT)
                          (detA ~== detAT)
    ]

dotProductTest :: TestTree
dotProductTest = testGroup "Dot product"
    [ testProperty "Commutativity" (\v1 v2 -> dotProduct v1 v2 ~== dotProduct v2 v1)
    , testProperty "Zero is eliminator" (\v1 -> dotProduct v1 (Vec2 0 0) ~== 0)
    , testGroup "Measure of colinearity"
        [ testProperty "…with unit vector" $ \vGen ->
            let v1 = Vec2 1 0
                Vec2 x _ = vGen
            in signum (dotProduct v1 vGen) == signum x
        , testProperty "…with arbitrary vectors" $
            let nonZeroVec = do
                    (NonZero a, b, xy) <- arbitrary
                    pure (if xy then Vec2 a b else Vec2 b a)
            in forAll ((,) <$> nonZeroVec <*> nonZeroVec) $ \(v1, v2) ->
                let v0 = Vec2 0 0
                    angle = angleBetween (Line v0 v1) (Line v0 v2)
                    a = getRad angle
                    prod = dotProduct v1 v2
                in -- Exclude almost-90° angles to avoid blinker tests
                    abs (cos a) >= 1e-10 ==> counterexample
                    (printf "%s • %s = %f\nangle; cos = %s; %.2f"
                            (show v1) (show v2) prod (show angle) (cos a))
                    (signum (cos a) == signum prod)
        ]
    ]

polygonInstancesTest :: TestTree
polygonInstancesTest = testGroup "Eq Polygon"
    [ equalPolygonsTest
    ]
  where
    equalPolygonsTest = testProperty "Polygons with rotated corner order" (forAll gen test)
      where
        gen = do
            size <- getSize
            len <- fmap (+3) (choose (0, size))
            polygon <- fmap Polygon (Test.TastyAll.vectorOf len arbitrary)
            rot <- choose (0, len-1)
            pure (rot, polygon)
        test (rot, polygon)
          = let Polygon corners = polygon
                polygon' = Polygon (rotateList rot corners)
            in polygon === polygon'

-- | Rotate a list n times.
rotateList :: Int -> [a] -> [a]
rotateList n xs = let (a,b) = splitAt n xs in b ++ a

transformationTest :: TestTree
transformationTest = testGroup "Affine transformations"
    [ testGroup "Algebraic properties"
        [ testProperty "Multiple rotations add angles" $ \a1 a2 ->
            approxEqual (Tolerance 1e-5)
                (rotate a1 <> rotate a2)
                (rotate (a1 +. a2))
        ]
    , testGroup "Invertibility"
        [ invertibilityTest "Identity"
            (pure [mempty])
        , invertibilityTest "Translation"
            (do x <- choose (-1000,1000); y <- choose (-1000,1000); pure [translate (Vec2 x y)])
        , invertibilityTest "Scaling"
            (do let sign = elements [1,-1]
                    factor = choose (0.1,10)
                xSign <- sign
                ySign <- sign
                xScale <- factor
                yScale <- factor
                pure [G.scale' (xSign*xScale) (ySign*yScale)])
        , invertibilityTest "Rotation"
            (do angle <- arbitrary; pure [rotate angle])
        , invertibilityTest "Combination of transformations" $ do
            size <- getSize
            n <- choose (2, min size 10)
            Test.TastyAll.vectorOf n (frequency
                [ (1, pure mempty)
                , (3, rotate <$> arbitrary)
                , (3, translate <$> liftA2 Vec2 (choose (-100,100)) (choose (-100,100)))
                , (3, G.scale' <$> liftA2 (*) (elements [-1,1]) (choose (0.2, 5))
                               <*> liftA2 (*) (elements [-1,1]) (choose (0.2, 5))) ])
        ]
    , testGroup "Affine decomposition"
        [ testProperty "Decomposition of arbitrary non-singular transformation" $ \trafo ->
            let (dx, (sx,sy), q, phi) = decomposeTransformation trafo
                scaling = G.scale' sx sy
                shearing = G.shear 0 q
                rotation = rotate phi
                translation = G.translate dx
            in trafo ~=== translation <> scaling <> shearing <> rotation
        , testProperty "Decomposing rotateAround" $ \center angle ->
            let trafo = rotateAround center angle
                recomposed =
                    let (dx, _, _, phi) = decomposeTransformation trafo
                        rotation = rotate phi
                        translation = G.translate dx
                    in translation <> rotation
            in trafo ~=== recomposed
        ]
    ]
  where
    invertibilityTest name trafosGen
      = let gen = do
                trafos <- trafosGen
                let transformation = mconcat trafos
                vec <- arbitrary
                pure (vec :: Vec2, transformation)
            test (vec, transformation) = approxEqual (Tolerance 1e-5)
                ((transform (inverse transformation) . transform transformation) vec)
                vec
        in testProperty name (forAll gen test)

distanceFromLineTest :: TestTree
distanceFromLineTest = testProperty "Distance from point to line" $
    forAll gen $ \(d, (point, line)) -> approxEqual (Tolerance 1e-5) (distanceFromLine point line) d
  where
    -- Generate a simple geometry where the distance is known
    gen = do
        let coord = choose (-100, 100 :: Double)
        x1 <- coord
        x2 <- coord
        let line = Line (Vec2 x1 0) (Vec2 x2 0)
        distance <- coord
        let point = Vec2 0 distance

        angle1 <- arbitrary
        angle2 <- arbitrary
        offset1 <- arbitrary
        offset2 <- arbitrary
        let trafo = rotate angle2 <> translate offset2 <> rotate angle1 <> translate offset1

        pure (abs distance, transform trafo (point, line))
