{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.Grid (tests) where



import Data.Grid

import Test.TastyAll



tests :: TestTree
tests = localOption (QuickCheckMaxSize 15) $ testGroup "Data.Grid"
    [ testGroup "Comonad laws"
        [ testProperty "extract . duplicate ≡ id" $ \(g :: Grid Int) ->
            extract (duplicate g) === g
        , testProperty "fmap extract . duplicate ≡ id" $ \(g :: Grid Int) ->
            fmap extract (duplicate g) === g
        , testProperty "duplicate . duplicate ≡ fmap duplicate . duplicate" $ \(g :: Grid Int) ->
            duplicate (duplicate g) === fmap duplicate (duplicate g)
        ]
    , testProperty "size" $ \(Positive w) (Positive h) -> forAll (listOfLength h (listOfLength w arbitrary)) $ \(xss :: [[Int]]) ->
        size (fromList xss) === (w, h)
    ]

instance Arbitrary a => Arbitrary (Grid a) where
    arbitrary = do
        Positive w <- arbitrary
        Positive h <- arbitrary
        Grid _ d r _ xs <- fromList <$> listOfLength h (listOfLength w arbitrary)
        x <- chooseInt (0, r)
        y <- chooseInt (0, d)
        pure (Grid x (d-y) (r-x) y xs)

listOfLength :: Int -> Gen a -> Gen [a]
listOfLength n gen = replicateM n gen
