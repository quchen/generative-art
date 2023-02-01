{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.Grid (tests) where



import Data.Grid
import qualified Data.Map as M

import Test.TastyAll
import Data.Maybe



tests :: TestTree
tests = localOption (QuickCheckMaxSize 15) $ testGroup "Data.Grid"
    [ testGroup "Comonad laws"
        [ testProperty "extract . duplicate ≡ id" $ \(g :: RectilinearGrid Int) ->
            extract (duplicate g) === g
        , testProperty "fmap extract . duplicate ≡ id" $ \(g :: RectilinearGrid Int) ->
            fmap extract (duplicate g) === g
        , testProperty "duplicate . duplicate ≡ fmap duplicate . duplicate" $ \(g :: RectilinearGrid Int) ->
            duplicate (duplicate g) === fmap duplicate (duplicate g)
        ]
    , testProperty "size" $ \(Positive w) (Positive h) -> forAll (gridOfSize w h arbitrary) $ \(grid :: RectilinearGrid Int) ->
        size grid === (w, h)
    , testProperty "left is inverse to right" $ \(g :: RectilinearGrid Int) ->
        isJust (left g) ==> (left >=> right) g === Just g
    , testProperty "right is inverse to left" $ \(g :: RectilinearGrid Int) ->
        isJust (right g) ==> (right >=> left) g === Just g
    , testProperty "up is inverse to down" $ \(g :: RectilinearGrid Int) ->
        isJust (up g) ==> (up >=> down) g === Just g
    , testProperty "down is inverse to up" $ \(g :: RectilinearGrid Int) ->
        isJust (down g) ==> (down >=> up) g === Just g
    ]

instance Arbitrary a => Arbitrary (Grid (Int, Int) a) where
    arbitrary = do
        Positive w <- arbitrary
        Positive h <- arbitrary
        gridOfSize w h arbitrary

listOfLength :: Int -> Gen a -> Gen [a]
listOfLength n gen = replicateM n gen

gridOfSize :: Int -> Int -> Gen a -> Gen (RectilinearGrid a)
gridOfSize w h gen = do
    items <- listOfLength (w*h) gen
    let xs = M.fromList (zip [(x, y) | x <- [0..w-1], y <- [0..h-1]] items)
    x <- chooseInt (0, w-1)
    y <- chooseInt (0, h-1)
    pure (Grid (x, y) xs)
