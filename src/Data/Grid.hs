{-# LANGUAGE DeriveFunctor #-}
module Data.Grid (
      Grid(..)
    , left
    , right
    , up
    , down
    , fromList
    , toList
    , slice
    , mapAt
    , setAt
    , mapCurrent
    , size

    , Comonad(..)
) where



import Control.Comonad
import qualified Data.Vector as V

data Grid a = Grid !Int !Int !Int !Int (V.Vector (V.Vector a)) deriving (Eq, Ord, Show, Functor)



fromList :: [[a]] -> Grid a
fromList xss = Grid 0 d r 0 vss
  where
    vss = V.fromList (fmap V.fromList xss)
    d = V.length vss - 1
    r = minimum (V.toList (V.length <$> vss)) - 1

toList :: Grid a -> [[a]]
toList (Grid _ _ _ _ vs) = V.toList (V.toList <$> vs)

left, right, up, down :: Grid a -> Maybe (Grid a)
left  (Grid l d r u zs) | l > 0 = Just $ Grid (l-1) d (r+1) u zs
                        | otherwise = Nothing
right (Grid l d r u zs) | r > 0 = Just $ Grid (l+1) d (r-1) u zs
                        | otherwise = Nothing
up    (Grid l d r u zs) | u > 0 = Just $ Grid l (d+1) r (u-1) zs
                        | otherwise = Nothing
down  (Grid l d r u zs) | d > 0 = Just $ Grid l (d-1) r (u+1) zs
                        | otherwise = Nothing

size :: Grid a -> (Int, Int)
size (Grid l d r u _) = (l + r + 1, d + u + 1)

slice :: Int -> Int -> Int -> Int -> Grid a -> Grid a
slice l' d' r' u' (Grid l d r u xs)
    | l' > l || d' > d || r' > r || u' > u = error "Index out of bounds"
    | otherwise = Grid l' d' r' u' (V.slice x w <$> V.slice y h xs)
  where
    x = l - l'
    y = u - u'
    w = l' + r' + 1
    h = u' + d' + 1

instance Foldable Grid where
    foldr plus zero (Grid _ _ _ _ zz) = foldr plus zero (V.toList zz >>= V.toList)

instance Comonad Grid where
    extract (Grid l _ _ u xs) = xs V.! u V.! l

    duplicate grid@(Grid l d r u xs) = Grid l d r u xss
      where
        (w, h) = size grid
        xss = V.fromList
            [ V.fromList
                [ Grid l' d' r' u' xs
                | l' <- [0..w-1]
                , let r' = w-1-l'
                ]
            | u' <- [0..h-1]
            , let d' = h-1-u'
            ]

mapAt :: (Int, Int) -> (a -> a) -> Grid a -> Grid a
mapAt (x, y) f (Grid l d r u xs) = Grid l d r u (xs V.// [(y, (xs V.! y) V.// [(x, f (xs V.! y V.! x))])])

setAt :: (Int, Int) -> a -> Grid a -> Grid a
setAt (x, y) a = mapAt (x, y) (const a)

mapCurrent :: (a -> a) -> Grid a -> Grid a
mapCurrent f g@(Grid l _ _ u _) = mapAt (l, u) f g
