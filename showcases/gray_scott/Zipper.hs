{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Zipper where

import Control.Comonad



--------------------------------------------------------------------------------
-- * Zipper
--------------------------------------------------------------------------------

-- | The 'Zipper' has periodic boundary conditions, i.e. it wraps around the end.
data Zipper a = Zipper
    { before  :: [a]
    , current :: a
    , after   :: [a] }
    deriving (Functor)

moveBefore, moveAfter :: Zipper a -> Zipper a
moveBefore zipper@Zipper { before = [], after = cs }
    = moveBefore zipper { before = reverse cs, after = [] }
moveBefore zipper@Zipper { before = a : as, current = b, after = cs }
    = zipper { before = as, current = a, after = b : cs }
moveAfter zipper@Zipper { before = as, after = [] }
    = moveAfter zipper { before = [], after = reverse as }
moveAfter  zipper@Zipper { before = as, current = b, after = c : cs }
    = zipper { before = b : as, current = c, after = cs }

-- | Renders the 'current' and the @n - 1@ elements 'after' as list.
zipperToList :: Zipper a -> [a]
zipperToList Zipper{..} = reverse before ++ [current] ++ after

-- | Takes a list and creates a 'Zipper' from it. The 'current' element will be
-- the 'head' of the list, and 'after' that 'tail'. The rest will be filled with
-- @a@s to an infinite 'Zipper'.
zipperFromList :: [a] -> Zipper a
zipperFromList = \case
    []     -> error "Cannot construct empty Zipper"
    b : bs -> Zipper { before = [], current = b, after = bs }

instance Comonad Zipper where
    extract = current
    extend f zipper = fmap f $ Zipper
        { before  = iterate1 moveBefore zipper
        , current = zipper
        , after   = iterate1 moveAfter zipper }
      where
        iterate1 f x = tail (iterate f x)


--------------------------------------------------------------------------------
-- * Plane (two-dimensional 'Zipper')
--------------------------------------------------------------------------------

-- | A plane is a 'Zipper' of 'Zipper's. The outer layer zips through lines
-- (up\/down), the inner layer through columns (left\/right).
-- Like the 'Zipper', the 'Plane' has periodic boundary conditions.
newtype Plane a = Plane { unPlane :: Zipper (Zipper a) }
    deriving (Functor)

moveLeft, moveRight, moveUp, moveDown :: Plane a -> Plane a
moveLeft  = Plane . fmap moveBefore . unPlane
moveRight = Plane . fmap moveAfter  . unPlane
moveUp    = Plane . moveBefore      . unPlane
moveDown  = Plane . moveAfter       . unPlane


-- | Renders @m@ lines and @n@ columns as nested list.
planeToList :: Plane a -> [[a]]
planeToList (Plane Zipper{..}) = fmap zipperToList before ++ [zipperToList current] ++ fmap zipperToList after

-- | Create a 'Plane' from a list of lists, filling the rest with @a@s in all
-- directions.
planeFromList :: [[a]] -> Plane a
planeFromList = \case
    []       -> error " Cannot construct empty Plane"
    as : ass -> Plane Zipper { before = [], current = zipperFromList as, after = zipperFromList <$> ass }

instance Comonad Plane where
    extract = current . current . unPlane
    extend f plane = fmap f $ Plane $ Zipper
        { before  = fmap foo (iterate1 moveUp plane)
        , current = foo plane
        , after   = fmap foo (iterate1 moveDown plane) }
      where
        foo p = Zipper
            { before = iterate1 moveLeft p
            , current = p
            , after = iterate1 moveRight p }
        iterate1 f x = tail (iterate f x)
