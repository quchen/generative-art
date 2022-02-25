module Algebra.Group (Group(..)) where

-- A group is something you can invert. Transformations or symmetries are typical
-- examples.
class Monoid a => Group a where
    inverse :: a -> a
