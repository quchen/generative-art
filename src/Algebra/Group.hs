module Algebra.Group (Group(..)) where

class Monoid a => Group a where
    inverse :: a -> a
