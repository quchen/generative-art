module Algebra.Group where

class Monoid a => Group a where
    inverse :: a -> a
