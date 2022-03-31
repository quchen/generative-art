module Util (
      nubOrd
    , bugError
    , todo
) where



import qualified Data.Set as S



-- | 'nub', but based on 'S.Set' for performance reasons.
nubOrd :: Ord a => [a] -> [a]
nubOrd = go S.empty
  where
    go _ [] = []
    go seen (x:xs)
        | S.member x seen = go seen xs
        | otherwise       = x : go (S.insert x seen) xs

bugError :: String -> String -> a
bugError location msg = errorWithoutStackTrace (location ++ ": " ++ msg ++ "\nThis should never happen! Please report it as a bug.")

{-# WARNING todo "TODO in code" #-}
todo :: String -> String -> a
todo location msg = errorWithoutStackTrace (location ++ ": TODO: " ++ msg ++ "\nThis should not be present in live code, but it is! Commence panicking!")
