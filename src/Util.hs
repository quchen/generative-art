module Util (
      bugError
    , todo
) where



bugError :: String -> String -> a
bugError location msg = errorWithoutStackTrace (location ++ ": " ++ msg ++ "\nThis should never happen! Please report it as a bug.")

{-# WARNING todo "TODO in code" #-}
todo :: String -> String -> a
todo location msg = errorWithoutStackTrace (location ++ ": TODO: " ++ msg ++ "\nThis should not be present in live code, but it is! Commence panicking!")
