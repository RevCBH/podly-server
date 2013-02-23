{-# LANGUAGE PatternGuards #-}
module Util.TH where

import Prelude
import Data.List (stripPrefix)
import Data.Char (toLower)
import Data.Maybe (isJust)

infixl 0 |>
(|>) :: t -> (t -> u) -> u
a |> b = b a

removePrefix :: String -> String -> String
removePrefix p str =
  str |> (drop . length) p
      |> downcaseFirst
 where
  downcaseFirst (x:xs) = (Data.Char.toLower x):xs
  downcaseFirst [] = []

removeFirstPrefix :: [String] -> String -> String
removeFirstPrefix (p:_) str | isJust (stripPrefix p str) = removePrefix p str
removeFirstPrefix (_:rest) str = removeFirstPrefix rest str
removeFirstPrefix _ str = error $ "No matching prefix for: " ++ str