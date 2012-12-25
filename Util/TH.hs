module Util.TH where

import Prelude
import Data.List(stripPrefix)
import Data.Char(toLower)

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
removeFirstPrefix (p:rest) str | Just restOfString <- stripPrefix p str = removePrefix p str
removeFirstPrefix (p:rest) str = removeFirstPrefix rest str
removeFirstPrefix _ str = error $ "No matching prefix for: " ++ str