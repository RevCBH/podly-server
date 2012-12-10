module Util.TH where

import Prelude
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