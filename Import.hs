module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)
import           Data.Aeson           as Import (ToJSON, FromJSON, encode, decode)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import           GHC.Generics          as Import (Generic)

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

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