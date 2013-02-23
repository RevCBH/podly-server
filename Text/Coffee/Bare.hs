{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for CoffeeScript, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript with
-- the system tool @coffee@.
--
-- To use this module, @coffee@ must be installed on your system.
--
-- @#{...}@ is the Shakespearean standard for variable interpolation, but
-- CoffeeScript already uses that sequence for string interpolation. Therefore,
-- Shakespearean interpolation is introduced with @%{...}@.
--
-- Further reading:
--
-- 1. Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
-- 2. CoffeeScript: <http://coffeescript.org/>
module Text.Coffee.Bare
    ( coffeeB
    , coffeeBareFile
    , coffeeBareFileReload

#ifdef TEST_EXPORT
    , coffeeBareSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax

import Data.Maybe (Maybe(..))
import System.FilePath(FilePath)

import Text.Shakespeare
import Text.Julius

import GHC.Base

-- Begin WTF section
--infixr 0  $

--{-# INLINE ($) #-}
--($)                     :: (a -> b) -> a -> b
--f $ x                   =  f x
-- End WTF section

coffeeBareSettings :: Q ShakespeareSettings
coffeeBareSettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '%'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "coffee" ["-sbp"]
    , preEscapeBegin = "`"
    , preEscapeEnd = "`"
    , preEscapeIgnoreBalanced = "'\"`"
    , preEscapeIgnoreLine = "#"
    }
  }

  -- | Read inline, quasiquoted CoffeeScript.
coffeeB :: QuasiQuoter
coffeeB = QuasiQuoter { quoteExp = \s -> do
    rs <- coffeeBareSettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a CoffeeScript template file. This function reads the file once, at
-- compile time.
coffeeBareFile :: FilePath -> Q Exp
coffeeBareFile fp = do
    rs <- coffeeBareSettings
    shakespeareFile rs fp

-- | Read in a CoffeeScript template file. This impure function uses
-- unsafePerformIO to re-read the file on every call, allowing for rapid
-- iteration.
coffeeBareFileReload :: FilePath -> Q Exp
coffeeBareFileReload fp = do
    rs <- coffeeBareSettings
    shakespeareFileReload rs fp
