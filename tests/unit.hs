{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.QuickCheck

--import Import
--import Yesod.Default.Config
--import Yesod.Test
--import Application (makeFoundation)

--import HomeTest

main :: IO ()
main = undefined
    --conf <- loadConfig $ (configSettings Testing) { csParseExtra = parseExtra }
    --foundation <- makeFoundation conf
    --app <- toWaiAppPlain foundation
    --runTests app (connPool foundation) homeSpecs
