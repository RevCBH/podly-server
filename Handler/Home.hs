{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        setTitle "Player control test"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"
        addScriptRemote "/static/js/froogaloop.js"
        $(widgetFile "homepage")
