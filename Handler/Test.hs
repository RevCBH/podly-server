{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Test where

import Import
import Data.Aeson (Result(..), ToJSON(..), FromJSON(..), fromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import Data.Time(addUTCTime, getCurrentTime, Day, UTCTime(..), secondsToDiffTime)
import Data.Text (pack)
import Text.Coffee (coffeeFile)
import qualified Data.HashMap.Strict as Map

import Fixtures
import Document
import Handler.Util

import Yesod.Form.Jquery

getUsersR :: Handler RepHtml
getUsersR = do
    users <- runDB $ selectList [] [Asc UserIdent]
    let widget = do
        [whamlet|
            <ul>
                $forall Entity tid x <- users
                    <li>#{userIdent x}
        |]
    defaultLayout widget

postEchoJsonR :: Handler RepJson
postEchoJsonR = do
    res <- parseJsonBody
    case res of
        Success (val :: Value) -> jsonToRepJson val
        Error msg -> jsonToRepJson $ object ["error" .= msg]

getNukeR :: Handler RepJson
getNukeR = do
    --runDB $ deleteWhere ([] :: [Filter Event])
    runDB $ deleteWhere ([] :: [Filter NodeInstance])
    runDB $ deleteWhere ([] :: [Filter Node])
    runDB $ deleteWhere ([] :: [Filter NodeType])
    runDB $ deleteWhere ([] :: [Filter Episode])
    runDB $ deleteWhere ([] :: [Filter Podcast])

    loadNodeTypes "node_types"
    runDB $ loadEpisodes "episodes"

    jsonToRepJson ("OK" :: String)

countEpisodesFor (Entity _ podcast) = do
    runDB $ count $ [EpisodePodcast ==. podcastName podcast]

getPodcastIndexR :: Handler RepHtmlJson
getPodcastIndexR = do
    entities <- runDB $ selectList [] [Asc PodcastName]
    let podcasts = (flip map) entities (\(Entity _ x) -> x)
    let widget = do
        setTitle $ toHtml ("Podcasts" :: String)
        $(widgetFile "podcasts/index")
    json <- (flip mapM) entities (\entity -> do
                    count <- countEpisodesFor entity
                    let (Object o) = toJSON entity
                    return $ Map.insert "episodeCount" (toJSON count) o)
    defaultLayoutJson widget json

getPodcastR :: Text -> Handler RepJson
getPodcastR name = do
    podcast <- runDB $ getBy404 $ UniquePodcastName name
    jsonToRepJson podcast

getPodcastEpisodeIndexR :: Text -> Handler RepHtmlJson
getPodcastEpisodeIndexR name = do
    episodes <- runDB $ selectList [EpisodePodcast ==. name] [Asc EpisodeNumber]
    let widget = do
        setTitle $ toHtml $ name <> " - Episodes"
        $(widgetFile "podcasts/show")
    let json = episodes
    defaultLayoutJson widget json

-- /episodes EpisodesR GET POST

getEpisodesR :: Handler RepHtmlJson
getEpisodesR = do
    episodes <- runDB $ selectList [] [Desc EpisodeAirDate]
    let widget = do
        setTitle $ toHtml ("Episodes" :: String)
        [whamlet|There are #{length episodes} episodes.|]
    let json = episodes
    defaultLayoutJson widget json


getEpisodesJsonR :: Handler RepJson
getEpisodesJsonR = do
    episodes <- runDB $ selectList [] [Desc EpisodeAirDate]
    --docs <- runDB $ mapM documentFromEpisode episodes
    jsonToRepJson episodes

{-
postNodeInstanceR :: Text -> Int -> Handler RepHtml
postNodeInstanceR podcast number = do
    (Entity episodeId _) <- runDB $ getBy404 $ UniqueEpisodeNumber podcast number
    nodes <- runDB $ selectList [] [Asc NodeTitle]
    (source, nodeInstance) <- fromJsonOrFormPost $ newNodeInstanceForm episodeId nodes
    runDB $ insert nodeInstance
    ----case source of
    ----    PostJson -> jsonToRepJson entity
    --    PostForm -> redirect $ PodcastEpisodeR (episodePodcast episode) (episodeNumber episode)
    redirect $ PodcastEpisodeR podcast number
-}