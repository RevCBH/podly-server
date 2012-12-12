{-# LANGUAGE OverloadedStrings #-}
module Handler.Episode where

import Import
import Handler.Util

import Data.Aeson (Result(..), FromJSON(..), fromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import Data.Time(Day, UTCTime(..), secondsToDiffTime)
import Data.Text (pack)

import Yesod.Form.Jquery

import Document

newNodeInstanceForm :: EpisodeId -> [Entity Node] -> Form NodeInstance
newNodeInstanceForm episodeId nodes = renderDivs $ NodeInstance
    <$> areq (selectFieldList nodes') "Node" Nothing
    <*> aopt hiddenField "" Nothing
    <*> areq hiddenField "" (Just episodeId)
    <*> areq intField "Time" (Just 0)
  where
    nodes' = (flip map) nodes (\(Entity tid x) -> (nodeTitle x, tid))

getPodcastEpisodeR :: Text -> Int -> Handler RepHtmlJson
getPodcastEpisodeR name number = do
    entity@(Entity episodeId episode) <- runDB $ getBy404 $ UniqueEpisodeNumber name number
    episodeDoc <- runDB $ documentFromEpisode entity
    nodes <- runDB $ selectList [] [Asc NodeTitle]
    (formWidget, enctype) <- generateFormPost $ newNodeInstanceForm episodeId nodes
    let widget = do
        setTitle $ toHtml $ name <> " #" <> (pack $ show number) <> " - " <> (episodeTitle episode)
        $(widgetFile "episodes/show")
    let json = episodeDoc
    defaultLayoutJson widget json

instance FromJSON Day where
    parseJSON val = do
        let res = (fromJSON val) :: (Result UTCTime)
        case res of
            (Success utc) -> return $ utctDay utc
            (Error msg) -> error msg

instance ToJSON Day where
    toJSON d = toJSON $ UTCTime d $ secondsToDiffTime 0

data EpisodeHACK = EpisodeHACK {
    hack__id :: Maybe String,
    hack_podcast :: Text,
    hack_title :: Text,
    hack_number :: Int,
    hack_searchSlug :: Maybe Text,
    hack_airDate :: Maybe Day,
    hack_published :: Bool,
    hack_duration :: Maybe Int
} deriving (Show, Generic)

$(deriveJSON (removePrefix "hack_") ''EpisodeHACK)

newEpisodeForm :: Maybe Text -> Form EpisodeHACK
newEpisodeForm maybePodcast = renderDivs $ EpisodeHACK
    <$> aopt hiddenField "" Nothing
    <*> areq textField "Podcast" maybePodcast
    <*> areq textField "Title" Nothing
    <*> areq intField "Number" Nothing
    <*> aopt textField "Search slug" Nothing
    <*> aopt (jqueryDayField def
                { jdsChangeYear = True -- give a year dropdown
                , jdsYearRange = "1980:-1" -- 1900 till five years ago
                }) "Air date" Nothing
    <*> areq checkBoxField "Published" (Just False)
    <*> aopt intField "Duration" Nothing

{-
getNewEpisodeR :: Text -> Handler RepHtml
getNewEpisodeR podcast = do
    (formWidget, enctype) <- generateFormPost $ newEpisodeForm $ Just podcast
    defaultLayout $ do
        setTitle "New Episode"
        $(widgetFile "episodes/new")
-}

postEpisodesR :: Handler RepJson
postEpisodesR = do
    (source, episodeHACK) <- fromJsonOrFormPost $ newEpisodeForm Nothing
    let episode = episodeFromEpisodeHACK episodeHACK
    _ <- ensurePodcast $ episodePodcast episode
    entity <- ensureEpisode episode
    case source of
        PostJson -> jsonToRepJson entity
        PostForm -> redirect $ PodcastEpisodeR (episodePodcast episode) (episodeNumber episode)
  where
    ensurePodcast name =
        let item = Podcast name Nothing Nothing Nothing
            constraint = UniquePodcastName name
        in ensureEntity item constraint
    ensureEpisode ep =
        let constraint = UniqueEpisodeNumber (episodePodcast ep) (episodeNumber ep)
        in ensureEntity ep constraint
    episodeFromEpisodeHACK (EpisodeHACK _ podcast title number slug date published duration) =
        let dTime = secondsToDiffTime 0
            utc = do
                d <- date
                return $ UTCTime d dTime
        in Episode podcast title number slug utc published duration