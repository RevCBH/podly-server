{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Test where

import Import
import Data.Aeson (Result(..), ToJSON(..), FromJSON(..), fromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import Data.Time(addUTCTime, getCurrentTime, Day, UTCTime(..), secondsToDiffTime)
import Data.Text (pack)
import qualified Data.HashMap.Strict as Map

import Fixtures
import Document
import Handler.Util

import Yesod.Form.Jquery

getTestR :: Handler RepJson
getTestR = do
    now <- liftIO getCurrentTime
    _ <- runDB $ insert $ Event "foo" "bar" now
    events <- runDB $ selectList [EventCreated >. fiveHoursBefore now] []
    jsonToRepJson events
  where
    fiveHoursBefore = addUTCTime (-5*60*60)

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
    runDB $ deleteWhere ([] :: [Filter Event])
    runDB $ deleteWhere ([] :: [Filter Episode])
    runDB $ deleteWhere ([] :: [Filter Node])
    runDB $ deleteWhere ([] :: [Filter NodeType])
    runDB $ deleteWhere ([] :: [Filter NodeInstance])
    runDB $ deleteWhere ([] :: [Filter Podcast])

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

getPodcastR :: Text -> Handler RepHtmlJson
getPodcastR name = do
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
    docs <- runDB $ mapM documentFromEpisode episodes
    jsonToRepJson docs

newNodeInstanceForm :: EpisodeId -> [Entity Node] -> Form NodeInstance
newNodeInstanceForm episodeId nodes = renderDivs $ NodeInstance
    <$> areq (selectFieldList nodes') "Node" Nothing
    <*> aopt hiddenField "" Nothing
    <*> areq hiddenField "" (Just episodeId)
    <*> areq textField "Time" (Just "00:00:00")
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
    hack_searchSlug :: Text,
    hack_airDate :: Day,
    hack_published :: Bool
} deriving (Show, Generic)

$(deriveJSON (removePrefix "hack_") ''EpisodeHACK)

newEpisodeForm :: Maybe Text -> Form EpisodeHACK
newEpisodeForm maybePodcast = renderDivs $ EpisodeHACK
    <$> aopt hiddenField "" Nothing
    <*> areq textField "Podcast" maybePodcast
    <*> areq textField "Title" Nothing
    <*> areq intField "Number" Nothing
    <*> areq textField "Search slug" Nothing
    <*> areq (jqueryDayField def
                { jdsChangeYear = True -- give a year dropdown
                , jdsYearRange = "1980:-1" -- 1900 till five years ago
                }) "Air date" Nothing
    <*> areq checkBoxField "Published" (Just False)

getNewEpisodeR :: Text -> Handler RepHtml
getNewEpisodeR podcast = do
    (formWidget, enctype) <- generateFormPost $ newEpisodeForm $ Just podcast
    defaultLayout $ do
        setTitle "New Episode"
        $(widgetFile "episodes/new")

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
    episodeFromEpisodeHACK (EpisodeHACK _ podcast title number slug date published) =
        let dTime = secondsToDiffTime 0
            utc = UTCTime date dTime
        in Episode podcast title number slug utc published