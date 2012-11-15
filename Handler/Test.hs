{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Test where

import Import
import Data.Aeson (Result(..), ToJSON(..))
import Data.List (nub)
import Data.Maybe
import Data.Time(addUTCTime, getCurrentTime, Day, UTCTime(..), secondsToDiffTime)
import Data.Text (pack)
import qualified Data.HashMap.Strict as Map
import Network.Wai (requestHeaders)

import Fixtures
import Document

import Yesod.Form.Jquery

isJsonBody :: Handler Bool
isJsonBody = do
    req <- waiRequest
    return $ ("Content-type", "application/json") `elem` (requestHeaders req)    

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

getNodeR :: NodeId -> Handler RepHtmlJson
getNodeR nid = do
    node <- runDB $ get404 nid
    refs <- runDB $ selectList [NodeInstanceNodeId ==. nid] []
    let episodeIds = nub $ (flip map) refs (\(Entity _ x) -> nodeInstanceEpisodeId x)
    episodes <- runDB $ selectList [EpisodeId <-. episodeIds] [Asc EpisodePodcast, Desc EpisodeNumber]
    let widget = do
        setTitle $ toHtml $ "Node - " <> (nodeTitle node)
        $(widgetFile "nodes/show")
    let json = Entity nid node
    defaultLayoutJson widget json

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

getPodcastEpisodeR :: Text -> Int -> Handler RepHtmlJson
getPodcastEpisodeR name number = do
    entity@(Entity _ episode) <- runDB $ getBy404 $ UniqueEpisodeNumber name number
    episodeDoc <- runDB $ documentFromEpisode entity
    let widget = do
        setTitle $ toHtml $ name <> " #" <> (pack $ show number) <> " - " <> (episodeTitle episode)
        $(widgetFile "episodes/show")
    let json = episodeDoc
    defaultLayoutJson widget json

data EpisodeHACK = EpisodeHACK Text Text Int Text Day Bool

newEpisodeForm :: Form EpisodeHACK
newEpisodeForm = renderDivs $ EpisodeHACK
    <$> areq textField "Podcast" Nothing
    <*> areq textField "Title" Nothing
    <*> areq intField "Number" Nothing
    <*> areq textField "Search slug" Nothing
    <*> areq (jqueryDayField def
                { jdsChangeYear = True -- give a year dropdown
                , jdsYearRange = "1980:-1" -- 1900 till five years ago
                }) "Air date" Nothing
    <*> areq checkBoxField "Published" (Just False)

getNewEpisodeR :: Handler RepHtml
getNewEpisodeR = do
    (formWidget, enctype) <- generateFormPost newEpisodeForm
    defaultLayout $ do 
        setTitle "New Episode"
        $(widgetFile "episodes/new")

ensurePodcast name = do
    mPodcast <- runDB $ getBy $ UniquePodcastName name
    case mPodcast of
        Just entity@(Entity _ _) -> return entity
        Nothing -> do
            let pc = Podcast name Nothing Nothing Nothing
            tid <- runDB $ insert pc
            return (Entity tid pc)

ensureEpisode ep = do
    _ <- ensurePodcast $ episodePodcast ep
    mEpisode <- runDB $ getBy $ UniqueEpisodeNumber (episodePodcast ep) (episodeNumber ep)
    case mEpisode of
        Just entity -> return entity
        Nothing -> do
            tid <- runDB $ insert ep
            return $ Entity tid ep

data PostSource = PostForm | PostJson

postEpisodesR :: Handler RepJson
postEpisodesR = do
    (source, episode) <- episodeFromJsonOrForm
    entity <- ensureEpisode episode
    case source of
        PostJson -> jsonToRepJson entity
        PostForm -> redirect $ PodcastEpisodeR (episodePodcast episode) (episodeNumber episode)
  where
    -- ISSUE - this is generalizable but needs specific support b/c of EpisodeHACK
    episodeFromJsonOrForm = do
        jsonBody <- isJsonBody
        if jsonBody
            then do 
                episode <- parseJsonBody_
                return (PostJson, episode)
            else do
                ((result, widget), enctype) <- runFormPost newEpisodeForm
                case result of
                    FormSuccess episode -> return (PostForm, episodeFromEpisodeHACK episode)
                    _ -> notFound
    episodeFromEpisodeHACK (EpisodeHACK podcast title number slug date published) =
        let dTime = secondsToDiffTime 0
            utc = UTCTime date dTime
        in Episode podcast title number slug utc published
