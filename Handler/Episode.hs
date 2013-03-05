{-# LANGUAGE OverloadedStrings #-}
module Handler.Episode where

import Import
--import Handler.Util
import Handler.Home (angularPlayerForEpisode)
import Podly.Affiliate
import qualified Podly.Metadata as Meta

-- toJSON required when deploying for some reason
import Data.Aeson (Result(..), FromJSON(..), fromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Maybe
import qualified Data.Traversable as DT
import qualified Data.ByteString.Char8 as BS
import Data.Time(Day, UTCTime(..), secondsToDiffTime)
import Data.Time.Clock (addUTCTime)
import qualified Data.Time.Format as TF
import System.Locale (defaultTimeLocale)
--import Data.Text (pack, unpack)
import qualified Data.Text as T
--import Control.Monad (liftM)
import Network.HTTP.Types (notModified304)
import Network.HTTP.Types.Header (hIfModifiedSince)
import Network.Wai (requestHeaders)

import Yesod.Form.Jquery

import Document

--import Debug.Trace

parseHeaderTime :: TF.ParseTime b => String -> [b]
parseHeaderTime t =
  -- From RFC 2616 Sec 3.3 http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html
  let formats = [
        --Sun, 06 Nov 1994 08:49:37 GMT
        "%a, %d %b %Y %X %Z",
        --Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
        "%A, %d-%b-%y %X %Z",
        --Sun Nov  6 08:49:37 1994
        "%a %e %X %Y"]
      tryParse x = TF.parseTime defaultTimeLocale x t
  in mapMaybe tryParse formats

ensureStale :: UTCTime -> GHandler sub master ()
ensureStale time = do
  req <- waiRequest
  let setCacheHeaders = do
      --now <- liftIO getCurrentTime
      let fmtT = T.pack . TF.formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"
      -- TODO - determine good max-age instead of 1 year
      setHeader "Cache-Control" "public; max-age=31536000"
      --setHeader "Expires" $ fmtT $ addUTCTime 31536000 now
      setHeader "Last-Modified" $ fmtT time
      return ()

  let mMod = lookup hIfModifiedSince $ requestHeaders req
  case mMod of
    Just t -> do
      let time' = addUTCTime (-1) time
      case parseHeaderTime $ BS.unpack t of
        headerT:[] | time' <= headerT -> sendResponseStatus notModified304 (typeJson, emptyContent)
        _ -> setCacheHeaders
    Nothing -> setCacheHeaders


-- TODO - access control?
renderJsonEpisode :: Entity Episode -> Handler RepJson
renderJsonEpisode entity@(Entity _ episode) = do
  ensureStale $ episodeLastModified episode
  episodeDoc <- runDB $ documentFromEpisode entity
  nodes <- mapM updateNodeUrl $ docEpisodeNodes episodeDoc --episodeDoc

  jsonToRepJson $ episodeDoc { docEpisodeNodes = nodes }
 where
  updateNodeUrl n = do
    case docNodeUrl n of
      Just url -> do
        newUrl <- rewriteLink $ T.unpack url
        return n {docNodeUrl = Just $ T.pack newUrl}
      Nothing -> return n

getPodcastEpisodeR :: Text -> Int -> Handler RepHtmlJson
getPodcastEpisodeR name number = do
    (Entity podcastId _) <- runDB $ getBy404 $ UniquePodcastName name
    episodeEntity <- runDB $ getBy404 $ UniqueEpisodeNumber podcastId number
    (RepHtml html) <- angularPlayerForEpisode episodeEntity
    (RepJson json) <- renderJsonEpisode episodeEntity
    return $ RepHtmlJson html json

getEpisodeR :: EpisodeId -> Handler RepJson -- RepHtmlJson?
getEpisodeR tid = do
   episode <- runDB $ get404 tid
   renderJsonEpisode $ Entity tid episode

{-
data EpisodeThumbnail = EpisodeThumbnail {
  epThumbUrl :: Text,
  epThumbWidth :: Int,
  epThumbHeight :: Int}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "epThumb") ''EpisodeThumbnail)

data EpisodeEphemeral = EpisodeEphemeral {
  epEphem_id :: EpisodeId,
  epEphemNumber :: Int,
  epEphemTitle :: Text,
  epEphemHeaderText :: Text,
  epEphemPreviewImage :: Maybe EpisodeThumbnail }
 deriving (Show, Generic)
$(deriveJSON (removePrefix "epEphem") ''EpisodeEphemeral)

data EmbedPlayerConfig = EmbedPlayerConfig {
  plrCfgGetMoreText :: Text,
  plrCfgBannerText :: Text,
  plrCfgBannerUrl :: Text}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "plrCfg") ''EmbedPlayerConfig)

data EpisodeMeta = EpisodeMeta {
  epMetaPrev :: Maybe EpisodeEphemeral,
  epMetaNext :: Maybe EpisodeEphemeral,
  epMetaEmbedPlayerConfig :: EmbedPlayerConfig}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "epMeta") ''EpisodeMeta)
-}

getEpisodeMetaR :: EpisodeId -> Handler RepJson
getEpisodeMetaR tid = do
  (prev, next) <- runDB $ getPrevNext
  let playerCfg = Meta.EmbedPlayerConfig {
                    Meta.plrCfgGetMoreText = "Get More Episodes",
                    Meta.plrCfgBannerText = "Podly: Wikipedia for podcasts. See more at Podly.co",
                    Meta.plrCfgBannerUrl = "http://podly.co"}
  jsonToRepJson $ Meta.EpisodeEphemeral prev next playerCfg epSocial
 where
  mkEphem headerText (Entity _tid episode) = do
    -- TODO - unify with canonical URL forms
    previewImage <- episodePreviewImageUrl _tid
    let epThumb = do
          url <- previewImage
          -- TODO - don't hardcode dimensions
          return $ Meta.Image url 480 360

    let f = Meta.Episode _tid <$> episodeNumber <*> episodeTitle
    return $ f episode headerText epThumb
  epSocial =
    Meta.EpisodeSocial {
      Meta.epSocTwitter = "Thee Tweetr @thePodly",
      Meta.epSocEmail = Meta.EmailMessage {
        Meta.emailSubject = "Awesome Podly!",
        Meta.emailBody = "For serious: http://podly.co"}}
  getPrevNext = do
    episode <- get404 $ tid
    -- map mkEphem over the results (at most one), convert list to Maybe, move Maybe inside DB monad
    let sel op sort headerText = DT.sequence . listToMaybe . map (mkEphem headerText) =<< selectList
          [EpisodePodcastId ==. episodePodcastId episode,
           op EpisodeNumber $ episodeNumber episode]
          [sort EpisodeNumber, LimitTo 1]
    prev <- sel (<.) Desc "Previous Episode"
    next <- sel (>.) Asc "Next Episode"
    return (prev, next)
  episodePreviewImageUrl episodeId = do
    src <- getBy $ UniqueMediaKindForEpisode episodeId VideoYouTube
    return $ flip fmap src $ \(Entity _ x) -> mconcat ["http://img.youtube.com/vi/", mediaSourceResource x, "/0.jpg"]

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

--postEpisodesR :: Handler RepJson
--postEpisodesR = do
--    (source, episodeHACK) <- fromJsonOrFormPost $ newEpisodeForm Nothing
--    let episode = episodeFromEpisodeHACK episodeHACK
--    _ <- ensurePodcast $ episodePodcast episode
--    entity <- ensureEpisode episode
--    case source of
--        PostJson -> jsonToRepJson entity
--        PostForm -> redirect $ PodcastEpisodeR (episodePodcast episode) (episodeNumber episode)
--  where
--    ensurePodcast name =
--        let item = Podcast name Nothing Nothing Nothing
--            constraint = UniquePodcastName name
--        in ensureEntity item constraint
--    ensureEpisode ep =
--        let constraint = UniqueEpisodeNumber (episodePodcast ep) (episodeNumber ep)
--        in ensureEntity ep constraint
--    episodeFromEpisodeHACK (EpisodeHACK _ podcast title number slug date published duration) =
--        let dTime = secondsToDiffTime 0
--            utc = do
--                d <- date
--                return $ UTCTime d dTime
--        in Episode podcast title number slug utc published duration


getEpisodesR :: Handler RepHtmlJson
getEpisodesR = do
  page <- (paramOr "page" 1) >>= return . max 0 . flip (-) 1
  limit <- paramOr "limit" 8
  let resultsPerPage = min (max 1 limit) 20 :: Int

  episodeCount <- runDB $ count [EpisodePublished ==. StatePublished]
  episodes <- runDB $ selectList [EpisodePublished ==. StatePublished] [Desc EpisodeNumber, LimitTo resultsPerPage, OffsetBy (page * resultsPerPage)]
  let pageCount = (episodeCount `quot` resultsPerPage) + (if episodeCount `mod` resultsPerPage > 0 then 1 else 0)

  let widget = do
      setTitle $ toHtml ("Episodes" :: String)
      [whamlet|There are #{length episodes} episodes.|]

  let pageInfo = object [
        "page" .= (page + 1),
        "of" .= pageCount]

  let json = object [
        "pageInfo" .= pageInfo,
        "episodes" .= episodes]

  defaultLayoutJson widget json
 where
  paramOr name defaultValue = do
    mParam <- lookupGetParam name
    return $ maybe defaultValue (read . T.unpack) mParam
