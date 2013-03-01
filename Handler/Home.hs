{-# LANGUAGE TupleSections, OverloadedStrings, RankNTypes #-}
module Handler.Home where

import Import
import Yesod.Angular
import Yesod.Default.Config (appExtra)
import qualified Network.Wai as W

import Handler.Util
import Podly.Auth
import qualified Podly.Facebook.OpenGraph.Entities as OG

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as V
import qualified Data.Aeson as A
import Data.String.Utils (splitWs, join)
import Data.Text (pack, unpack)
import Data.List (nub, groupBy, sortBy, head)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Text.Coffee (coffeeFile)
import Control.Monad (liftM)
import Control.Monad.Trans.Maybe
import Text.Hamlet (hamletFile)
import Database.Persist.GenericSql (rawSql, Single(..))
import Database.Persist.GenericSql.Raw (SqlPersist)
import qualified Database.Persist.Store as DSP
import Text.Regex.PCRE

--import Debug.Trace

import Document

-- Imports for type signatures
import Database.Persist.GenericSql (RawSql)
import Database.Persist.GenericSql.Raw (SqlBackend)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Logger (MonadLogger)
--

newtype Singleton a = Singleton { unSingleton :: a }
instance A.ToJSON a => A.ToJSON (Singleton a) where
    toJSON = Array . V.singleton . A.toJSON . unSingleton
instance A.FromJSON a => A.FromJSON (Singleton a) where
    parseJSON (Array a) =
        case V.toList a of
            [x] -> Singleton <$> A.parseJSON x
            _ -> fail "Not a single-element array"
    parseJSON _ = fail "Not an array"

getCrossdomainR :: Handler RepXml
getCrossdomainR = undefined

handleHomeR :: Handler RepHtml
handleHomeR = do
  res <- runDB $ selectList [EpisodePublished ==. StatePublished] [Desc EpisodeNumber, LimitTo 1]
  case res of
    (episodeEntity : _) -> angularPlayerForEpisode episodeEntity
    _ -> notFound

angularPlayerForEpisode :: Entity Episode -> Handler RepHtml
angularPlayerForEpisode entity@(Entity _ episode) = do
  nodeTypes <- runDB $ selectList [] [Asc NodeTypeTitle]
  let nodeTypesJson = L8.unpack $ encode $ map documentFromNodeType nodeTypes

  tParam <- lookupGetParam "t"
  let startAt = case tParam of
                  Nothing -> "0"
                  Just t -> show t

  req <- waiRequest
  let proto = if W.isSecure req then "https" else "http" :: String
  --let hostname = B8.unpack $ W.serverName req
  let hostname = "podly.co"
  --let port = if (W.serverPort req) `elem` [80, 443] then "" else (":" ++ show (W.serverPort req))
  let port = ""
  let _approot = proto ++ "://" ++ hostname ++ port ++ "/"
  let cfg = ModuleConfig (Just "playerMod") Nothing

  episodeDoc <- runDB $ documentFromEpisode entity
  let metaWidget = toWidgetHead $ OG.tags episodeDoc

  runNgModuleWidget cfg metaWidget $ do
    cmdSetNodeInstance <- addCommand $ \() -> do
      _ <- notFound
      return $ Singleton ("OK" :: String)
    cmdSignupEmail <- addCommand $ \(Singleton email) -> do
      if (unpack email) =~ ("[^@]+@[^.]+\\..+" :: String)
        then do
          --_ <- runDB $ insert $ Email email Nothing Nothing
          return $ Singleton ("OK" :: String)
        else
          return $ Singleton ("Error" :: String)

    $(addLib "util")
    $(addLib "models")
    $(addLib "media")
    $(addLib "scroll")
    $(addCtrl "/podcasts/:podcastName/episodes/:episodeNumber" "player")

    setDefaultRoute $ mconcat ["/podcasts/", episodePodcast episode,
                               "/episodes/", pack . show $ episodeNumber episode]

getPlayNodeR :: NodeInstanceId -> Handler RepHtml
getPlayNodeR nid = do
  inst <- runDB $ get404 nid
  ep <- runDB $ get404 (nodeInstanceEpisodeId inst)
  let stAt = (show $ nodeInstanceTime inst)
  -- HACK
  redirect $ "/podcasts/" ++ (unpack $ episodePodcast ep) ++ "/episodes/" ++ (show $ episodeNumber ep) ++ "?t=" ++ stAt

embeddedLayout :: GWidget sub App () -> GHandler sub App RepHtml
embeddedLayout widget = do
  master <- getYesod
  pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      addStylesheet $ StaticR css_embed_css
      $(widgetFile "embedded/default-layout")
  hamletToRepHtml $(hamletFile "templates/embedded/default-layout-wrapper.hamlet")

handleEmbedPlayerR :: EpisodeId -> Handler RepHtml
handleEmbedPlayerR epId = do
  episode <- runDB $ get404 epId
  tParam <- lookupGetParam "t"
  let startAt = case tParam of
                  Nothing -> 0 :: Int
                  Just t -> read . unpack $ t

  let _approot = "http://podly.co/" :: String
  let cfg = ModuleConfig (Just "playerMod") (Just embeddedLayout)
  runNgModule cfg $ do
    cmdSetNodeInstance <- addCommand $ \() -> do
      _ <- notFound
      return $ Singleton ("OK" :: String)

    $(addLib "util")
    $(addLib "models")
    $(addLib "media")
    $(addLib "scroll")
    $(addCtrl "/embed/:episodeId" "embedded/player")

    setDefaultRoute $ pack $ "/embed/" ++ (show $ A.toJSON epId)

-- TODO - pre-populate partials and dispatch on name
--getPartialR :: Text -> Handler RepHtml
--getPartialR name = do
--  pc <- widgetToPageContent $ do
--    toWidget $(hamletFile $ mconcat ["templates/partials/", name, ".hamlet"])
--    toWidgetBody $(coffeeFile $ mconcat ["templates/partials/", name, ".coffee"])
--  hamletToRepHtml [hamlet|
--    ^{pageBody pc}
--  |]

getPartialsPlayerR :: Handler RepHtml
getPartialsPlayerR = do
  pc <- widgetToPageContent $ do
    toWidget $(hamletFile "templates/partials/player.hamlet")
    toWidgetBody $(coffeeFile "templates/partials/player.coffee")
  hamletToRepHtml [hamlet|
    ^{pageBody pc}
  |]

-- TODO - search operators? quote support?
data SearchResult = SearchResult Double (Entity Episode) [NodeDocument]
instance A.ToJSON SearchResult where
  toJSON (SearchResult weight episode nodes) = object
    [ "weight" .= weight,
      "episode" .= (A.toJSON episode),
      "nodes" .= (A.toJSON nodes)]

searchEpisodes :: (MonadResource m, MonadLogger m, RawSql a) => Text -> SqlPersist m [a]
searchEpisodes txt = do
  let plain = DSP.PersistText txt
  let tsquery = DSP.PersistText $ pack $ join "|" $ splitWs $ unpack txt
  --let searchQuery = "SELECT ??, ??, ts_rank_cd(to_tsvector(node_instance.title || ' ' || episode.title), to_tsquery(?)) as \"query_rank\", ts_rank_cd(to_tsvector(node_instance.title || ' ' || episode.title), plainto_tsquery(?)) as \"plain_rank\" FROM episode, node_instance WHERE to_tsvector(node_instance.title || ' ' || episode.title) @@ to_tsquery(?) AND episode.id = node_instance.episode_id ORDER BY ts_rank_cd(to_tsvector(node_instance.title || ' ' || episode.title), plainto_tsquery(?)) DESC, ts_rank_cd(to_tsvector(node_instance.title || ' ' || episode.title), to_tsquery(?)) DESC"
  let searchQuery = "SELECT ??, ??, ts_rank_cd(to_tsvector(node_instance.title), to_tsquery(?)) as \"query_rank\", ts_rank_cd(to_tsvector(node_instance.title), plainto_tsquery(?)) as \"plain_rank\" FROM episode, node_instance WHERE to_tsvector(node_instance.title) @@ to_tsquery(?) AND episode.id = node_instance.episode_id ORDER BY ts_rank_cd(to_tsvector(node_instance.title), plainto_tsquery(?)) DESC, ts_rank_cd(to_tsvector(node_instance.title), to_tsquery(?)) DESC"
  rawSql searchQuery [tsquery, plain, tsquery, plain, tsquery]

reifyTuple :: (Num t, PersistStore m, PersistMonadBackend m ~ SqlBackend) =>
                 (t1, Entity NodeInstance, Single t, Single t) -> MaybeT m (t, t1, NodeDocument)
reifyTuple (ep, ni, (Single qw), (Single pw)) = do
  nodeDoc <- documentFromNodeInstance ni
  return (qw + pw, ep, nodeDoc)

getSearchR :: Handler RepJson
getSearchR = do
  plain <- maybe404 $ lookupGetParam "q"
  resultTuples <- runDB $ searchEpisodes plain
  resultList <- liftM catMaybes $ sequence $ map (runDB . runMaybeT . reifyTuple) resultTuples
  jsonToRepJson $ map makeResult $ sortGroups . groupResults $ resultList
 where
  pickEp (_,x,_) = x :: Entity Episode
  pickEntityId (Entity tid _) = tid
  withSel s f a b = f (s a) (s b) -- perform operation f after applying selector s to agruments
  withEpisodes = withSel pickEp
  --withWeights = withSel (\(x,_,_) -> x)
  entityIdsEq = withSel pickEntityId (==)
  cmpEntityIds = withSel pickEntityId compare
  episodesEq x = withEpisodes entityIdsEq x
  cmpEpisodes x = withEpisodes cmpEntityIds x
  sumWeights = foldl1 (+) . map (\(x,_,_) -> x)
  cmpGroupWeights a b = (sumWeights a) `compare` (sumWeights b)
  groupResults xs = (groupBy episodesEq) . (sortBy cmpEpisodes) $ xs
  sortGroups = reverse . sortBy cmpGroupWeights
  makeResult xs =
    let w = sumWeights xs
        ep = pickEp $ head xs
        ns = foldr (:) [] $ map (\(_,_,x) -> x) xs
    in SearchResult w ep ns

tryInsertNodeType :: NodeType -> Handler (Entity NodeType)
tryInsertNodeType nodeType = do
  ensureEntity nodeType $ UniqueTypeTitle $ nodeTypeTitle nodeType

handleAdminR :: Texts -> Handler RepHtml
handleAdminR _ = do
  (Entity userId user) <- requireAuth
  rights <- requirePermissions userId
  let rightsDoc = L8.unpack $ encode rights
  let userDoc = L8.unpack $ encode $ object ["_id" .= userId, "ident" .= (userIdent user)]

  icons <- runDB $ selectList [] [Asc IconName]

  let cfg = ModuleConfig (Just "admin") Nothing
  runNgModule cfg $ do
    cmdCreateEpisode <- addCommand $ \ep -> do
      requireCreateEpisode rights
      --episode <- tryInsertEpisode ep
      -- TODO - ensure episode doesn't exist
      let episodeUrl = mconcat ["http://podly.co/podcasts/", docEpisodePodcast ep,
                                "/episodes/", pack . show $ docEpisodeNumber ep]
      let startNode = DocNode {
                        docNodeRelId  = Nothing,
                        docNodeTitle  = "Play this episode from the beginning",
                        docNodeUrl    = Just $ episodeUrl,
                        docNodeTime   = 0,
                        docNodeNodeType = Just $ DocNodeT Nothing "start.png" "Start" }
      (Entity tid _) <- runDB . episodeFromDocument $ ep {docEpisodeNodes = startNode : docEpisodeNodes ep}
      return $ ep {docEpisode_id = Just tid}

    cmdSetEpisodeTitle <- addCommand $ \(epId, title) ->
      guardUpdateEpisode requireEditEpisode epId [EpisodeTitle =. title] documentFromEpisode

    cmdSetEpisodeNumber <- addCommand $ \(epId, number) ->
      guardUpdateEpisode requireEditEpisode epId [EpisodeNumber =. number] documentFromEpisode

    cmdCreateNodeType <- addCommand $ \nt -> do
      nodeType <- tryInsertNodeType nt
      return $ documentFromNodeType nodeType

    cmdDeleteNode <- addCommand $ \(Singleton rel) -> do
      ins <- runDB $ get404 (rel :: NodeInstanceId)
      let epId = nodeInstanceEpisodeId ins
      requireEditEpisode rights epId
      runDB $ do delete rel; touchEpisode epId
      return $ Singleton ("OK" :: String)

    cmdSetNodeInstance <- addCommand $ \(epId, node) -> do
      requireEditEpisode rights epId
      ins <- runDB $ syncInstance epId node
      doc <- runDB $ runMaybeT $ documentFromNodeInstance ins
      return doc

    cmdReplaceNodes <- addCommand $ \ep -> do
      (Entity tid _) <- runDB $ getBy404 $ UniqueEpisodeNumber (docEpisodePodcast ep) (docEpisodeNumber ep)
      requireEditEpisode rights tid
      runDB $ deleteWhere [NodeInstanceEpisodeId ==. tid]
      (Entity _ episode) <- runDB $ episodeFromDocument ep
      doc <- runDB $ documentFromEpisode (Entity tid episode)
      return doc

    cmdRemoveMediaSource <- addCommand $ \(epId, kind) -> do
      requireManageEpisode rights epId
      runDB $ deleteWhere [MediaSourceEpisodeId ==. epId, MediaSourceKind ==. kind]
      runDB $ touchEpisode epId
      return $ Singleton ("OK" :: String)

    cmdAddMediaSource <- addCommand $ \(epId, DocMediaSource kind res offset) -> do
      requireManageEpisode rights epId
      runDB $ do
        _ <- insert $ MediaSource epId kind offset res
        touchEpisode epId
      return $ Singleton ("OK" :: String)

    cmdSubmitForReview <- addCommand $ \(Singleton epId) ->
      guardUpdateEpisode requireSubmitEpisode epId [EpisodePublished =. StateSubmitted] documentFromEpisode

    cmdPublishEpisode <- addCommand $ \(Singleton epId) -> --do
      guardUpdateEpisode requirePublishEpisode epId [EpisodePublished =. StatePublished] documentFromEpisode

    cmdUnpublishEpisode <- addCommand $ \(Singleton epId) ->
      guardUpdateEpisode requirePublishEpisode epId [EpisodePublished =. StatePending] documentFromEpisode

    cmdGrantsForEpisode <- addCommand $ \(Singleton epId) -> do
      grants <- runDB $ selectList [EpisodeGrantEpisodeId ==. epId] [Asc EpisodeGrantUserId]
      let selEntity (Entity _ x) = x
      let selId (Entity x _) = x
      let userIds = nub $ map (episodeGrantUserId . selEntity) grants
      users <- runDB $ selectList [UserId <-. userIds] [Asc UserIdent]

      -- Map user ids to (user ident, [] :: [HasEpisodeGrant])
      let userTbl = foldr (\x m -> Map.insert (selId x) (userIdent $ selEntity x, []) m) Map.empty users
      -- Convert an Entity to a HasEpisodeGrant
      let mkGrant = (HasEpisodeGrant <$> episodeGrantEpisodeId <*> episodeGrantPrivilege) . selEntity
      -- Collect HasEpisodeGrants grouped by userIds
      let appendGrant x (i, xs) = (i, (mkGrant x : xs))
      let _rights = foldr (\x m -> Map.adjust (appendGrant x) (episodeGrantUserId $ selEntity x) m) userTbl grants
      -- Convert to JSON compatible format
      let mapPair f g = map (\(a, b) -> (f a, g b))
      let txtId = pack . L8.unpack . encode
      let objPair (i, xs) = object ["ident" .= i, "grants" .= xs]
      return $ object $ mapPair txtId objPair $ Map.assocs _rights

    cmdGrant <- addCommand $ \(_userId, perm) ->
      case perm of
        HasRole role -> do
          requireGrant role rights
          _ <- runDB $ insert $ Role _userId role
          return $ Singleton ("OK" :: String)
        HasEpisodeGrant epId role -> do
          requireGrantOnEp role epId rights
          _ <- runDB $ insert $ EpisodeGrant _userId role epId
          runDB $ touchEpisode epId
          return $ Singleton ("OK" :: String)

    cmdRevoke <- addCommand $ \(_userId, perm) ->
      case perm of
        HasRole role -> do
          requireGrant role rights
          runDB $ deleteWhere [RoleUserId ==. _userId, RolePrivilege ==. role]
          return $ Singleton ("OK" :: String)
        HasEpisodeGrant epId role -> do
          requireGrantOnEp role epId rights
          runDB $ deleteWhere [EpisodeGrantUserId ==. _userId,
                               EpisodeGrantPrivilege ==. role,
                               EpisodeGrantEpisodeId ==. epId]
          return $ Singleton ("OK" :: String)

    cmdGetUserForEmail <- addCommand $ \(Singleton email) -> runDB $ getBy404 $ UniqueUser email

    addExtLib "ui"
    $(addLib "util")
    $(addLib "models")
    $(addLib "media")

    $(addCtrl "/admin/podcasts" "podcastIndex")
    $(addCtrl "/admin/podcasts/:podcastName" "showPodcast")
    $(addCtrl "/admin/podcasts/:podcastName/episodes.new" "newEpisode")
    $(addCtrl "/admin/podcasts/:podcastName/episodes/:episodeNumber" "editEpisode")
    $(addCtrl "/admin/users" "admin/users/index")

    setDefaultRoute "/admin/podcasts"