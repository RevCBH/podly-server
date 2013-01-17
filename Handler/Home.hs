{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import Yesod.Angular
import Yesod.Default.Config (appExtra)
import qualified Network.Wai as W

import Handler.Util
import Podly.Auth

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector as V
import qualified Data.Aeson as A
import Data.Text (pack, unpack)
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Text.Coffee (coffeeFile)
import Control.Monad (liftM)
import Control.Monad.Trans.Maybe
import Text.Hamlet (hamletFile)

import Debug.Trace

import Document

handleHomeR :: Handler RepHtml
handleHomeR = do
  (Entity _ episode):_ <- runDB $ selectList [EpisodePublished ==. StatePublished] [Desc EpisodeNumber]
  nodeTypes <- runDB $ selectList [] [Asc NodeTypeTitle]
  let nodeTypesJson = L8.unpack $ encode $ map documentFromNodeType nodeTypes

  mStartAt <- lookupSession "StartAt"
  startAt <- case mStartAt of
                  Just x -> do
                    deleteSession "StartAt"
                    return x
                  Nothing -> return "0"
  req <- waiRequest
  let proto = if W.isSecure req then "https" else "http" :: String
  --let hostname = B8.unpack $ W.serverName req
  let hostname = "podly.co"
  --let port = if (W.serverPort req) `elem` [80, 443] then "" else (":" ++ show (W.serverPort req))
  let port = ""
  let approot = proto ++ "://" ++ hostname ++ port ++ "/"

  let cfg = ModuleConfig (Just "playerMod") Nothing
  runNgModule cfg $ do
    cmdSetNodeInstance <- addCommand $ \() -> do
      notFound
      return $ Singleton ("OK" :: String)

    $(addLib "filters")
    $(addLib "models")
    $(addLib "media")
    $(addCtrl "/player/:podcastName/:episodeNumber" "player")

    setDefaultRoute $ pack $ "/player/The Joe Rogan Experience/" ++ (show $ episodeNumber episode)

getPlayNodeR :: NodeInstanceId -> Handler RepHtml
getPlayNodeR nid = do
  inst <- runDB $ get404 nid
  ep <- runDB $ get404 (nodeInstanceEpisodeId inst)
  setSession (pack "StartAt") (pack . show $ nodeInstanceTime inst)
  -- HACK
  redirect $ "/#/player/" ++ (unpack $ episodePodcast ep) ++ "/" ++ (show $ episodeNumber ep)

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

  let startAt = "0" :: String
  let approot = "http://podly.co/" :: String
  let cfg = ModuleConfig (Just "playerMod") (Just embeddedLayout)
  runNgModule cfg $ do
    cmdSetNodeInstance <- addCommand $ \() -> do
      notFound
      return $ Singleton ("OK" :: String)

    $(addLib "filters")
    $(addLib "models")
    $(addLib "media")
    $(addCtrl "/:podcastName/:episodeNumber" "embedded/player")
    -- $(addCtrl "/:epId" "embedded/player")

    setDefaultRoute $ pack $ "/The Joe Rogan Experience/" ++ (show $ episodeNumber episode)

newtype Singleton a = Singleton { unSingleton :: a }
instance A.ToJSON a => A.ToJSON (Singleton a) where
    toJSON = Array . V.singleton . A.toJSON . unSingleton
instance A.FromJSON a => A.FromJSON (Singleton a) where
    parseJSON (Array a) =
        case V.toList a of
            [x] -> Singleton <$> A.parseJSON x
            _ -> fail "Not a single-element array"
    parseJSON _ = fail "Not an array"


--tryInsertEpisode :: Episode -> Handler (Entity Episode)
--tryInsertEpisode episode = do
--  _ <- ensurePodcast $ episodePodcast episode
--  ensureEpisode episode
-- where
--  ensurePodcast name =
--      let item = Podcast name Nothing Nothing Nothing
--          constraint = UniquePodcastName name
--      in ensureEntity item constraint
--  ensureEpisode ep =
--      let constraint = UniqueEpisodeNumber (episodePodcast ep) (episodeNumber ep)
--      in ensureEntity ep constraint
--tryInsertEpisode episode = do
--  episodeFromDocument episode

tryInsertNodeType :: NodeType -> Handler (Entity NodeType)
tryInsertNodeType nodeType = do
  ensureEntity nodeType $ UniqueTypeTitle $ nodeTypeTitle nodeType

handleAdminR :: Handler RepHtml
handleAdminR = do
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
      (Entity tid _) <- runDB $ episodeFromDocument (ep :: EpisodeDocument)
      --doc <- runDB $ documentFromEpisode episode
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
        insert $ MediaSource epId kind offset res
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
      let rights = foldr (\x m -> Map.adjust (appendGrant x) (episodeGrantUserId $ selEntity x) m) userTbl grants
      -- Convert to JSON compatible format
      let mapPair f g = map (\(a, b) -> (f a, g b))
      let txtId = pack . L8.unpack . encode
      let objPair (i, xs) = object ["ident" .= i, "grants" .= xs]
      return $ object $ mapPair txtId objPair $ Map.assocs rights

    cmdGrant <- addCommand $ \(userId, perm) ->
      case perm of
        HasRole role -> do
          requireGrant role rights
          _ <- runDB $ insert $ Role userId role
          return $ Singleton ("OK" :: String)
        HasEpisodeGrant epId role -> do
          requireGrantOnEp role epId rights
          _ <- runDB $ insert $ EpisodeGrant userId role epId
          runDB $ touchEpisode epId
          return $ Singleton ("OK" :: String)

    cmdRevoke <- addCommand $ \(userId, perm) ->
      case perm of
        HasRole role -> do
          requireGrant role rights
          runDB $ deleteWhere [RoleUserId ==. userId, RolePrivilege ==. role]
          return $ Singleton ("OK" :: String)
        HasEpisodeGrant epId role -> do
          requireGrantOnEp role epId rights
          runDB $ deleteWhere [EpisodeGrantUserId ==. userId,
                               EpisodeGrantPrivilege ==. role,
                               EpisodeGrantEpisodeId ==. epId]
          return $ Singleton ("OK" :: String)

    cmdGetUserForEmail <- addCommand $ \(Singleton email) -> runDB $ getBy404 $ UniqueUser email

    addExtLib "ui"
    $(addLib "filters")
    $(addLib "models")
    $(addLib "media")

    $(addCtrl "/podcasts" "podcastIndex")
    $(addCtrl "/podcasts/:podcastName" "showPodcast")
    $(addCtrl "/podcasts/:podcastName/episodes.new" "newEpisode")
    $(addCtrl "/podcasts/:podcastName/episodes/:episodeNumber" "editEpisode")
    $(addCtrl "/users" "admin/users/index")

    setDefaultRoute "/podcasts"