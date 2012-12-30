{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import Yesod.Angular

import Handler.Util
import Podly.Auth

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Vector as V
import qualified Data.Aeson as A
import Data.Text (pack)
import Data.Maybe (fromJust)
import Text.Coffee (coffeeFile)
import Control.Monad (liftM)
import Control.Monad.Trans.Maybe

import Debug.Trace

import Document

handleHomeR :: Handler RepHtml
handleHomeR = do
  (Entity _ episode):_ <- runDB $ selectList [EpisodePublished ==. StatePublished] [Desc EpisodeNumber]
  nodeTypes <- runDB $ selectList [] [Asc NodeTypeTitle]
  let nodeTypesJson = L8.unpack $ encode $ map documentFromNodeType nodeTypes

  runNgModule (Just "playerMod") $ do
    cmdSetNodeInstance <- addCommand $ \() -> do
      notFound
      return $ Singleton ("OK" :: String)

    $(addLib "filters")
    $(addLib "models")
    $(addLib "media")
    $(addCtrl "/player/:podcastName/:episodeNumber" "player")

    setDefaultRoute $ pack $ "/player/The Joe Rogan Experience/" ++ (show $ episodeNumber episode)

newtype Singleton a = Singleton { unSingleton :: a }
instance A.ToJSON a => A.ToJSON (Singleton a) where
    toJSON = Array . V.singleton . A.toJSON . unSingleton
instance A.FromJSON a => A.FromJSON (Singleton a) where
    parseJSON (Array a) =
        case V.toList a of
            [x] -> Singleton <$> A.parseJSON x
            _ -> fail "Not a single-element array"
    parseJSON _ = fail "Not an array"


tryInsertEpisode :: Episode -> Handler (Entity Episode)
tryInsertEpisode episode = do
  _ <- ensurePodcast $ episodePodcast episode
  ensureEpisode episode
 where
  ensurePodcast name =
      let item = Podcast name Nothing Nothing Nothing
          constraint = UniquePodcastName name
      in ensureEntity item constraint
  ensureEpisode ep =
      let constraint = UniqueEpisodeNumber (episodePodcast ep) (episodeNumber ep)
      in ensureEntity ep constraint

tryInsertNodeType :: NodeType -> Handler (Entity NodeType)
tryInsertNodeType nodeType = do
  ensureEntity nodeType $ UniqueTypeTitle $ nodeTypeTitle nodeType

handleAdminR :: Handler RepHtml
handleAdminR = do
  (Entity userId user) <- requireAuth
  rights <- requirePermissions userId

  icons <- runDB $ selectList [] [Asc IconName]

  runNgModule (Just "admin") $ do
    cmdCreateEpisode <- addCommand $ \ep -> do
      requireCreateEpisode rights
      episode <- tryInsertEpisode ep
      return episode

    cmdSetEpisodeTitle <- addCommand $ \(epId, title) -> do
      requireEditEpisode rights epId
      runDB $ update epId [EpisodeTitle =. title]
      episode <- runDB $ get404 epId
      return $ Singleton $ episodeTitle episode

    cmdSetEpisodeNumber <- addCommand $ \(epId, number) -> do
      requireEditEpisode rights epId
      runDB $ update epId [EpisodeNumber =. number]
      episode <- runDB $ get404 epId
      return $ Singleton $ episodeNumber episode

    cmdCreateNodeType <- addCommand $ \nt -> do
      nodeType <- tryInsertNodeType nt
      return $ documentFromNodeType nodeType

    cmdDeleteNode <- addCommand $ \(Singleton rel) -> do
      ins <- runDB $ get404 (rel :: NodeInstanceId)
      requireEditEpisode rights $ nodeInstanceEpisodeId ins
      runDB $ delete rel
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
      episode <- runDB $ episodeFromDocument ep
      doc <- runDB $ documentFromEpisode (Entity tid episode)
      return doc

    cmdSubmitForReview <- addCommand $ \(Singleton epId) -> do
      requireEditEpisode rights epId
      runDB $ update epId [EpisodePublished =. StateSubmitted]
      episode <- runDB $ get404 epId
      doc <- runDB $ documentFromEpisode (Entity epId episode)
      return doc

    cmdGrant <- addCommand $ \(userId, perm) ->
      case perm of
        HasRole role -> do
          requireGrant role rights
          _ <- runDB $ insert $ Role userId role
          return $ Singleton ("OK" :: String)
        HasEpisodeGrant epId role -> do
          requireGrantOnEp role epId rights
          _ <- runDB $ insert $ EpisodeGrant userId role epId
          return $ Singleton ("OK" :: String)

    $(addLib "filters")
    $(addLib "models")
    $(addLib "media")

    $(addCtrl "/podcasts" "podcastIndex")
    $(addCtrl "/podcasts/:podcastName" "showPodcast")
    $(addCtrl "/podcasts/:podcastName/episodes.new" "newEpisode")
    $(addCtrl "/podcasts/:podcastName/episodes/:episodeNumber" "editEpisode")
    $(addCtrl "/users" "admin/users/index")

    setDefaultRoute "/podcasts"