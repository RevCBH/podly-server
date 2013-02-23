{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Document (
  NodeTypeDocument(..),
  NodeDocument(..), EpisodeDocument(..),
  MediaSourceDocument(..),
  episodeFromDocument,
  documentFromEpisode,
  nodeTypeIdFromDoc,
  syncInstance,
  documentFromNodeInstance,
  documentFromNodeType) where

import Import

import Data.Aeson.TH (deriveJSON)
import Data.Maybe (isJust, fromJust)
import Data.Time(UTCTime)

import Control.Monad (liftM, filterM)
import Control.Monad.Trans.Maybe
import Data.Time.Clock (getCurrentTime)

--import Debug.Trace

-- Type signature imports
import qualified Database.Persist.GenericSql.Raw
import qualified Control.Monad.Trans.Resource
import qualified Control.Monad.IO.Class
import qualified Control.Monad.Logger

data NodeTypeDocument = DocNodeT {
  docNodeType_id :: Maybe NodeTypeId,
  docNodeTypeIcon :: Text,
  docNodeTypeTitle :: Text
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docNodeType") ''NodeTypeDocument)

data NodeDocument = DocNode {
  docNodeRelId :: Maybe NodeInstanceId,
  docNodeTitle :: Text,
  docNodeUrl :: Maybe Text,
  -- docNodeLinkTitle :: Text,
  docNodeTime :: Int,
  docNodeNodeType :: Maybe NodeTypeDocument
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docNode") ''NodeDocument)

data MediaSourceDocument = DocMediaSource {
  docSourceKind :: MediaKind,
  docSourceResource :: Text,
  docSourceOffset :: Int

} deriving (Show, Generic)

$(deriveJSON (removePrefix "docSource") ''MediaSourceDocument)

data EpisodeDocument = DocEpisode {
  docEpisode_id :: Maybe EpisodeId,
  docEpisodePodcast :: Text,
  docEpisodeNumber :: Int,
  docEpisodeAirDate :: Maybe UTCTime,
  docEpisodeTitle :: Text,
  docEpisodeSearchSlug :: Maybe Text,
  docEpisodeDuration :: Maybe Int,
  docEpisodePublished :: PublishedState,
  docEpisodeLastModified :: Maybe UTCTime,
  docEpisodeMediaSources :: [MediaSourceDocument],
  docEpisodeNodes :: [NodeDocument]
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docEpisode") ''EpisodeDocument)

-- Type synonyms for use in signatures
type MaybeDb x = forall (m :: * -> *). (Control.Monad.Trans.Resource.MonadThrow m,
                                          Control.Monad.IO.Class.MonadIO m,
                                          Control.Monad.Trans.Resource.MonadUnsafeIO m,
                                          Control.Monad.Logger.MonadLogger m, MonadBaseControl IO m) =>
                                          MaybeT
                                            (Database.Persist.GenericSql.Raw.SqlPersist m)
                                            x
type DbVal x = forall (m :: * -> *). (Control.Monad.Trans.Resource.MonadThrow m,
                                      Control.Monad.IO.Class.MonadIO m,
                                      Control.Monad.Trans.Resource.MonadUnsafeIO m,
                                      Control.Monad.Logger.MonadLogger m, MonadBaseControl IO m) =>
                                        Database.Persist.GenericSql.Raw.SqlPersist m x
type DbKey x = Key Database.Persist.GenericSql.Raw.SqlPersist x

documentFromNodeType :: Entity NodeType -> NodeTypeDocument
documentFromNodeType (Entity tid (NodeType title icon)) = DocNodeT (Just tid) icon title

documentFromNodeInstance :: Entity NodeInstance -> MaybeDb NodeDocument
documentFromNodeInstance (Entity relId (NodeInstance title mUrl mNodeTypeId _ time)) = do
  mNtDoc <- maybeDocFromMaybeNodeTypeId mNodeTypeId
  return $ DocNode (Just relId) title mUrl time $ mNtDoc
 where
  maybeDocFromMaybeNodeTypeId Nothing = return Nothing
  maybeDocFromMaybeNodeTypeId (Just ntid) = do
    nt <- MaybeT . get $ ntid
    return . Just . documentFromNodeType $ (Entity ntid nt)

documentFromMediaSource :: Entity MediaSource -> MediaSourceDocument
documentFromMediaSource (Entity _ (MediaSource _ kind offset resource)) =
  DocMediaSource kind resource offset

documentFromEpisode :: Entity Episode -> DbVal EpisodeDocument
documentFromEpisode episode = do
  let Entity tid (Episode podcast title number slug airDate published duration lastModified) = episode
  instancesWithIds <- selectList [NodeInstanceEpisodeId ==. tid] [Asc NodeInstanceTime]
  mNodes <- mapM (runMaybeT . documentFromNodeInstance) instancesWithIds
  nodes <- filterM (return . isJust) mNodes
  justNodes <- mapM (return . fromJust) nodes
  mediaSources <- mapM (return . documentFromMediaSource) =<< selectList [MediaSourceEpisodeId ==. tid] []
  return $ DocEpisode (Just tid) podcast number airDate title slug duration published (Just lastModified) mediaSources justNodes

nodeTypeIdFromDoc :: PersistUnique backend m =>
                     NodeTypeDocument -> backend m (Key backend (NodeTypeGeneric backend))
nodeTypeIdFromDoc doc = do
  mNT <- getBy $ UniqueTypeTitle $ docNodeTypeTitle doc
  case mNT of
    Nothing -> insert $ NodeType (docNodeTypeTitle doc) (docNodeTypeIcon doc)
    Just (Entity tid _) -> return tid

syncInstance :: DbKey Episode -> NodeDocument -> DbVal (Entity NodeInstance)
syncInstance episodeId (DocNode mRelId title mUrl time mNodeTypeDoc) = do
  mNodeTypeId <- maybeMaybeNodeTypeIdFromDoc mNodeTypeDoc
  let getNewInstanceId = insert $ NodeInstance title mUrl mNodeTypeId episodeId time

  relId <-  case mRelId of
              Just tid  -> return tid
              Nothing   -> do
                touchEpisode episodeId
                getNewInstanceId

  ins <- liftM fromJust $ get relId
  let updatePlan = map snd $ filter fst $ [(title /= nodeInstanceTitle ins, NodeInstanceTitle =. title),
                                           (mUrl /= nodeInstanceUrl ins, NodeInstanceUrl =. mUrl),
                                           (time /= nodeInstanceTime ins, NodeInstanceTime =. time),
                                           (mNodeTypeId /= nodeInstanceNodeTypeId ins, NodeInstanceNodeTypeId =. mNodeTypeId)]

  mIns <- if length updatePlan > 0
            then do
              update relId updatePlan
              touchEpisode episodeId
              get relId
            else
              return $ Just ins

  return (Entity relId $ fromJust mIns)
 where
  maybeMaybeNodeTypeIdFromDoc Nothing = return Nothing
  maybeMaybeNodeTypeIdFromDoc (Just doc) = return . Just =<< nodeTypeIdFromDoc doc

episodeFromDocument :: EpisodeDocument -> DbVal (Entity Episode)
episodeFromDocument doc = do
  (Entity episodeId episode) <- episodeAndIdFromDoc doc
  mapM_ (syncInstance episodeId) $ docEpisodeNodes doc
  mapM_ (syncMediaSource episodeId) $ docEpisodeMediaSources doc

  return $ Entity episodeId episode
 where
  syncMediaSource episodeId (DocMediaSource kind resource offset) = do
    mSource <- getBy $ UniqueMediaKindForEpisode episodeId kind
    case mSource of
      Just (Entity tid _) -> do
        -- TODO - update plan ?
        update tid [MediaSourceResource =. resource, MediaSourceOffset =. offset]
        source' <- get tid
        return $ Entity tid $ fromJust source'
      Nothing -> do
        let source = MediaSource episodeId kind offset resource
        tid <- insert source
        return $ Entity tid source
  episodeAndIdFromDoc (DocEpisode _ podcast number airDate title slug duration published _ _ _) = do
    mPodcast <- getBy $ UniquePodcastName podcast
    case mPodcast of
      Nothing -> do
        _ <- insert $ Podcast {podcastName = podcast, podcastDescription = Nothing,
                               podcastImage = Nothing, podcastCategory = Nothing}
        return ()
      _ -> return ()
    mEpisode <- getBy $ UniqueEpisodeNumber podcast number
    case mEpisode of
      Nothing -> do
        curT <- liftIO getCurrentTime
        let ep = Episode podcast title number slug airDate published duration curT
        tid <- insert ep
        return (Entity tid ep)
      Just entity -> return entity