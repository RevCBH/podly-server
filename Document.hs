{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Document (
  NodeTypeDocument(..),
  NodeDocument(..), EpisodeDocument(..),
  MediaSourceDocument(..),
  episodeFromDocument,
  documentFromEpisode,
  nodeTypeIdFromDoc,
  documentFromNodeType) where

import Import

import Data.Aeson.TH (deriveJSON)
import Data.Maybe (isJust, fromJust)
import Data.Time(UTCTime)

import Control.Monad (filterM)
import Control.Monad.Trans.Maybe

type MaybeDB = (PersistStore backend m) => MaybeT (backend m)

data NodeTypeDocument = DocNodeT {
  docNodeType_id :: Maybe NodeTypeId,
  docNodeTypeIcon :: Text,
  docNodeTypeTitle :: Text
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docNodeType") ''NodeTypeDocument)

data NodeDocument = DocNode {
  docNode_id :: Maybe NodeId,
  docNodeRelId :: Maybe NodeInstanceId,
  docNodeTitle :: Text,
  docNodeUrl :: Text,
  docNodeLinkTitle :: Text,
  docNodeTime :: Int,
  docNodeNodeType :: NodeTypeDocument
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
  docEpisodeMediaSources :: [MediaSourceDocument],
  docEpisodeNodes :: [NodeDocument]
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docEpisode") ''EpisodeDocument)

type DB a = PersistUnique backend m => backend m (a backend)
type DBK a backend = Key backend (a backend)
type DBKey a = DB (DBK a)

--documentFromNodeType :: Entity (NodeTypeGeneric t) -> NodeTypeDocument
documentFromNodeType x = do
  let Entity tid (NodeType title icon) = x
  DocNodeT (Just tid) icon title

--documentFromNodeInstance :: PersistStore t m => Entity (NodeInstanceGeneric t) -> t m (Maybe NodeDocument)
--documentFromNodeInstance :: Entity NodeInstance -> MaybeDB NodeDocument
documentFromNodeInstance (Entity relId x) = do
  let nodeId = nodeInstanceNodeId x
  (Node title url linkTitle mNodeTypeId) <- (MaybeT . get) nodeId
  defNodeTypeId <- liftMaybe mNodeTypeId
  nt <- (MaybeT . get) defNodeTypeId
  let time = nodeInstanceTime x
  let docFromNT = documentFromNodeType (Entity defNodeTypeId nt)
  return $ DocNode (Just nodeId) (Just relId) title url linkTitle time $ docFromNT
 where
  liftMaybe = MaybeT . return --maybe mzero return

documentFromMediaSource (Entity tid (MediaSource _ kind offset url)) = do
  DocMediaSource kind url offset

--documentFromEpisode :: PersistQuery backend m => Entity (EpisodeGeneric backend) -> backend m EpisodeDocument
documentFromEpisode episode = do
  let Entity tid (Episode podcast title number slug airDate _ duration) = episode
  instancesWithIds <- selectList [NodeInstanceEpisodeId ==. tid] [Asc NodeInstanceTime]
  mNodes <- mapM (runMaybeT . documentFromNodeInstance) instancesWithIds
  nodes <- filterM (return . isJust) mNodes
  justNodes <- mapM (return . fromJust) nodes
  mediaSources <- mapM (return . documentFromMediaSource) =<< selectList [MediaSourceEpisodeId ==. tid] []
  return $ DocEpisode (Just tid) podcast number airDate title slug duration mediaSources justNodes

--nodeTypeIdFromDoc :: NodeTypeDocument -> DBKey NodeTypeGeneric
nodeTypeIdFromDoc doc = do
  mNT <- getBy $ UniqueTypeTitle $ docNodeTypeTitle doc
  case mNT of
    Nothing -> insert $ NodeType (docNodeTypeTitle doc) (docNodeTypeIcon doc)
    Just (Entity tid _) -> return tid

--nodeIdAndTimeFromDoc :: NodeDocument -> (DBKey NodeGeneric, String)
--nodeIdAndTimeFromDoc :: PersistUnique backend m => NodeDocument -> backend m (Key backend (NodeGeneric backend), Key backend (NodeTypeGeneric backend), Int)
nodeIdAndTimeFromDoc doc = do
  let DocNode nodeId _ title url linkTitle time ntDoc = doc
  mNode <- getBy $ UniqueNodeTitle title
  typeId <- nodeTypeIdFromDoc ntDoc
  tid <- case mNode of
    Nothing -> insert $ Node title url linkTitle (Just typeId)
    Just (Entity tid _) -> return tid
  return (tid, typeId, time)

nodeInstanceIdFromNodeInEpisode :: PersistUnique backend m =>
  Int
  -> Key backend (EpisodeGeneric backend)
  -> Key backend (NodeGeneric backend)
  -> Key backend (NodeTypeGeneric backend)
  -> backend m (Key backend (NodeInstanceGeneric backend))
nodeInstanceIdFromNodeInEpisode time episodeId nodeId nodeTypeId = do
  mNI <- getBy $ UniqueInstanceEpisodeTime episodeId time
  case mNI of
    Nothing -> insert $ NodeInstance nodeId nodeTypeId episodeId time
    Just (Entity tid _) -> return tid

-- episodeAndIdFromDoc :: EpisodeDocument -> (DBKey EpisodeGeneric, Episode)
episodeAndIdFromDoc :: PersistUnique backend m =>
  EpisodeDocument
  -> backend m (Key backend (EpisodeGeneric backend), EpisodeGeneric backend)

episodeAndIdFromDoc doc =
  let DocEpisode _ podcast number airDate title slug duration _ _ = doc
  in do
    mPodcast <- getBy $ UniquePodcastName podcast
    case mPodcast of
      Nothing -> do
        _ <- insert $ Podcast {podcastName = podcast,
                               podcastDescription = Nothing,
                               podcastImage = Nothing,
                               podcastCategory = Nothing}
        return ()
      _ -> return ()

    mEpisode <- getBy $ UniqueEpisodeNumber podcast number
    case mEpisode of
      Nothing -> do
        let ep = Episode podcast title number slug airDate True duration
        tid <- insert ep
        return (tid, ep)
      Just (Entity tid ep) -> return (tid, ep)

--episodeFromDocument :: EpisodeDocument -> DB EpisodeGeneric
episodeFromDocument doc = do
  (episodeId, episode) <- episodeAndIdFromDoc doc
  nodeData <- mapM nodeIdAndTimeFromDoc $ docEpisodeNodes doc
  mapM_ (reifyInstances episodeId) nodeData

  return episode
 where
  reifyInstances episodeId (nodeId, nodeTypeId, time) = nodeInstanceIdFromNodeInEpisode time episodeId nodeId nodeTypeId

