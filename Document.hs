{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Document (NodeTypeDocument(..), NodeDocument(..), EpisodeDocument(..), episodeFromDocument, documentFromEpisode, nodeTypeIdFromDoc) where

import Import

import Data.Aeson.TH (deriveJSON)
import Data.Maybe (isJust, fromJust)
import Data.Time(UTCTime)

import Control.Monad (filterM)

data NodeTypeDocument = DocNodeT {
  docNodeType_id :: Maybe NodeTypeId,
  docNodeTypeIcon :: Text,
  docNodeTypeTitle :: Text
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docNodeType") ''NodeTypeDocument)

data NodeDocument = DocNode {
  docNode_id :: Maybe NodeId,
  docNodeTitle :: Text,
  docNodeUrl :: Text,
  docNodeLinkTitle :: Text,
  docNodeTime :: Text,
  docNodeNodeType :: NodeTypeDocument
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docNode") ''NodeDocument)

data EpisodeDocument = DocEpisode {
  docEpisodePodcast :: Text,
  docEpisodeNumber :: Int,
  docEpisodeAirDate :: UTCTime,
  docEpisodeTitle :: Text,
  docEpisodeSearchSlug :: Text,
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
documentFromNodeInstance (Entity _ x) = do
  let nodeId = nodeInstanceNodeId x
  maybeNode <- get nodeId
  case maybeNode of
    Just (Node title url linkTitle nodeTypeId) -> do
      maybeType <- get nodeTypeId
      case maybeType of
        Just nt -> do
          let time = nodeInstanceTime x
          return $ Just $ DocNode (Just nodeId) title url linkTitle time $ documentFromNodeType (Entity nodeTypeId nt)
        Nothing -> return Nothing
    Nothing -> return Nothing

--documentFromEpisode :: PersistQuery backend m => Entity (EpisodeGeneric backend) -> backend m EpisodeDocument
documentFromEpisode episode = do
  let Entity tid (Episode podcast title number slug airDate _) = episode
  instancesWithIds <- selectList [NodeInstanceEpisodeId ==. tid] [Asc NodeInstanceTime]
  mNodes <- mapM documentFromNodeInstance instancesWithIds
  nodes <- filterM (return . isJust) mNodes
  justNodes <- mapM (return . fromJust) nodes

  return $ DocEpisode podcast number airDate title slug justNodes

nodeTypeIdFromDoc :: NodeTypeDocument -> DBKey NodeTypeGeneric
nodeTypeIdFromDoc doc = do
  mNT <- getBy $ UniqueTypeTitle $ docNodeTypeTitle doc
  case mNT of
    Nothing -> insert $ NodeType (docNodeTypeTitle doc) (docNodeTypeIcon doc)
    Just (Entity tid _) -> return tid

--nodeIdAndTimeFromDoc :: NodeDocument -> (DBKey NodeGeneric, String)
nodeIdAndTimeFromDoc :: PersistUnique backend m => NodeDocument -> backend m (Key backend (NodeGeneric backend), Text)
nodeIdAndTimeFromDoc doc = do
  let DocNode nodeId title url linkTitle time ntDoc = doc
  mNode <- getBy $ UniqueNodeTitle title
  tid <- case mNode of
    Nothing -> do
      typeId <- nodeTypeIdFromDoc ntDoc
      insert $ Node title url linkTitle typeId
    Just (Entity tid _) -> return tid
  return (tid, time)

nodeInstanceIdFromNodeInEpisode :: PersistUnique backend m =>
  Text
  -> Key backend (EpisodeGeneric backend)
  -> Key backend (NodeGeneric backend)
  -> backend m (Key backend (NodeInstanceGeneric backend))

nodeInstanceIdFromNodeInEpisode time episodeId nodeId = do
  mNI <- getBy $ UniqueInstanceEpisodeTime episodeId time
  case mNI of
    Nothing -> insert $ NodeInstance nodeId Nothing episodeId time
    Just (Entity tid _) -> return tid

-- episodeAndIdFromDoc :: EpisodeDocument -> (DBKey EpisodeGeneric, Episode)
episodeAndIdFromDoc :: PersistUnique backend m =>
  EpisodeDocument
  -> backend m (Key backend (EpisodeGeneric backend), EpisodeGeneric backend)

episodeAndIdFromDoc doc =
  let DocEpisode podcast number airDate title slug _ = doc
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
        let ep = Episode podcast title number slug airDate True
        tid <- insert ep
        return (tid, ep)
      Just (Entity tid ep) -> return (tid, ep)

episodeFromDocument :: EpisodeDocument -> DB EpisodeGeneric
episodeFromDocument doc = do
  (episodeId, episode) <- episodeAndIdFromDoc doc
  let nodes = docEpisodeNodes doc
  nodeData <- mapM nodeIdAndTimeFromDoc nodes
  mapM_ (reifyInstances episodeId) nodeData

  return episode
 where
  reifyInstances episodeId (nodeId, time) = nodeInstanceIdFromNodeInEpisode time episodeId nodeId

