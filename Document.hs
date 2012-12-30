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

--type MaybeDB = (PersistStore backend m) => MaybeT (backend m)
-- DEBUG start
import Debug.Trace
-- DEBUG end

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
  docEpisodeMediaSources :: [MediaSourceDocument],
  docEpisodeNodes :: [NodeDocument]
} deriving (Show, Generic)

$(deriveJSON (removePrefix "docEpisode") ''EpisodeDocument)

type DB a = PersistUnique backend m => backend m (a backend)
type DBK a backend = Key backend (a backend)
type DBKey a = DB (DBK a)

--documentFromNodeType :: Entity (NodeTypeGeneric t) -> NodeTypeDocument
documentFromNodeType (Entity tid (NodeType title icon)) = DocNodeT (Just tid) icon title

--documentFromNodeInstance :: PersistStore t m => Entity (NodeInstanceGeneric t) -> t m (Maybe NodeDocument)
--documentFromNodeInstance :: Entity NodeInstance -> MaybeDB NodeDocument

maybeDocFromMaybeNodeTypeId Nothing = return Nothing
maybeDocFromMaybeNodeTypeId (Just ntid) = do
  nt <- MaybeT . get $ ntid
  return . Just . documentFromNodeType $ (Entity ntid nt)

documentFromNodeInstance (Entity relId (NodeInstance title mUrl mNodeTypeId episodeId time)) = do
  mNtDoc <- maybeDocFromMaybeNodeTypeId mNodeTypeId
  return $ DocNode (Just relId) title mUrl time $ mNtDoc

documentFromMediaSource (Entity tid (MediaSource _ kind offset resource)) = do
  DocMediaSource kind resource offset

--documentFromEpisode :: PersistQuery backend m => Entity (EpisodeGeneric backend) -> backend m EpisodeDocument
documentFromEpisode episode = do
  let Entity tid (Episode podcast title number slug airDate published duration) = episode
  instancesWithIds <- selectList [NodeInstanceEpisodeId ==. tid] [Asc NodeInstanceTime]
  mNodes <- mapM (runMaybeT . documentFromNodeInstance) instancesWithIds
  nodes <- filterM (return . isJust) mNodes
  justNodes <- mapM (return . fromJust) nodes
  mediaSources <- mapM (return . documentFromMediaSource) =<< selectList [MediaSourceEpisodeId ==. tid] []
  return $ DocEpisode (Just tid) podcast number airDate title slug duration published mediaSources justNodes

--nodeTypeIdFromDoc :: NodeTypeDocument -> DBKey NodeTypeGeneric
nodeTypeIdFromDoc doc = do
  mNT <- getBy $ UniqueTypeTitle $ docNodeTypeTitle doc
  case mNT of
    Nothing -> insert $ NodeType (docNodeTypeTitle doc) (docNodeTypeIcon doc)
    Just (Entity tid _) -> return tid

--nodeIdAndTimeFromDoc :: NodeDocument -> (DBKey NodeGeneric, String)
--nodeIdAndTimeFromDoc :: PersistUnique backend m => NodeDocument -> backend m (Key backend (NodeGeneric backend), Key backend (NodeTypeGeneric backend), Int)
--nodeInstanceIdAndTimeFromDoc (DocNode relId _ _ _ time ntDoc) = do
--  typeId <- nodeTypeIdFromDoc ntDoc
--  return (relId, typeId, time)

nodeInstanceIdFromNodeInEpisode :: PersistUnique backend m =>
  Int
  -> Text -- title
  -> Maybe Text -- url
  -> Key backend (EpisodeGeneric backend)
  -- -> Key backend (NodeGeneric backend)
  -> Maybe (Key backend (NodeTypeGeneric backend))
  -> backend m (Key backend (NodeInstanceGeneric backend))
nodeInstanceIdFromNodeInEpisode time title mUrl episodeId mNodeTypeId = do
  mNI <- getBy $ UniqueInstanceEpisodeTime episodeId time
  case mNI of
    Nothing -> insert $ NodeInstance title mUrl mNodeTypeId episodeId time
    Just (Entity tid _) -> return tid

episodeAndIdFromDoc :: PersistUnique backend m =>
  EpisodeDocument
  -> backend m (Key backend (EpisodeGeneric backend), EpisodeGeneric backend)
episodeAndIdFromDoc (DocEpisode _ podcast number airDate title slug duration published _ _) = do
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
      let ep = Episode podcast title number slug airDate published duration
      tid <- insert ep
      return (tid, ep)
    Just (Entity tid ep) -> return (tid, ep)

    {-
    NodeInstance
    title Text
    url Text
    nodeTypeId NodeTypeId
    episodeId EpisodeId Eq
    time Int
    UniqueInstanceEpisodeTime episodeId time
    -}

maybeMaybeNodeTypeIdFromDoc Nothing = return Nothing
maybeMaybeNodeTypeIdFromDoc (Just doc) = return . Just =<< nodeTypeIdFromDoc doc

syncInstance episodeId (DocNode mRelId title mUrl time mNodeTypeDoc) = do
  mNodeTypeId <- maybeMaybeNodeTypeIdFromDoc mNodeTypeDoc
  let getNewInstanceId = insert $ NodeInstance title mUrl mNodeTypeId episodeId time

  relId <-  case mRelId of
              Just tid  -> return tid
              Nothing   -> getNewInstanceId

  ins <- liftM fromJust $ get relId
  let updatePlan = map snd $ filter fst $ [(title /= nodeInstanceTitle ins, NodeInstanceTitle =. title),
                                           (mUrl /= nodeInstanceUrl ins, NodeInstanceUrl =. mUrl),
                                           (time /= nodeInstanceTime ins, NodeInstanceTime =. time),
                                           (mNodeTypeId /= nodeInstanceNodeTypeId ins, NodeInstanceNodeTypeId =. mNodeTypeId)]

  mIns <- if length updatePlan > 0
            then do
              update relId updatePlan
              get relId
            else
              return $ Just ins

  return (Entity relId $ fromJust mIns)

  --mInstance <- get relId

syncMediaSource episodeId (DocMediaSource kind resource offset) = do
  mSource <- getBy $ UniqueMediaKindForEpisode episodeId kind
  case mSource of
    Just (Entity tid source) -> do
      -- TODO - update plan ?
      update tid [MediaSourceResource =. resource, MediaSourceOffset =. offset]
      source' <- get tid
      return $ Entity tid $ fromJust source'
    Nothing -> do
      let source = MediaSource episodeId kind offset resource
      tid <- insert source
      return $ Entity tid source

--episodeFromDocument :: EpisodeDocument -> DB EpisodeGeneric
episodeFromDocument doc = do
  liftIO $ traceIO "\nepisodeFromDocument:"
  (episodeId, episode) <- episodeAndIdFromDoc doc
  liftIO $ traceIO $ "\tepisodeId:" ++ (show episodeId)
  mapM_ (syncInstance episodeId) $ docEpisodeNodes doc
  liftIO $ traceIO "\tsync'd instances"
  mapM_ (syncMediaSource episodeId) $ docEpisodeMediaSources doc
  liftIO $ traceIO $ "\tsync'd sources (count: " ++ (show $ length $ docEpisodeMediaSources doc) ++ ")"

  return $ Entity episodeId episode
 --where
 -- syncInstance (DocNode relId title url _ time nodeTypeDoc) =

  --reifyInstance episodeId (mRelId, nodeTypeId, time) =
  --  nodeInstanceIdFromNodeInEpisode time episodeId nodeId nodeTypeId

--data NodeDocument = DocNode {
--  docNodeRelId :: Maybe NodeInstanceId,
--  docNodeTitle :: Text,
--  docNodeUrl :: Text,
--  docNodeLinkTitle :: Text,
--  docNodeTime :: Int,
--  docNodeNodeType :: NodeTypeDocument
--} deriving (Show, Generic)