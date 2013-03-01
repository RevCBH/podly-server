{-# LANGUAGE DeriveGeneric, RankNTypes #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time
import Database.Persist.Quasi

import Data.Aeson hiding (object)
import Data.Aeson.TH (deriveJSON)
import Control.Applicative ((<$>), (<*>))

import GHC.Generics (Generic)

-- Type sig imports
--import qualified Control.Monad.IO.Class
import Database.Persist.GenericSql.Raw (SqlBackend)
--

data MediaKind =
  AudioMp3
  | VideoVimeo
  | VideoYouTube
 deriving (Show, Read, Eq, Generic, Enum)
derivePersistField "MediaKind"

$(deriveJSON id ''MediaKind)

data PublishedState =
  StateDraft
  | StateSubmitted
  | StatePending
  | StatePublished
 deriving (Show, Read, Eq, Generic, Enum)
derivePersistField "PublishedState"

$(deriveJSON id ''PublishedState)

data Privilege =
  -- | View
  AsEditor
  | AsManager
  | AsPublisher
  | AsAdmin
  deriving (Show, Read, Eq, Generic, Enum)
derivePersistField "Privilege"
$(deriveJSON id ''Privilege)

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity Event) where
  toJSON (Entity tid (Event title data_ created)) = object
    [ "_id" .= tid,
      "title" .= title,
      "data" .= data_,
      "created" .= (toJSON created)]

instance FromJSON NodeType where
  parseJSON (Object o) = NodeType <$>
    o .: "title"                  <*>
    o .: "icon"
  parseJSON _  = error "Object expected when parsing NodeType"

--instance ToJSON (Entity NodeType) where
--  toJSON (Entity tid (NodeType title icon)) = object
--    [ "_id" .= tid,
--      "title" .= title,
--      "icon" .= icon]

--instance FromJSON (NodeGeneric t) where
--  parseJSON (Object o) = Node <$>
--    o .: "title"              <*>
--    o .: "url"                <*>
--    o .: "linkTitle"          <*>
--    o .: "nodeTypeId"
--  parseJSON _  = error "Object expected when parsing Node"

instance FromJSON NodeInstance where
  parseJSON (Object o) = NodeInstance <$>
    o .: "title"                      <*>
    o .: "url"                        <*>
    o .: "nodeTypeId"                 <*>
    o .: "episodeId"                  <*>
    o .: "time"
  parseJSON _ = error "Object expected when parsing NodeInstance"

--instance ToJSON (Entity Node) where
--  toJSON (Entity tid n) = object
--    [ "_id" .= tid,
--      "title" .= nodeTitle n]

instance ToJSON (Entity Episode) where
  toJSON (Entity tid (Episode podcast title number slug airDate published duration lastModified)) = object
    [ "_id" .= tid,
      "podcast" .= podcast,
      "title" .= title,
      "number" .= number,
      "searchSlug" .= slug,
      "published" .= published,
      "duration" .= duration,
      "airDate" .= (toJSON airDate),
      "lastModified" .= (toJSON lastModified)]

touchEpisode :: (PersistQuery m, PersistMonadBackend m ~ SqlBackend) => Key Episode -> m ()
touchEpisode episodeId = do
  curT <- liftIO getCurrentTime
  update episodeId [EpisodeLastModified =. curT]

--instance FromJSON (EpisodeGeneric t) where
--  parseJSON (Object o) = Episode <$>
--    o .: "podcast"               <*>
--    o .: "title"                 <*>
--    o .: "number"                <*>
--    o .: "searchSlug"            <*>
--    o .: "airDate"               <*>
--    o .: "published"             <*>
--    o .: "duration"
--  parseJSON _  = error "Object expected when parsing Episode"

instance ToJSON (Entity Podcast) where
  toJSON (Entity tid (Podcast name description category image)) = object
    [ "_id" .= tid,
      "name" .= name,
      "description" .= description,
      "category" .= category,
      "image" .= image]

instance ToJSON (Entity Icon) where
  toJSON (Entity _ (Icon name)) = object
    ["name" .= name]

instance ToJSON (Entity User) where
  toJSON (Entity tid (User ident _)) = object
    ["_id" .= tid,
     "identity" .= ident]
