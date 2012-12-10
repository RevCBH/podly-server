{-# LANGUAGE DeriveGeneric #-}
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

data MediaKind =
  AudioMp3
  | VideoVimeo
  | VideoYouTube
 deriving (Show, Read, Eq, Generic, Enum)
derivePersistField "MediaKind"

$(deriveJSON id ''MediaKind)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity Event) where
  toJSON (Entity tid (Event title data_ created)) = object
    [ "_id" .= tid,
      "title" .= title,
      "data" .= data_,
      "created" .= (toJSON created)]

instance FromJSON (NodeTypeGeneric t) where
  parseJSON (Object o) = NodeType <$>
    o .: "title"                  <*>
    o .: "icon"
  parseJSON _  = error "Object expected when parsing NodeType"

--instance ToJSON (Entity NodeType) where
--  toJSON (Entity tid (NodeType title icon)) = object
--    [ "_id" .= tid,
--      "title" .= title,
--      "icon" .= icon]

instance FromJSON (NodeGeneric t) where
  parseJSON (Object o) = Node <$>
    o .: "title"              <*>
    o .: "url"                <*>
    o .: "linkTitle"          <*>
    o .: "nodeTypeId"
  parseJSON _  = error "Object expected when parsing Node"

instance FromJSON (NodeInstanceGeneric t) where
  parseJSON (Object o) = NodeInstance <$>
    o .: "nodeId"                     <*>
    o .: "nodeTypeId"                 <*>
    o .: "episodeId"                  <*>
    o .: "time"
  parseJSON _ = error "Object expected when parsing NodeInstance"

instance ToJSON (Entity Node) where
  toJSON (Entity tid n) = object
    [ "_id" .= tid,
      "title" .= nodeTitle n]

instance ToJSON (Entity Episode) where
  toJSON (Entity tid (Episode podcast title number slug airDate published duration)) = object
    [ "_id" .= tid,
      "podcast" .= podcast,
      "title" .= title,
      "number" .= number,
      "searchSlug" .= slug,
      "published" .= published,
      "duration" .= duration,
      "airDate" .= (toJSON airDate)]

instance FromJSON (EpisodeGeneric t) where
  parseJSON (Object o) = Episode <$>
    o .: "podcast"               <*>
    o .: "title"                 <*>
    o .: "number"                <*>
    o .: "searchSlug"            <*>
    o .: "airDate"               <*>
    o .: "published"             <*>
    o .: "duration"
  parseJSON _  = error "Object expected when parsing Episode"

instance ToJSON (Entity Podcast) where
  toJSON (Entity tid (Podcast name description category image)) = object
    [ "_id" .= tid,
      "name" .= name,
      "description" .= description,
      "category" .= category,
      "image" .= image]





