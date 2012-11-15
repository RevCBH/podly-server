module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

import Data.Aeson hiding (object)
import Control.Applicative ((<$>), (<*>))

share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


instance ToJSON (Entity Event) where
  toJSON (Entity tid (Event title data_ created)) = object
    [ "_id" .= tid,
      "title" .= title,
      "data" .= data_,
      "created" .= (toJSON created)]

instance ToJSON (Entity Node) where
  toJSON (Entity tid n) = object
    [ "_id" .= tid,
      "title" .= nodeTitle n]

instance ToJSON (Entity Episode) where
  toJSON (Entity tid (Episode podcast title number slug airDate published)) = object
    [ "_id" .= tid,
      "podcast" .= podcast,
      "title" .= title,
      "number" .= number,
      "searchSlug" .= slug,
      "published" .= published,
      "airDate" .= (toJSON airDate)]

instance FromJSON (EpisodeGeneric t) where
  parseJSON (Object o) = Episode <$>
    o .: "podcast"               <*>
    o .: "title"                 <*>
    o .: "number"                <*>
    o .: "searchSlug"            <*>
    o .: "airDate"               <*>
    o .: "published"
  parseJSON _  = error "Object expected when parsing Episode"

instance ToJSON (Entity Podcast) where
  toJSON (Entity tid (Podcast name description category image)) = object
    [ "_id" .= tid,
      "name" .= name,
      "description" .= description,
      "category" .= category,
      "image" .= image]





