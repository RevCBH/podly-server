module Podly.Metadata where

import Import hiding (Episode)
 --(Text, Int, EpisodeId, Show, Generic, Maybe, removePrefix)
--import Model (EpisodeId)
import Data.Aeson.TH (deriveJSON)

data Image = Image {
  imgUrl :: Text,
  imgWidth :: Int,
  imgHeight :: Int}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "img") ''Image)

data EmailMessage = EmailMessage {
  emailSubject :: Text,
  emailBody :: Text}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "email") ''EmailMessage)

data Episode = Episode {
  ep_id :: EpisodeId,
  epNumber :: Int,
  epTitle :: Text,
  epHeaderText :: Text,
  epPreviewImage :: Maybe Image }
 deriving (Show, Generic)
$(deriveJSON (removePrefix "ep") ''Episode)

data EmbedPlayerConfig = EmbedPlayerConfig {
  plrCfgGetMoreText :: Text,
  plrCfgBannerText :: Text,
  plrCfgBannerUrl :: Text}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "plrCfg") ''EmbedPlayerConfig)

data EpisodeSocial = EpisodeSocial {
  epSocTwitter :: Text,
  epSocEmail :: EmailMessage}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "epSoc") ''EpisodeSocial)

data EpisodeEphemeral = EpisodeEphemeral {
  epePrev :: Maybe Episode,
  epeNext :: Maybe Episode,
  epeEmbedPlayerConfig :: EmbedPlayerConfig,
  epeShare :: EpisodeSocial}
 deriving (Show, Generic)
$(deriveJSON (removePrefix "epe") ''EpisodeEphemeral)