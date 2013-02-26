{-# LANGUAGE OverloadedStrings #-}
module HomeTest
    ( homeSpecs,
      playerSpecs
    ) where

import Prelude (($), return, undefined, (>>=))
import TestImport
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Maybe (Maybe(..))
import Control.Monad.IO.Class (liftIO)

defaultEpisode :: Episode
defaultEpisode =
  Episode { episodePodcast = "TestCast",
            episodeTitle = "Testpisodezz",
            episodeNumber = 42,
            episodeSlug = Nothing,
            episodeAirDate = Nothing,
            episodePublished = StatePublished,
            episodeDuration = Nothing,
            episodeLastModified = UTCTime (fromGregorian 2013 1 1) (secondsToDiffTime 0)
          }

defaultMediaSourceFor :: Key Episode -> MediaSource
defaultMediaSourceFor episodeId =
  MediaSource { mediaSourceEpisodeId = episodeId,
                mediaSourceKind = VideoYouTube,
                mediaSourceOffset = 0,
                mediaSourceResource = "zefside"
              }

resetDB = do
  deleteWhere ([] :: [Filter MediaSource])
  deleteWhere ([] :: [Filter Episode])
  return ()

insertDefaults = do
  episodeId <- insert defaultEpisode
  _ <- insert $ defaultMediaSourceFor episodeId
  return ()

homeSpecs :: Specs
homeSpecs = do
  describe "Loading the default episode from /" $ do
    it "fails with 404 if no episodes are available" $ do
      runDB $ resetDB

      get_ "/"
      statusIs 404

    it "loads the default and checks that it looks right" $ do
      runDB $ do
        resetDB
        insertDefaults

      get_ "/"
      statusIs 200
      htmlAllContain "title" "Podly"

      -- Verify that OpenGraph tags aren't present
      htmlCount "meta" 1

playerSpecs :: Specs
playerSpecs = do
  describe "Loading a specific episode" $ do
    it "has appropriate Facebook OpenGraph tags" $ do
      runDB $ do
          resetDB
          insertDefaults

      --get_ "/podcasts/TestCast/episodes/42"
      get_ "/"
      -- Check FB meta tags
      let openGraphProperties = ["title", "description", "type", "url", "site_name",
                                 "video", "video:type", "video:height", "video:width"]

      htmlCount "meta[property=video]" 1
      --post "/" $ do
      --  addNonce
      --  fileByLabel "Choose a file" "tests/main.hs" "text/plain" -- talk about self-reference
      --  byLabel "What's on the file?" "Some Content"

      --statusIs 200
      --htmlCount ".message" 1
      --htmlAllContain ".message" "Some Content"
      --htmlAllContain ".message" "text/plain"
