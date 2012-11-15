{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fixtures (readFixture, loadFixture, loadEpisodes) where

import Import
import System.Directory(getCurrentDirectory)
import qualified Data.ByteString.Lazy as L
import qualified Control.Monad.IO.Class(MonadIO)

import Document

readFixture :: String -> (FromJSON a) => IO (Maybe [a])
readFixture f = do
  dirName <- getCurrentDirectory
  vals <- L.readFile $ dirName ++ "/fixtures/" ++ f ++ ".json"
  return $ decode vals

--loadFixture :: (PersistEntity a) => IO [a] -> Handler [Key a]
loadFixture :: (PersistStore (YesodPersistBackend master) (GHandler sub master), PersistEntity a, YesodPersist master) => 
  IO [a] -> GHandler sub master [Key (YesodPersistBackend master) a]
loadFixture loader = liftIO loader >>= runDB . (mapM insert)

-- create an action that loads an episode document fixture into the persistent store
loadEpisodes :: (Control.Monad.IO.Class.MonadIO (backend m), PersistUnique backend m) => 
  String -> backend m ()
loadEpisodes name = do
  docs <- liftIO loadEpisodeDocuments
  mapM_ episodeFromDocument docs
 where
  loadEpisodeDocuments = do
    mDocs <- readFixture name
    case mDocs of
      (Just docs) -> return (docs :: [EpisodeDocument])
      Nothing -> error "Couldn't load episode fixture"