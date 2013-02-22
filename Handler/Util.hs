{-# LANGUAGE RankNTypes #-}
module Handler.Util where

import Import
import Network.Wai (requestHeaders)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)

data PostSource = PostForm | PostJson

isJsonBody :: Handler Bool
isJsonBody = do
  req <- waiRequest
  return $ ("Content-type", "application/json") `elem` (requestHeaders req)

fromJsonOrFormPost :: FromJSON t => Form t -> Handler (PostSource, t)
fromJsonOrFormPost form = do
  jsonBody <- isJsonBody
  if jsonBody
    then do
      result <- parseJsonBody_
      return (PostJson, result)
    else do
      ((result, widget), enctype) <- runFormPost form
      case result of
        FormSuccess result' -> return (PostForm, result')
        _ -> notFound

ensureEntity :: forall master sub val.
                               (PersistEntity val,
                                PersistUnique (YesodPersistBackend master) (GHandler sub master),
                                YesodPersist master,
                                PersistEntityBackend val ~ YesodPersistBackend master) =>
                               val
                               -> Unique val (YesodPersistBackend master)
                               -> GHandler sub master (Entity val)
ensureEntity item constraint = do
  mEntity <- runDB $ getBy $ constraint
  case mEntity of
    Just entity -> return entity
    Nothing -> do
      tid <- runDB $ insert item
      return $ Entity tid item

maybe404 ::  GHandler sub master (Maybe b) -> GHandler sub master b
maybe404 action = action >>= maybe notFound return

acceptContentType :: GHandler sub master (Maybe Text)
acceptContentType =
  -- request headers -> select content-type -> convert to Text from ByteString
  fmap decodeUtf8 . lookup "Accept" . requestHeaders . reqWaiRequest <$> getRequest

canonicalEpisodeUrl :: Episode -> Text
canonicalEpisodeUrl episode =
  -- TODO make approot non-static
  let approot = "http://podly.co/"
  in mconcat [pack approot, "podcasts/", episodePodcast episode, "/episodes/", pack . show $ episodeNumber episode]

--episodePreviewImageUrl :: PersistUnique backend m =>
--                          Key backend (EpisodeGeneric backend) -> backend m (Maybe Text)
--youtubePreviewImageUrl src =
--  flip fmap src $ \(Entity _ x) -> mconcat ["http://img.youtube.com/vi/", mediaSourceResource x, "/0.jpg"]

episodePreviewImageUrl episodeId = do
  src <- getBy $ UniqueMediaKindForEpisode episodeId VideoYouTube
  return $ flip fmap src $ \(Entity _ x) -> mconcat ["http://img.youtube.com/vi/", mediaSourceResource x, "/0.jpg"]

--episodeVideoUrl :: PersistUnique backend m =>
--                   Key backend (EpisodeGeneric backend) -> backend m (Maybe Text)
episodeVideoUrl episodeId = do
  src <- getBy $ UniqueMediaKindForEpisode episodeId VideoYouTube
  return $ flip fmap src $ \(Entity _ x) -> mconcat ["http://www.youtube.com/v/", mediaSourceResource x, "?version=3&autohide=1"]