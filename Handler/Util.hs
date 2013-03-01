{-# LANGUAGE RankNTypes #-}
module Handler.Util where

import Import
import Network.Wai (requestHeaders)
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
      ((result, _), _) <- runFormPost form
      case result of
        FormSuccess result' -> return (PostForm, result')
        _ -> notFound

ensureEntity :: (PersistEntity val, YesodPersist master,
                  PersistUnique (YesodPersistBackend master (GHandler sub master)),
                  PersistMonadBackend
                    (YesodPersistBackend master (GHandler sub master))
                  ~ PersistEntityBackend val) =>
                 val -> Unique val -> GHandler sub master (Entity val)
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