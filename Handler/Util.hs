{-# LANGUAGE RankNTypes #-}
module Handler.Util where

import Import
import Network.Wai (requestHeaders)
import Data.Maybe (isJust)
import Control.Monad (liftM)

import Debug.Trace

data PostSource = PostForm | PostJson

data Permission =
  HasRole {rolePermission :: Privilege}
  | HasEpisodeGrant EpisodeId Privilege
 deriving (Show)

permIsRole x = case x of HasRole _ -> True; _ -> False
permIsEpGrant x = case x of HasEpisodeGrant _ _ -> True; _ -> False

requireCreateEpisode perms = do
  if AsEditor `elem` (map rolePermission roles)
    then return ()
    else deny
 where
  roles = filter permIsRole perms
  deny = permissionDenied "You don't have permission to create episodes."

requirePermissions userId = do
  perms <- runDB $ getPermissions userId
  liftIO $ traceIO $ "perms: " ++ (show perms)
  if length perms == 0
    then permissionDenied "Insufficient permissions"
    else return perms

getPermissions userId = do
  roles <-  mapRoles $ selectList [RoleUserId ==. userId] []
  epGrants <- mapEpGrants $ selectList [EpisodeGrantUserId ==. userId] []
  return $ mconcat [roles, epGrants]
 where
  mapRoles = liftM $ map $ \(Entity _ x) -> HasRole . rolePrivilege $ x
  mapEpGrants =
    let mkGrant (Entity _ x) = HasEpisodeGrant <$> episodeGrantEpisodeId <*> episodeGrantPrivilege $ x
    in liftM (map mkGrant)

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