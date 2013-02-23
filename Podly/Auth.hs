{-# LANGUAGE DeriveGeneric, RankNTypes #-}
module Podly.Auth where

import Import
import Data.List (find)
import Control.Monad (liftM)
import Data.Monoid (Any(..), getAny)
import Data.Text (pack)

import Data.Aeson.TH (deriveJSON)

-- Imports for types
import Database.Persist.GenericSql.Raw (SqlPersist)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (MonadUnsafeIO, MonadThrow)
import Control.Monad.Logger (MonadLogger)
import Yesod.Auth (YesodAuth, AuthId)
import Database.Persist.Query.Internal (Update)

data Permission =
  HasRole {
      rolePermission :: Privilege
    }
  | HasEpisodeGrant {
      epGrantEpisodeId :: EpisodeId,
      epGrantPermission :: Privilege
    }
 deriving (Show, Generic, Eq)

$(deriveJSON (removeFirstPrefix ["role", "epGrant"]) ''Permission)

permIsRole :: Permission -> Bool
permIsRole x = case x of HasRole _ -> True; _ -> False

permIsEpGrant :: Permission -> Bool
permIsEpGrant x = case x of HasEpisodeGrant _ _ -> True; _ -> False

requireRole :: Privilege -> Text -> [Permission] -> GHandler sub master ()
requireRole role msg perms = do
  if role `elem` (map rolePermission roles)
    then return ()
    else permissionDenied msg
 where
  roles = filter permIsRole perms

requireEpisodePermission :: Privilege -> Text -> [Permission] -> EpisodeId -> GHandler sub master ()
requireEpisodePermission r msg perms epId = do
  case find grantsRight perms of
    Just _ -> return ()
    Nothing -> permissionDenied msg
 where
  grantsRight (HasRole r') = r == r'
  grantsRight (HasEpisodeGrant epId' r') = (epId == epId') && (r == r')
  --grantsRight _ = False

requireManageUsers :: [Permission] -> GHandler sub master ()
requireManageUsers = requireRole AsManager "You don't have permission to manage users"

requireCreateEpisode :: [Permission] -> GHandler sub master ()
requireCreateEpisode = requireRole AsEditor "You don't have permission to create episodes"

requireEditEpisode :: [Permission] -> EpisodeId -> GHandler sub master ()
requireEditEpisode = requireEpisodePermission AsEditor "You don't have permission to edit this episode"

requireManageEpisode :: [Permission] -> EpisodeId -> GHandler sub master ()
requireManageEpisode = requireEpisodePermission AsManager "You don't have permission to manage this episode"

requirePublishEpisode :: [Permission] -> EpisodeId -> GHandler sub master ()
requirePublishEpisode = requireEpisodePermission AsPublisher "You don't have permission to publish this episode"

requireSubmitEpisode :: (YesodPersist master, YesodPersistBackend master ~ SqlPersist) =>
                                          [Permission] -> EpisodeId -> GHandler sub master ()
requireSubmitEpisode perms epId = do
  requireEpisodePermission AsEditor "You don't have permission to edit this episode" perms epId
  episode <- runDB $ get404 epId
  case episodePublished episode of
    StateDraft -> return ()
    _ -> do
      let msg = "Can't submit " ++ (show $ epId) ++ ". Episode is already submitted."
      permissionDenied $ pack msg

canGrant :: Privilege -> [Permission] -> Bool
canGrant role perms =
  case role of
    AsAdmin -> AsAdmin `elem` roles
    AsManager -> AsAdmin `elem` roles
    AsPublisher -> AsAdmin `elem` roles
    AsEditor -> [AsAdmin, AsManager] `anyElem` roles
 where
  roles = map rolePermission $ filter permIsRole perms
  xs `anyElem` ys = getAny . mconcat $ map (Any . (`elem` ys)) xs

requireGrant :: Privilege -> [Permission] -> GHandler sub master ()
requireGrant role perms = do
  if canGrant role perms
    then return ()
    else permissionDenied "You can't grant that permission"

canGrantOnEp :: Privilege -> EpisodeId -> [Permission] -> Bool
canGrantOnEp role epId perms =
  if canGrant role perms
    then True
    else case role of
      AsAdmin -> AsAdmin `elem` grants
      AsManager -> (AsAdmin `elem` grants) || (AsManager `elem` roles)
      AsPublisher -> AsAdmin `elem` grants
      AsEditor -> [AsAdmin, AsManager] `anyElem` grants
 where
  roles = map rolePermission $ filter permIsRole perms
  grants =
    let epGrants = filter permIsEpGrant perms
        matchingGrants = filter ((epId ==) <$> epGrantEpisodeId) $ epGrants
    in map epGrantPermission matchingGrants
  xs `anyElem` ys = getAny . mconcat $ map (Any . (`elem` ys)) xs

requireGrantOnEp :: Privilege -> EpisodeId -> [Permission] -> GHandler sub master ()
requireGrantOnEp role epId perms =
  if canGrantOnEp role epId perms
    then return ()
    else permissionDenied "You can't grant that permission"

--requireCreateEpisode perms = do
--  if AsEditor `elem` (map rolePermission roles)
--    then return ()
--    else deny
-- where
--  roles = filter permIsRole perms
--  deny = permissionDenied "You don't have permission to create episodes."

requirePermissions :: (YesodPersist master, YesodPersistBackend master ~ SqlPersist) =>
                                     Key SqlPersist (UserGeneric SqlPersist)
                                     -> GHandler sub master [Permission]
requirePermissions userId = do
  perms <- runDB $ getPermissions userId
  if length perms == 0
    then permissionDenied "Insufficient permissions"
    else return perms

-- TODO - simplify type if possible, can we consolidate type constraints?
getPermissions :: forall (m :: * -> *).
                   (MonadIO m, MonadUnsafeIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m) =>
                   Key SqlPersist (UserGeneric SqlPersist) -> SqlPersist m [Permission]
getPermissions userId = do
  roles <-  mapRoles $ selectList [RoleUserId ==. userId] []
  epGrants <- mapEpGrants $ selectList [EpisodeGrantUserId ==. userId] []
  return $ mconcat [roles, epGrants]
 where
  mapRoles = liftM $ map $ \(Entity _ x) -> HasRole . rolePrivilege $ x
  mapEpGrants =
    let mkGrant (Entity _ x) = HasEpisodeGrant <$> episodeGrantEpisodeId <*> episodeGrantPrivilege $ x
    in liftM (map mkGrant)

-- TODO - simplify type if possible
guardUpdateEpisode :: (YesodPersist master, YesodAuth master, AuthId master ~ Key SqlPersist (UserGeneric SqlPersist), YesodPersistBackend master ~ SqlPersist) =>
                       ([Permission] -> Key SqlPersist (EpisodeGeneric SqlPersist) -> GHandler sub master a)
                       -> Key SqlPersist (EpisodeGeneric SqlPersist)
                       -> [Update (EpisodeGeneric SqlPersist)]
                       -> (Entity (EpisodeGeneric SqlPersist) -> YesodDB sub master b)
                       -> GHandler sub master b
guardUpdateEpisode ensure entityId updates toDoc = do
  doc <- guardUpdateEntity ensure entityId updates toDoc
  runDB $ touchEpisode entityId
  return doc

-- TODO - simplify type if possible
guardUpdateEntity :: (PersistEntity entity, YesodPersist master, YesodAuth master, AuthId master ~ Key SqlPersist (UserGeneric SqlPersist),
                                     YesodPersistBackend master ~ SqlPersist, PersistEntityBackend entity ~ SqlPersist) =>
                                    ([Permission] -> Key SqlPersist entity -> GHandler sub master a)
                                    -> Key SqlPersist entity
                                    -> [Update entity]
                                    -> (Entity entity -> YesodDB sub master b)
                                    -> GHandler sub master b
guardUpdateEntity ensure entityId updates toDoc = do
  (Entity userId _) <- requireAuth
  rights <- requirePermissions userId
  _ <- ensure rights entityId
  runDB $ update entityId updates
  entity <- runDB $ get404 entityId
  doc <- runDB $ toDoc (Entity entityId entity)
  return doc