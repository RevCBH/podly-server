{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Test where

import Import
-- required when deploying for some reason
--import Data.Aeson (toJSON)
--import Data.Aeson (Result(..))
--import Data.Text (pack)
import qualified Data.HashMap.Strict as Map
--import Control.Monad (filterM, liftM, liftM2)
--import Data.List (head)
--import Podly.Auth

--import System.Directory hiding (getPermissions)

--getUsersR :: Handler RepJson
--getUsersR = do
--    (Entity userId user) <- requireAuth
--    rights <- requirePermissions userId
--    requireManageUsers rights
--    users <- runDB $ selectList [] [Asc UserIdent]
--    json <- runDB $ mapM userWithPerms users

--    jsonToRepJson json
--  where
--    userWithPerms entity@(Entity uid _) = do
--        perms <- getPermissions uid
--        let (Object uJson) = toJSON entity
--        return $ object $ mconcat [Map.toList uJson, ["permissions" .= perms]]

--iconNames :: IO [Text]
--iconNames = do
--    xs <- getDirectoryContents =<< imgRoot
--    iconFolders <- filterM isImageDirectory $ filter isNotDotFile xs
--    icons <- mapM iconsInFolder iconFolders
--    return $ map pack $ concat icons
--  where
--    imgRoot = liftM (++ "/static/img/") getCurrentDirectory
--    isNotDotFile = not . (== '.') . head
--    isImageDirectory = (>>= doesDirectoryExist) . (liftM2 (++) imgRoot . return)
--    iconsInFolder x = do
--        r <- imgRoot
--        files <- filterM (return . isNotDotFile) =<< getDirectoryContents (r ++ x)
--        let x' = (x ++ "/")
--        return $ map (x' ++ ) files

--getFirstRunR :: Handler RepHtml
--getFirstRunR = do
--    mRole <- liftM maybeHead $ runDB $ selectList [RolePrivilege ==. AsAdmin] []
--    case mRole of
--        Just _ -> notFound
--        Nothing -> do
--            runDB $ do
--                deleteWhere ([] :: [Filter Icon])
--                xs <- liftIO iconNames
--                mapM_ insert $ map Icon xs

--            (Entity userId user) <- requireAuth
--            runDB $ insert $ Role userId AsAdmin
--            runDB $ insert $ Role userId AsPublisher
--            runDB $ insert $ Role userId AsEditor

--            defaultLayout [whamlet|<h1>Granted #{userIdent user} Admin privileges</h1>|]
--  where
--    maybeHead (x:_) = Just x
--    maybeHead _ = Nothing

--getNukeR :: Handler RepJson
--getNukeR = do
--    runDB $ deleteWhere ([] :: [Filter Icon])
--    runDB $ deleteWhere ([] :: [Filter NodeInstance])
--    -- runDB $ deleteWhere ([] :: [Filter Node])
--    runDB $ deleteWhere ([] :: [Filter NodeType])
--    runDB $ deleteWhere ([] :: [Filter Episode])
--    runDB $ deleteWhere ([] :: [Filter Podcast])

--    runDB $ do
--        xs <- liftIO iconNames
--        mapM_ insert $ map Icon xs

--    --loadNodeTypes "node_types"
--    --runDB $ loadEpisodes "episodes"

--    jsonToRepJson ("OK" :: String)

countEpisodesFor :: Entity Podcast -> Handler Int
countEpisodesFor (Entity podcastId _) = do
    runDB $ count $ [EpisodePodcastId ==. podcastId]

getPodcastIndexR :: Handler RepHtmlJson
getPodcastIndexR = do
    entities <- runDB $ selectList [] [Asc PodcastName]
    let podcasts = (flip map) entities (\(Entity _ x) -> x)
    let widget = do
        setTitle $ toHtml ("Podcasts" :: String)
        $(widgetFile "podcasts/index")
    json <- (flip mapM) entities (\entity -> do
                    _count <- countEpisodesFor entity
                    let (Object o) = toJSON entity
                    return $ Map.insert "episodeCount" (toJSON _count) o)
    defaultLayoutJson widget json

getPodcastR :: Text -> Handler RepJson
getPodcastR name = do
    podcast <- runDB $ getBy404 $ UniquePodcastName name
    jsonToRepJson podcast

getPodcastEpisodeIndexR :: Text -> Handler RepHtmlJson
getPodcastEpisodeIndexR name = do
    (Entity podcastId _) <- runDB $ getBy404 $ UniquePodcastName name
    episodes <- runDB $ selectList [EpisodePodcastId ==. podcastId] [Asc EpisodeNumber]
    let widget = do
        setTitle $ toHtml $ name <> " - Episodes"
        $(widgetFile "podcasts/show")
    let json = episodes
    defaultLayoutJson widget json

{-
postNodeInstanceR :: Text -> Int -> Handler RepHtml
postNodeInstanceR podcast number = do
    (Entity episodeId _) <- runDB $ getBy404 $ UniqueEpisodeNumber podcast number
    nodes <- runDB $ selectList [] [Asc NodeTitle]
    (source, nodeInstance) <- fromJsonOrFormPost $ newNodeInstanceForm episodeId nodes
    runDB $ insert nodeInstance
    ----case source of
    ----    PostJson -> jsonToRepJson entity
    --    PostForm -> redirect $ PodcastEpisodeR (episodePodcast episode) (episodeNumber episode)
    redirect $ PodcastEpisodeR podcast number-}


