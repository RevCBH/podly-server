{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import Yesod.Angular
import qualified Data.ByteString.Lazy.Char8 as L8

import Document

handleHomeR :: Handler RepHtml
handleHomeR = do
    episode:_ <- runDB $ selectList [EpisodeNumber ==. 275] [Desc EpisodeAirDate]
    doc <- runDB $ documentFromEpisode episode
    let epJson = L8.unpack $ encode doc
    runNgModule (Just "playerMod") $ do
        let angularMessage = "Angular" :: String
        --cmdGetPeople <- addCommand $ \() -> do
        --    people' <- getYesod >>= liftIO . readIORef . ipeople
        --    return $ map (\(pid, Person name _) -> PersonSummary pid name) $ Map.toList people'
        $(addCtrl "/episodes" "episodeList")
        $(addCtrl "/player/:podcastName/:episodeNumber" "player")

        setDefaultRoute "/player/The Joe Rogan Experience/275"
