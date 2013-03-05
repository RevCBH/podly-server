{-# LANGUAGE OverloadedStrings #-}
module Handler.Node where

import Import
import qualified Podly.Metadata as Meta

--import Data.List (nub)

--import Handler.Util

getNodeMetaR :: NodeInstanceId -> Handler RepJson
getNodeMetaR relId = do
  let social = Meta.EpisodeSocial {
                Meta.epSocTwitter = "Thee Tweetr @thePodly",
                Meta.epSocEmail = Meta.EmailMessage {
                  Meta.emailSubject = "Awesome Podly!",
                  Meta.emailBody = "For serious: http://podly.co"}}
  jsonToRepJson $ object ["share" .= social]

--getNodesR :: Handler RepHtml
--getNodesR = do
--    nodes <- runDB $ selectList [] [Asc NodeTitle]
--    defaultLayout $ do
--        setTitle "All Nodes"
--        $(widgetFile "nodes/index")

--getNodeR :: NodeId -> Handler RepHtmlJson
--getNodeR nid = do
--    node <- runDB $ get404 nid
--    refs <- runDB $ selectList [NodeInstanceNodeId ==. nid] []
--    let episodeIds = nub $ (flip map) refs (\(Entity _ x) -> nodeInstanceEpisodeId x)
--    episodes <- runDB $ selectList [EpisodeId <-. episodeIds] [Asc EpisodePodcast, Desc EpisodeNumber]
--    let widget = do
--        setTitle $ toHtml $ "Node - " <> (nodeTitle node)
--        -- TODO - get default node type
--        $(widgetFile "nodes/show")
--    let json = Entity nid node
--    defaultLayoutJson widget json

--newNodeForm :: [Entity NodeType] -> Form Node
--newNodeForm nodeTypes = renderDivs $ Node
--    <$> areq textField "Title" Nothing
--    <*> areq textField "Link" Nothing
--    <*> areq textField "Link title" Nothing
--    -- TODO - default node type
--    <*> aopt (selectFieldList nodeTypes') "Default node type" Nothing
--  where
--    nodeTypes' = (flip map) nodeTypes (\(Entity tid x) -> (nodeTypeTitle x, tid))

{-
getNewNodeR :: Handler RepHtml -- RepHtmlJson
getNewNodeR = do
    nodeTypes <- runDB $ selectList [] [Asc NodeTypeTitle]
    (formWidget, enctype) <- generateFormPost $ newNodeForm nodeTypes
    defaultLayout $ do
        setTitle "New Node"
        $(widgetFile "nodes/new")
-}

--postNodesR :: Handler RepHtml
--postNodesR = do
--  nodeTypes <- runDB $ selectList [] [Asc NodeTypeTitle]
--  (source, node) <- fromJsonOrFormPost $ newNodeForm nodeTypes
--  entity@(Entity tid _) <- ensureNode node
--  --case source of
--  --  PostJson -> jsonToRepJson entity
--  --  PostForm -> redirect $ NodeTypeR tid
--  redirect $ NodeR tid
-- where
--  ensureNode n = ensureEntity n $ UniqueNodeTitle $ nodeTitle n