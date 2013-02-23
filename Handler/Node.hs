{-# LANGUAGE OverloadedStrings #-}
module Handler.Node where

--import Import

--import Data.List (nub)

--import Handler.Util

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