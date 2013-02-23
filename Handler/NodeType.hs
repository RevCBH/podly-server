{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
module Handler.NodeType where

import Import

--import Handler.Util

import Document
--import Data.Aeson (Result(..), ToJSON(..))
--import Data.List (nub)
--import Data.Maybe
--import Data.Time(addUTCTime, getCurrentTime, Day, UTCTime(..), secondsToDiffTime)
--import Data.Text (pack)
--import qualified Data.HashMap.Strict as Map
--import Network.Wai (requestHeaders)

--import Fixtures
--import Document

--import Yesod.Form.Jquery

getNodeTypesR :: Handler RepHtmlJson
getNodeTypesR = do
  nodeTypes <- runDB $ selectList [] [Asc NodeTypeTitle]
  let widget = do
      setTitle "All Node Types"
      $(widgetFile "nodeTypes/index")
  defaultLayoutJson widget (map documentFromNodeType nodeTypes)

getNodeTypeR :: NodeTypeId -> Handler RepHtml
getNodeTypeR tid = do
  nodeType <- runDB $ get404 tid
  --nodes <- runDB $ selectList [NodeNodeTypeId ==. tid] [Asc NodeTitle]
  defaultLayout $ do
    setTitle $ toHtml $ "Node Type - " <> (nodeTypeTitle nodeType)
    $(widgetFile "nodeTypes/show")

supportedIcons :: [(Text, Text)]
supportedIcons = map (\x -> (x,x)) [
  "Media/Books", "Media/Documentary", "Media/Film", "Media/Music", "Media/Podcast", "Media/TV",
  "Media/Web_Networks", "Media/Youtube",

  "Mind/Ayuhausca", "Mind/DMT", "Mind/Marijuana", "Mind/Mind_Other", "Mind/Mushrooms",

  "People/Actor_PPL", "People/Actor2_PPL", "People/Athlete_PPL", "People/Author_PPL", "People/Chef_PPL",
  "People/Comedian_PPL", "People/Comedian2_PPL", "People/Fighter_PPL", "People/FilmMaker_PPL", "People/Guest",
  "People/Inventor_PPL", "People/MartialArts_PPL", "People/Musician_PPL", "People/Politician_PPL",
  "People/Scientist_PPL", "People/Tycoon_PPL",

  "Products/Health", "Products/Health2", "Products/Interest!", "Products/Locations", "Products/Money",
  "Products/Sponsors", "Products/Tech"]

{-
newNodeTypeForm :: Form NodeType
newNodeTypeForm = renderDivs $ NodeType
    <$> areq textField "Title" Nothing
    -- <*> areq textField "Icon" Nothing
    <*> areq (selectFieldList supportedIcons) "Icon" Nothing

getNewNodeTypeR :: Handler RepHtml
getNewNodeTypeR = do
    (formWidget, enctype) <- generateFormPost newNodeTypeForm
    defaultLayout $ do
        setTitle "New Node Type"
        $(widgetFile "nodeTypes/new")

postNodeTypesR :: Handler RepHtml
postNodeTypesR = do
  (source, nodeType) <- fromJsonOrFormPost newNodeTypeForm
  entity@(Entity tid _) <- ensureNodeType nodeType
  --case source of
  --  PostJson -> jsonToRepJson entity
  --  PostForm -> redirect $ NodeTypeR tid
  redirect $ NodeTypeR tid
 where
  ensureNodeType nt = ensureEntity nt $ UniqueTypeTitle $ nodeTypeTitle nt
-}