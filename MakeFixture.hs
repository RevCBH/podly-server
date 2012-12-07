{-# LANGUAGE OverloadedStrings #-}
import Text.CSV
import Data.Char (toLower)
import Data.Maybe (fromJust, isJust)
import Data.List.Split (splitOn)
import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock (UTCTime)
import Data.Time.Format (readTime)
import System.Locale (defaultTimeLocale)
import Network.URI (parseURI, uriAuthority, uriRegName)
import Data.Aeson (ToJSON(..), encode, (.=), Value(Null), object)
import System.Environment (getArgs)

-- DEBUG
import Debug.Trace (trace)

type TimeOffset = Int

data NodeRow = NodeRow {
  title :: String,
  nodeType :: String,
  time :: TimeOffset,
  --time :: String,
  url :: String,
  linkTitle :: String
} deriving (Show)

data Episode = Episode {
  epPodcast :: String,
  epTitle :: String,
  epNumber :: Int,
  epAirDate :: UTCTime,
  epSearchSlug :: String,
  epDuration :: TimeOffset,
  epStreamingUrl :: String,
  epNodes :: [NodeRow]
} deriving (Show)

parseTime :: String -> TimeOffset
parseTime str =
  let hh:mm:ss:[] = map read $ splitOn ":" str :: [Int]
  in ss + (60 * mm) + (60 * 60 * hh)

parseDate :: String -> UTCTime
parseDate = readTime defaultTimeLocale "%F"

instance ToJSON NodeRow where
  toJSON (NodeRow title nodeType time url linkTitle) = object [
    "_id" .= Null,
    "relId" .= Null,
    "title" .= title,
    "time" .= time,
    "url" .= url,
    "linkTitle" .= linkTitle,
    "nodeType" .= object [
      "_id" .= Null,
      "title" .= nodeType,
      "icon" .= ("" :: String)]]

instance ToJSON Episode where
  toJSON (Episode podcast title number airDate searchSlug duration streamingUrl nodes) = object [
    "podcast" .= podcast,
    "title" .= title,
    "number" .= number,
    "airDate" .= (toJSON airDate),
    "searchSlug" .= searchSlug,
    "duration" .= duration,
    "streamingUrl" .= streamingUrl,
    "nodes" .= nodes]

main = do
  fromFile:toFile:[] <- getArgs
  csv <- parseCSVFromFile fromFile
  let header:hData:rowHeader:rows = case csv of
              Left er -> error $ show er
              Right rows -> rows

  let hPodcast:hEpisodeTitle:hNumber:hAirDate:hSearchSlug:hDuration:hStreamingUrl:[] = makeColumns header ["podcast", "episode title", "number", "air date", "search slug", "duration", "streaming url"]
  let parsePodcast = Episode <$> hPodcast <*> hEpisodeTitle <*> read.hNumber <*> parseDate.hAirDate <*> hSearchSlug <*> parseTime.hDuration <*> hStreamingUrl
  let podcast = parsePodcast hData

  let cTitle:cNodeType:cTime:cUrl:[] = makeColumns rowHeader ["title", "node type", "time", "url"]
  let parseNode = NodeRow <$> cTitle <*> cNodeType <*> parseTime.cTime <*> cUrl <*> mkLinkTitle.cUrl

  let rows' = filter (\x -> (length . cTime) x > 0) rows
  let podcast = parsePodcast hData $ map parseNode rows'
  let json = [podcast]

  BL.writeFile toFile $ encode json

  return ()
 --where
makeColumns :: Record -> [String] -> [Record -> Field]
makeColumns header titles =
  let columnNames = map (map toLower) header
      indicies = map fromJust $ filter isJust $ flip map titles $ flip L.elemIndex columnNames
  in  trace ("columnNames: " ++ (show columnNames) ++ "\ntitles: " ++ (show titles) ++ "\nindicies: " ++ (show indicies)) $
      map (\n -> (!! n)) indicies

mkLinkTitle str =
  let mAuth = do uri <- parseURI str
                 auth <- uriAuthority uri
                 return $ uriRegName auth
  in case mAuth of
    Just x -> x
    Nothing -> str
