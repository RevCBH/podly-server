{-# LANGUAGE DeriveGeneric #-}

--import Import

import Import
import Text.CSV
import Data.Char (toLower)
import Data.Maybe (fromJust, isJust)
import Data.List.Split (splitOn)
import Data.Text (Text, pack, unpack)
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
import Data.Aeson.TH (deriveJSON)
import System.Environment (getArgs)

import Document

-- DEBUG
import Debug.Trace


parseTime :: Text -> Int
parseTime str =
  let hh:mm:ss:[] = map read $ splitOn ":" (unpack str) :: [Int]
  in ss + (60 * mm) + (60 * 60 * hh)

parseDate :: Text -> UTCTime
parseDate = readTime defaultTimeLocale "%F" . unpack

parsePodcast header =
  let cols = makeColumns header ["podcast", "episode title", "number", "air date", "search slug", "duration"]
      hPodcast:hEpisodeTitle:hNumber:hAirDate:hSearchSlug:hDuration:[] = cols
  in  DocEpisode              <$>
      hPodcast                <*>
      read.unpack.hNumber     <*>
      Main.parseDate.hAirDate <*>
      hEpisodeTitle           <*>
      hSearchSlug             <*>
      Main.parseTime.hDuration

parseNode rowHeader =
  let cols = makeColumns rowHeader ["title", "node type", "time", "url"]
      cTitle:cNodeType:cTime:cUrl:[] = cols
  in  (DocNode Nothing Nothing) <$>
      cTitle                    <*>
      cUrl                      <*>
      mkLinkTitle.cUrl          <*>
      Main.parseTime.cTime      <*>
      (DocNodeT Nothing "").cNodeType

parseMedia header =
  let cols = makeColumns header ["kind", "url", "offset"]
      hKind:hUrl:hOffset:[] = cols
  in  DocMediaSource      <$>
      read.unpack.hKind   <*>
      hUrl                <*>
      read.unpack.hOffset

main :: IO ()
main = do
  fromSrc:toFile:[] <- getArgs
  let nodesFile = fromSrc ++ ".Nodes.csv"
  let mediaFile = fromSrc ++ ".Media.csv"
  csv <- parseCSVFromFile nodesFile
  let header:hData:rowHeader:rows = case csv of
              Left er -> error $ show er
              Right rows -> rows

  csv2 <- parseCSVFromFile mediaFile
  let mediaHeader:mediaRows = case csv2 of
              Left er -> error $ show er
              Right rows -> rows

  --let podcast = parsePodcast header hData
  --let podcast = parsePodcast hData

  --let rows' = filter (\x -> (length . cTime) x > 0) rows
  let mediaSources = map (parseMedia mediaHeader) mediaRows
  let podcast = parsePodcast header hData mediaSources $ map (parseNode rowHeader) rows
  let json = [podcast]

  BL.writeFile toFile $ encode json

  return ()
 --where
makeColumns :: Record -> [String] -> [Record -> Text]
makeColumns header titles =
  let columnNames = map (map toLower) header
      indicies = map fromJust $ filter isJust $ flip map titles $ flip L.elemIndex columnNames
  in  trace ("columnNames: " ++ (show columnNames) ++ "\ntitles: " ++ (show titles) ++ "\nindicies: " ++ (show indicies)) $
      map (\n -> pack . (!! n)) indicies

mkLinkTitle :: Text -> Text
mkLinkTitle str =
  let mAuth = do uri <- parseURI $ unpack str
                 auth <- uriAuthority uri
                 return $ uriRegName auth
  in case mAuth of
    Just x -> pack x
    Nothing -> str
