module Podly.Affiliate where -- (rewriteLink) where

import Import
import Control.Monad (liftM)
import Data.Maybe
import Data.Text (pack, unpack)
import Text.Regex.Posix
import Network.URI

import Debug.Trace

rewriteLink :: String -> Handler String
rewriteLink lnk = do
  liftIO $ traceIO $ "rewriteLink: " ++ lnk
  let parsers = [liftM makeAmazonLink <$> liftM unpack . extraAmazonAffiliate]
  extra <- getExtra
  let ps = mapMaybe ($ extra) parsers
  case firstMaybe $ map ($ lnk) ps of
    Just x -> return $ trace ("\tto: " ++ x) x
    Nothing -> return lnk

hostname :: String -> Maybe String
hostname url = do
  uri <- parseURI url
  auth <- uriAuthority uri
  return $ uriRegName auth

urlPath :: String -> Maybe String
urlPath url = do
  uri <- parseURI url
  return $ uriPath uri

firstMaybe :: [Maybe a] -> Maybe a
firstMaybe (Just x:xs) = Just x
firstMaybe (Nothing:xs) = firstMaybe xs
firstMaybe _ = Nothing

maybeMatches :: String -> String -> Maybe String
maybeMatches pat str =
  if str =~ pat
    then Just str
    else Nothing

maybeEquals :: (Eq a) => a -> a -> Maybe a
maybeEquals a b =
  if a == b then Just b else Nothing

tryMatch idx p pattern =
  let (_, _, _, xs) = p =~ pattern :: (String, String, String, [String])
  in if idx < length xs
    then Just $ xs !! idx
    else Nothing

firstMatchAt :: Int -> String -> [String] -> Maybe String
firstMatchAt idx p patterns = firstMaybe $ map (tryMatch idx p) patterns

amazonShortProduct :: String -> Maybe AmazonLink
amazonShortProduct url = do
  hostname url >>= maybeMatches "amzn.com"
  p <- urlPath url
  m <- firstMatchAt 0 p ["^/([^/?]+)"]
  return $ AmazonProduct m

amazonProduct :: String -> Maybe AmazonLink
amazonProduct url = do
  hostname url >>= maybeMatches "amazon.com"
  p <- urlPath url
  m <- firstMatchAt 0 p ["/dp/([^/?]+)", "/gp/product/([^/?]+)"]
  return $ AmazonProduct m

amazonEntity :: String -> Maybe AmazonLink
amazonEntity url = do
  hostname url >>= maybeMatches "^(www.)?(amazon.com)|(amzn.com)"
  p <- urlPath url
  m <- firstMatchAt 1 p ["/([^/?]+)/e/([^/?]+)", "/(l)/([^/?]+)"]
  return $ AmazonEntity m

-- TODO - amazon search pages

data AmazonLink =
  AmazonProduct String
  | AmazonEntity String
 deriving (Show)

makeAmazonLink :: String -> String -> Maybe String
makeAmazonLink tag url = do
  x <- firstMaybe [amazonProduct url, amazonShortProduct url, amazonEntity url]
  case x of
    AmazonProduct p -> return $ "http://www.amazon.com/dp/" ++ p ++ "/?tag=" ++ tag
    AmazonEntity e -> return $ "http://www.amazon.com/l/" ++ e ++ "/?tag=" ++ tag
