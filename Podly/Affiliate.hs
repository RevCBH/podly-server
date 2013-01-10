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
urlPath url = parseURI url >>= return . uriPath

urlQuery :: String -> Maybe String
urlQuery url = parseURI url >>= return . uriQuery

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

amazonOther :: String -> Maybe AmazonLink
amazonOther url = do
  h <- hostname url >>= maybeMatches "^(www.)?(amazon.com)|(amzn.com)"
  p <- urlPath url
  q <- urlQuery url
  let pattern = "tag=([^&?=]*)[&?=]?" :: String
  let (pBefore, _, pAfter) = p =~ pattern :: (String, String, String)
  let (qBefore, _, qAfter) = q =~ pattern :: (String, String, String)
  let q' = qBefore ++ qAfter
  let post = if length q' == 0
                then "?"
                else "&"

  return $ AmazonGeneric $ h ++ pBefore ++ pAfter ++ q' ++ post

-- TODO - amazon search pages
-- TODO - http://www.amazon.com/gp/feature.html?tag=fake&ie=UTF8&docId=1000664761
-- TODO - http://www.amazon.com/b/ref=sr_tc_sc_2_0?node=133141011&pf_rd_r=2052D86DEB1345DBBB48&pf_rd_m=ATVPDKIKX0DER&pf_rd_t=301&pf_rd_i=kindle&pf_rd_p=1396097482&pf_rd_s=structured-results-2&qid=1354306284&sr=8-2-tc

data AmazonLink =
  AmazonProduct String
  | AmazonEntity String
  | AmazonGeneric String
 deriving (Show)

makeAmazonLink :: String -> String -> Maybe String
makeAmazonLink tag url = do
  x <- firstMaybe [amazonProduct url, amazonShortProduct url, amazonEntity url, amazonOther url]
  case x of
    AmazonProduct p -> return $ "http://amzn.com/" ++ p ++ "?tag=" ++ tag
    AmazonEntity e -> return $ "http://amzn.com/l/" ++ e ++ "?tag=" ++ tag
    AmazonGeneric url -> return $ "http://" ++ url ++ "tag=" ++ tag
