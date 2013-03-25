module Podly.Facebook.OpenGraph.Entities (OpenGraphEntity, elements, tags) where

import Import
import qualified Data.Text as T
import Data.Maybe (fromJust)

import Podly.Facebook.OpenGraph
import Podly.Urls

import Document

class OpenGraphEntity a where
  elements :: a -> [FBOpenGraphElement]
  tags :: a -> Html
  tags x = renderTags $ elements x

fbEmbedUrlForEpisodeDocument :: EpisodeDocument -> Text
fbEmbedUrlForEpisodeDocument episode =
  mconcat ["http://podly.co/static/fb-player.swf?episodeId=" :: Text,
           T.pack . fromRight . fromPersistValue . unKey . fromJust $ docEpisode_id episode]
 where
  fromRight (Right x) = x

instance OpenGraphEntity EpisodeDocument where
  elements episode =
    let ogTitle = mconcat [docEpisodePodcast episode, " #", T.pack . show $ docEpisodeNumber episode]
        media = filter (\x -> docSourceKind x == VideoYouTube) $ docEpisodeMediaSources episode
    in case media of
      (video : _) ->
        [Title ogTitle,
         Description $ docEpisodeTitle episode,
         Type "video",
         Url $ canonicalUrl episode,
         Image $ canonicalUrl $ ThumbnailImage video,
         SiteName "Podly.co",
         --Video (canonicalUrl video) 360 640 "application/x-shockwave-flash"]
         Video (fbEmbedUrlForEpisodeDocument episode) 398 438 "application/x-shockwave-flash"]
      _ -> []

-- WARNING, HACK - this depends on the ordering of OG tags in the above instance (OpenGraphEntity EpisodeDocument)
instance (Show a, Ord a, Num a) => OpenGraphEntity (EpisodeDocument, a) where
  elements (episode, time) =
    if time > 0
      then
        case (reverse $ elements episode) of
          (videoTag : xs) ->
            let newUrl = mconcat [videoUrl videoTag, T.pack "&timeId=", T.pack $ show time]
            in reverse $ (videoTag {videoUrl = newUrl}) : xs
          _ -> []
      else elements episode