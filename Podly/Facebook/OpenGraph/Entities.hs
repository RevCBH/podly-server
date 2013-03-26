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
  mconcat ["https://podly.herokuapp.com/static/fb-player.swf?episodeId=" :: Text,
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

instance OpenGraphEntity (EpisodeDocument, Maybe Text) where
  elements (episode, time) =
    let updateUrls t x = case x of
                          Url url -> Url $ mconcat [url, T.pack "?t=", t]
                          v@(Video vUrl _ _ _) -> v {videoUrl = mconcat [vUrl, T.pack "&timeId=", t] }
                          _ -> x
    in case (elements episode, time) of
      (xs, Just t) -> map (updateUrls t) xs
      (xs, Nothing) -> xs