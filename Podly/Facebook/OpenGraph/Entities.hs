module Podly.Facebook.OpenGraph.Entities (OpenGraphEntity, elements, tags) where

import Import
import qualified Data.Text as T

import Podly.Facebook.OpenGraph
import Podly.Urls

import Document

class OpenGraphEntity a where
  elements :: a -> [FBOpenGraphElement]
  tags :: a -> Html
  tags x = renderTags $ elements x

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
         Video (canonicalUrl video) 360 640 "application/x-shockwave-flash"]
      _ -> []