module Podly.Urls where

import Import
--import Data.String (IsString)
import qualified Data.Text as T

import Document

class CanonicalUrl a where
  canonicalUrl ::  a -> Text

instance CanonicalUrl Episode where
  canonicalUrl episode =
    -- TODO make _approot non-static
    let _approot = "http://podly.co/"
    in mconcat [T.pack _approot, "podcasts/", episodePodcast episode, "/episodes/", T.pack . show $ episodeNumber episode]

instance CanonicalUrl EpisodeDocument where
  canonicalUrl episode =
      -- TODO make _approot non-static
    let _approot = "http://podly.co/"
    in mconcat [T.pack _approot, "podcasts/", docEpisodePodcast episode, "/episodes/", T.pack . show $ docEpisodeNumber episode]

newtype ThumbnailImage a = ThumbnailImage a

instance CanonicalUrl (ThumbnailImage MediaSourceDocument) where
  canonicalUrl (ThumbnailImage source) =
    -- TODO - non-YouTube sources
    mconcat ["http://img.youtube.com/vi/", docSourceResource source, "/0.jpg"]

instance CanonicalUrl MediaSourceDocument where
  canonicalUrl source =
    -- TODO - non-YouTube sources
    mconcat ["http://www.youtube.com/v/", docSourceResource source, "?version=3&autohide=1"]