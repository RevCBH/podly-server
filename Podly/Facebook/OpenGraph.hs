module Podly.Facebook.OpenGraph where

import Import hiding (concatMap)
import Data.Text (pack, concatMap, singleton)
--import qualified Data.Text as T
import Text.Blaze ((!))
import Text.Blaze.Internal (customAttribute, AttributeValue, textValue)
import Text.Blaze.Html5 (meta)
import Text.Blaze.Html5.Attributes (content)

data FBOpenGraphElement =
  Title Text
  | Description Text
  | Type Text -- 'video', etc.
  | Url Text
  | Image Text
  | SiteName Text
  | Video {videoUrl :: Text, videoWidth :: Int, videoHeight :: Int, videoType :: Text}

mkTag :: AttributeValue -> Text -> Html
mkTag name value =  -- mconcat ["<meta property=\"og:", name, "\" content=\"", content, "\" />"]
  meta ! customAttribute "property" (mappend "og:" name) ! (content $ textValue value)

escapeUrlChar :: Char -> Text
escapeUrlChar c =
  case c of
    ' ' -> "%20"
    x -> singleton x

render :: FBOpenGraphElement -> Html
render (Title x) = mkTag "title" x
render (Description x) = mkTag "description" x
render (Type x) = mkTag "type" x
render (Url x) = mkTag "url" (concatMap escapeUrlChar x)
render (Image x) = mkTag "image" x
render (SiteName x) = mkTag "site_name" x
render (Video url h w t) =
  mconcat [mkTag "video" url,
            mkTag "video:height" $ pack $ show h,
            mkTag "video:width" $ pack $ show w,
            mkTag "video:type" t]

renderTags :: [FBOpenGraphElement] -> Html
renderTags = mconcat . map render

-- <meta property="og:type" content="video" />
-- <meta property="og:url" content="http://ahrengot.com/playground/circular-scrubbing"/>
-- <meta property="og:image" content="http://ahrengot.com/playground/circular-scrubbing/thumb.jpg"/>
-- <meta property="og:site_name" content="Ahrengot's Playground"/>
-- <meta property="fb:admins" content="100000936142315"/>
--
-- <!-- Video player specific OG tags -->
-- <meta property="og:video" content="http://ahrengot.com/playground/circular-scrubbing/video-player.swf?url=http://ahrengot.com/playground/circular-scrubbing/assets/video/example.mp4" />
-- <meta property="og:video:width" content="374" />
-- <meta property="og:video:height" content="202" />
-- <meta property="og:video:type" content="application/x-shockwave-flash" />