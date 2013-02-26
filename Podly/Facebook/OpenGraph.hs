module Podly.Facebook.OpenGraph (FBOpenGraphElement(..), renderTags) where

--import Import hiding (concatMap)
import Prelude hiding (concatMap)
import Data.Text (Text, pack, concatMap, singleton)
import Data.Monoid (mappend, mconcat)
import Text.Blaze ((!))
import Text.Blaze.Internal (customAttribute, AttributeValue, textValue)
import Text.Blaze.Html5 (meta, Html)
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
mkTag name value =
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