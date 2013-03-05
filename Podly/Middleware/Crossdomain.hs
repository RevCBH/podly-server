module Podly.Middleware.Crossdomain (crossdomainXml) where

import Import
import Network.Wai (Middleware, pathInfo, Response(ResponseFile))
import Network.HTTP.Types (status200)

crossdomainXml :: Middleware
crossdomainXml app req =
  case pathInfo req of
    ["crossdomain.xml"] ->
      return $ ResponseFile status200 [("Content-Type", "text/x-cross-domain-policy")] "./static/crossdomain.xml" Nothing
    _ -> app req