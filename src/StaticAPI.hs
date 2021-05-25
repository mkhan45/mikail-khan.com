{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module StaticAPI where

import Servant
import Servant.HTML.Blaze

type StaticAPI = "Assets" :> Raw
            :<|> "CSS"    :> Raw
            :<|> "img"    :> Raw
            :<|> "js"     :> Raw

staticServer :: Server StaticAPI
staticServer = serveDirectoryWebApp "static/Assets"
          :<|> serveDirectoryWebApp "static/CSS"
          :<|> serveDirectoryWebApp "static/img"
          :<|> serveDirectoryWebApp "static/js"
