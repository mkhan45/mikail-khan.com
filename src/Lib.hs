{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( app ) where

import Data.Aeson
import Data.Aeson.TH
import Servant
import Servant.HTML.Blaze

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import qualified System.IO.Strict as SIO

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html (preEscapedToHtml)

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import System.Environment

import Views.Index
import Views.Resume.ResumeView
import Views.Resume.ResumePrintView
import Views.Portfolio.PortfolioView

import Views.Memes.MemeAPI
import StaticAPI

type API =                  Get '[HTML] H.Html
     :<|> "portfolio"    :> Get '[HTML] H.Html
     :<|> "resume"       :> Get '[HTML] H.Html
     :<|> "resume_print" :> Get '[HTML] H.Html
     :<|> "reload_cache" :> Get '[PlainText] String
     :<|> MemeAPI
     :<|> StaticAPI

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

readHTMLFile :: String -> IO H.Html
readHTMLFile f = do
    contents <- SIO.readFile f
    return $ preEscapedToHtml contents

serveHTML :: String -> Handler H.Html
serveHTML = liftIO . readHTMLFile

server :: Server API
server = return index
    :<|> serveHTML "generated/portfolio.html"
    :<|> serveHTML "generated/resume.html"
    :<|> serveHTML "generated/resume_print.html"
    :<|> do 
            liftIO reloadResumePrintCache
            liftIO reloadResumeCache
            liftIO reloadPortfolioCache
            return "success"
    :<|> memeServer
    :<|> staticServer
