{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import Views.Portfolio.PortfolioView
import Views.Memes.MemeData
import Views.Memes.MemeEdit
import Views.Memes.MemeView

type API =                                          Get '[HTML] H.Html
     :<|> "portfolio"                            :> Get '[HTML] H.Html
     :<|> "resume"                               :> Get '[HTML] H.Html
     :<|> "reload_cache"                         :> Get '[PlainText] String
     :<|> "memes"                                :> Get '[HTML] H.Html
     :<|> "memes" :> Capture "pagenum" Int       :> Get '[HTML] H.Html
     :<|> "memes" :> "edit"                      :> Get '[HTML] H.Html
     :<|> "memes" :> "meme_submit" :> MemeSubReq :> Post '[HTML] H.Html
     :<|> "Assets"                               :> Raw
     :<|> "CSS"                                  :> Raw
     :<|> "img"                                  :> Raw

type MemeSubReq = ReqBody '[FormUrlEncoded] MemeSubmitInfo

app :: Application
app = serve api $ server

api :: Proxy API
api = Proxy

readHTMLFile :: String -> IO H.Html
readHTMLFile f = do
    contents <- SIO.readFile f
    return $ preEscapedToHtml contents

serveHTML :: String -> Handler H.Html
serveHTML f = do
    h <- liftIO $ readHTMLFile f
    return h

server :: Server API
server = return index
    :<|> serveHTML "generated/portfolio.html"
    :<|> serveHTML "generated/resume.html"
    :<|> do 
            liftIO reloadResumeCache
            liftIO reloadPortfolioCache
            return "success"
    :<|> throwError err301 { errHeaders = [("Location", "/memes/0")] }
    :<|> memeEndpoint
    :<|> do
            memes <- liftIO $ readMemeFile 0
            return $ memeEditHTML memes
    :<|> memeSubmitHandler
    :<|> serveDirectoryWebApp "static/Assets"
    :<|> serveDirectoryWebApp "static/CSS"
    :<|> serveDirectoryWebApp "static/img"

memeEndpoint :: Int -> Handler H.Html
memeEndpoint pagenum = do
    memes <- liftIO $ readMemeFile pagenum
    return $ memeHTML pagenum memes

memeSubmitHandler :: MemeSubmitInfo -> Handler H.Html
memeSubmitHandler inf = do
    passHash <- liftIO $ getEnv "PASSHASH"
    if checkPass passHash (submitPass inf) then do
        liftIO $ addMeme (submitTy inf) (submitTitle inf) (submitURL inf)
        throwError err303 { errHeaders = [("Location", "/memes/edit")] }
    else
        throwError err303 { errHeaders = [("Location", "/memes/edit")] }
