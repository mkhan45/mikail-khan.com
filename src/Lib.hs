{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    ) where

import           Web.Scotty
import           Network.Wai (Application)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Handler.Warp (defaultSettings, setPort)
import           Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           Control.Monad.IO.Class (liftIO)

import qualified System.IO.Strict as SIO

import           Views.Index
import           Views.Resume.ResumeView
import           Views.Memes.MemeData
import           Views.Memes.MemeView
import           Views.Memes.MemeEdit

import 		 System.Environment

updateVisitCount :: Int -> IO ()
updateVisitCount visitCount = do
    writeFile "visitCount.txt" (show $ visitCount + 1)
    return ()

getVisitCount :: IO Int
getVisitCount = do
    contents <- SIO.readFile "visitCount.txt"
    let visitCount = read contents :: Int
    return visitCount

server :: IO Application
server = scottyApp $ do
    let config = setPort 443 defaultSettings

    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    --get "/" $ do
    --    visitCount <- liftIO getVisitCount
    --    html $ renderHtml $ index (visitCount + 1)
    --    liftIO $ updateVisitCount visitCount
    get "/" $ do
        html $ renderHtml resume
    get "/memes/edit" $ do
        memes <- liftIO $ readMemeFile 0
        html $ renderHtml $ memeEditHTML memes
    post "/memes/meme_submit" $ do
        ty <- param "type"
        title <- param "title"
        url <- param "url"
        passwd <- param "password"
	passHash <- liftIO $ getEnv "PASSHASH"
        if (checkPass passwd passHash) then do
            liftIO $ addMeme (T.pack ty) title url
            redirect "/memes/edit"
        else
            redirect "/memes/edit"
    get "/memes" $ do
        redirect "/memes/0"
    get "/memes/:pagenum" $ do
        pagenumStr <- param "pagenum"
        let pagenum = read pagenumStr :: Int
        memes <- liftIO $ readMemeFile pagenum
        html $ renderHtml $ memeHTML pagenum memes
