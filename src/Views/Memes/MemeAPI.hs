{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Memes.MemeAPI 
    ( MemeAPI
    , memeServer 
    ) where

import Servant
import Servant.HTML.Blaze

import qualified Text.Blaze.Html5 as H

import qualified Data.Text as T

import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)

import Control.Monad.IO.Class (liftIO)
import System.Environment

import Views.Memes.MemeData
import Views.Memes.MemeEdit
import Views.Memes.MemeView

data MemeSubmitInfo = MemeSubmitInfo
    { submitTy    :: T.Text
    , submitTitle :: T.Text
    , submitURL   :: T.Text
    , submitPass  :: String }

instance FromForm MemeSubmitInfo where
    fromForm f = MemeSubmitInfo
        <$> parseUnique "type" f
        <*> parseUnique "title" f
        <*> parseUnique "url" f
        <*> parseUnique "password" f

type MemeAPI = "memes"                                :> Get '[HTML] H.Html
          :<|> "memes" :> Capture "pagenum" Int       :> Get '[HTML] H.Html
          :<|> "memes" :> "edit"                      :> Get '[HTML] H.Html
          :<|> "memes" :> "meme_submit" :> MemeSubReq :> Post '[HTML] H.Html

type MemeSubReq = ReqBody '[FormUrlEncoded] MemeSubmitInfo

memeServer :: Server MemeAPI
memeServer = throwError err301 { errHeaders = [("Location", "/memes/0")] }
        :<|> memeEndpoint
        :<|> do
                memes <- liftIO $ readMemeFile 0
                return $ memeEditHTML memes
        :<|> memeSubmitHandler

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
