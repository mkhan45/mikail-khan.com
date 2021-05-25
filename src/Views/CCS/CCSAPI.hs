{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.CCS.CCSAPI
    ( CCSAPI
    , ccsServer
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

type CCSAPI = "cool_code_snippets" :> Get '[HTML] H.Html

ccsServer :: Server CCSAPI
ccsServer = ccsEndpoint

ccsEndpoint :: Handler H.Html
ccsEndpoint = do
    return $ H.toHtml $ T.pack "test"
