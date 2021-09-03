{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Portfolio.ProjectDetails where

import Servant
import Servant.HTML.Blaze

import qualified Data.Text                      as T
import qualified Data.Text.Lazy as LT

import           Text.Blaze
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import Text.Blaze.Html.Renderer.Text (renderHtml)

import System.IO (writeFile)

import           Views.Portfolio.PortfolioData
import           Views.Portfolio.Projects

import           Views.Util

import           Data.Maybe
import           Data.List
import           Control.Monad.IO.Class (liftIO)

type ProjectDetailAPI = "portfolio" :> "details" :> Capture "project" String :> Get '[HTML] H.Html

projectDetailServer :: Server ProjectDetailAPI
projectDetailServer = projectDetailEndpoint 
tToHTML = H.toHtml . T.pack

projectDetailEndpoint :: String -> Handler H.Html
projectDetailEndpoint projectNameString = do
    projects <- liftIO $ readProjects
    let projectNameText = T.pack projectNameString
        project = fromJust $ find ((==projectNameText) . projectName) projects
    return $ projectDetailHTML project

projectDetailHTML :: Project -> H.Html
projectDetailHTML project = do
    H.html $ do
        H.head $ do
            counterDevLink
            H.title $ H.toHtml name
    H.body $ do
        H.h1 $ do
            H.toHtml name
        H.p $ do
            H.toHtml desc
    where name = projectName project
          desc = projectDesc project
