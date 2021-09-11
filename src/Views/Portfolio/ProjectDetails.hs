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
            H.title $ H.toHtml projName
            counterDevLink
            link ! rel "stylesheet" ! href "/CSS/base.css"
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
            script ! src "/js/pixi.min.js" $ mempty
    H.body $ do
        H.canvas ! A.id "bg" $ mempty
        H.main $ do
            H.div ! A.class_ "detail-container flex-parent" $ do
                H.div ! A.class_ "header detail-header opaque" $ do
                    H.h1 $ H.toHtml projName
                    H.div ! A.class_ "detail-link" $ do
                        H.a ! A.href (H.toValue url) $ H.toHtml url
                    H.nav ! class_ "body-center smallmenu" $ do
                        H.div $ linkButton "/" "Home"
                        H.div $ linkButton "/portfolio" "Portfolio"
                H.div ! A.class_ "body-center detail-body" $ do
                    H.div ! A.class_ "detail-card opaque" $ do
                        H.div ! A.class_ "detail-img-container" $ do
                            H.img ! A.src thumbnail ! A.class_ "detail-img"
                        H.p $ do
                            H.toHtml desc
        script ! src "/js/index.js" $ mempty
    where projName = projectName project
          desc = H.preEscapedToHtml $ projectDetailDesc project
          thumbnail = H.toValue $ projectThumbnail project
          url = projectURL project
