{-# LANGUAGE OverloadedStrings #-}

module Views.Portfolio.PortfolioView where

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

instance ToMarkup Project where
    toMarkup project = 
        H.div ! class_ "project" $ a ! href url $ do
            H.span ! class_ "align-center" $ h1 nameHtml
            img ! src thumbnail
            H.div ! class_ "project-desc" $ discHtml
        where url = H.toValue $ projectURL project
              nameHtml = H.toHtml $ projectName project
              thumbnail = H.toValue $ projectThumbnail project
              discHtml = H.toHtml $ projectDesc project

portfolio :: [Project] -> H.Html
portfolio projects = 
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            counterDevLink
            link ! rel "stylesheet" ! href "/CSS/base.css"
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
            script ! src "/js/pixi.min.js" $ mempty
        body $ do
            H.canvas ! A.id "bg" $ mempty
            H.main $ do
                H.div ! class_ "flex-parent" $ H.div ! class_ "section" $ do
                    H.div ! class_ "header opaque" $ do
                        h1 "Projects"
                        H.nav ! class_ "body-center smallmenu" $ do
                            H.div $ linkButton "/" "Home"
                            H.div $ linkButton "/resume" "Resume"
                    H.div ! class_ "body-center" $ do
                        H.div ! class_ "project-grid" $ do 
                            mapM_ H.toHtml projects
            script ! src "/js/index.js" $ mempty

reloadPortfolioCache :: IO ()
reloadPortfolioCache = do
    projects <- readProjects
    let htmlOut = LT.unpack $ renderHtml $ portfolio projects
    writeFile "generated/portfolio.html" htmlOut
