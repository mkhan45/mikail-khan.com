{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import           Data.Text                      as T
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           Views.Portfolio.PortfolioData

import           Views.Util

index :: H.Html
index =
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            link ! rel "stylesheet" ! href "/CSS/base.css"
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        body $ H.div ! class_ "flex-parent" $ H.div ! class_ "section" $ do
            H.div ! class_ "header center-page" $ do
                h1 "Mikail Khan"
                h2 "Math + CS @ Purdue"
                githubIconBtn
                linkedinIconBtn
                blogIconBtn
            H.div ! class_ "body-center menu" $ do
                H.nav $ do
                    linkButton "/portfolio" "Portfolio"
                    linkButton "/resume" "Resume"
