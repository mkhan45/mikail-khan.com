{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeView where

import qualified Data.Text                      as T

import           Text.Blaze.Html5               (html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_)
import qualified Text.Blaze.Html5.Attributes    as A

tToHTML = H.toHtml . T.pack

resume :: H.Html
resume = 
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            link ! rel "stylesheet" ! href "https://unpkg.com/boba@1/dist/boba-extended.min.css"
        body $ nav ! class_ "navbar blue" $ do
            a ! href "#" ! class_ "navbar-brand" $ "Mikail Khan"
            input ! class_ "navbar-menu-target" ! type_ "checkbox" ! A.id "nav"
            H.label ! class_ "navbar-menu-toggle" ! for "nav" $ H.span ! class_ "navbar-icon" $ mempty
            ul ! class_ "navbar-menu-items" $ do
                li $ a ! href "#" $ H.span "Category"
                li $ a ! href "#" $ H.span "Category"
                li $ a ! href "#" $ H.span "Category"
