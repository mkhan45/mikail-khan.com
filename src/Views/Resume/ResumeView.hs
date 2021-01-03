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
        body $ do
            H.h1 "Under Construction"
