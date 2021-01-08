{-# LANGUAGE OverloadedStrings #-}

module Views.Navbar where

import           Data.Text                      as T
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

resumeLink :: H.Html
resumeLink = H.a ! A.href "/resume" $ "Resume"

portfolioLink :: H.Html
portfolioLink = H.a ! A.href "/resume" $ "Portfolio"

navbarHTML :: H.Html
navbarHTML = do
    H.nav $ do
        H.span ! A.class_ "navbarName" $ "Mikail Khan"
        H.span ! A.class_ "navbarLinks" $ do
            resumeLink
            portfolioLink
