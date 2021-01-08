{-# LANGUAGE OverloadedStrings #-}

module Views.Util where

import           Data.Text                      as T
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           Views.Portfolio.PortfolioData

linkButton :: T.Text -> T.Text -> H.Html
linkButton url text = do
    a ! class_ "linkButton" ! href (H.toValue url) $ H.toHtml text

iconBtn :: IconBtn -> H.Html
iconBtn icon = 
    a ! href url $ H.div ! class_ "icon-btn" ! dataAttribute "icon" id ! dataAttribute "size" "m" $ textFallback
        where url = H.toValue $ iconURL icon
              id = H.toValue $ iconID icon
              textFallback = H.toHtml $ iconText icon

githubIconBtn :: H.Html
githubIconBtn = iconBtn (IconBtn { iconText="GitHub", iconURL="https://github.com/mkhan45", iconID="ei-sc-github" })

linkedinIconBtn :: H.Html
linkedinIconBtn = iconBtn (IconBtn { iconText="LinkedIn", iconURL="https://www.linkedin.com/in/mikail-khan-6121921a1/", iconID="ei-sc-linkedin" })

blogIconBtn :: H.Html
blogIconBtn = iconBtn (IconBtn { iconText="Blog", iconURL="https://mkhan45.github.io", iconID="ei-pencil" })
