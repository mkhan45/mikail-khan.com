{-# LANGUAGE OverloadedStrings #-}

module Views.Index where

import           Data.Text                      as T
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           Views.Portfolio.PortfolioData

iconBtn :: IconBtn -> H.Html
iconBtn icon = 
    a ! href url $ H.div ! class_ "icon-btn" ! dataAttribute "icon" id ! dataAttribute "size" "m" $ textFallback
        where url = H.toValue $ iconURL icon
              id = H.toValue $ iconID icon
              textFallback = H.toHtml $ iconText icon

linkButton :: T.Text -> T.Text -> H.Html
linkButton url text = do
    a ! class_ "linkButton" ! href (H.toValue url) $ H.toHtml text

index :: H.Html
index =
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            link ! rel "stylesheet" ! href "/CSS/base.css"
            link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/evil-icons@1.9.0/assets/evil-icons.min.css"
            script ! src "https://cdn.jsdelivr.net/npm/evil-icons@1.9.0/assets/evil-icons.min.js" $ mempty
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        body $ H.div ! class_ "flex-parent" $ H.div ! class_ "section" $ do
            H.div ! class_ "header" $ do
                h1 "Mikail Khan"
                h2 "Math + CS @ Purdue"
                iconBtn (IconBtn { iconText="GitHub", iconURL="https://github.com/mkhan45", iconID="ei-sc-github" })
                iconBtn (IconBtn { iconText="LinkedIn", iconURL="https://www.linkedin.com/in/mikail-khan-6121921a1/", iconID="ei-sc-linkedin" })
                iconBtn (IconBtn { iconText="Blog", iconURL="https://mkhan45.github.io", iconID="ei-pencil" })
            H.div ! class_ "body-center menu" $ do
                H.div $ do
                    linkButton "/portfolio" "Portfolio"
                    linkButton "/resume" "Resume"
