{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeView where

import qualified Data.Text                      as T

import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           Views.Resume.ResumeData
import           Views.Resume.Projects


iconBtn :: IconBtn -> H.Html
iconBtn icon = 
    a ! href url $ H.div ! class_ "icon-btn" ! dataAttribute "icon" id ! dataAttribute "size" "m" $ textFallback
        where url = H.toValue $ iconURL icon
              id = H.toValue $ iconID icon
              textFallback = H.toHtml $ iconText icon

projectEntry :: Project -> H.Html
projectEntry project = 
    H.div ! class_ "project" $ a ! href url $ do
        H.span ! class_ "align-center" $ h1 nameHtml
        img ! src thumbnail
        H.div ! class_ "project-desc" $ discHtml
    where url = H.toValue $ projectURL project
          nameHtml = H.toHtml $ projectName project
          thumbnail = H.toValue $ projectThumbnail project
          discHtml = H.toHtml $ projectDesc project

resume :: [Project] -> H.Html
resume projects = 
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            link ! rel "stylesheet" ! href "/CSS/Resume.css"
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
            H.div ! class_ "body-center" $ do
                H.span ! class_ "align-center" $ h1 "Projects"
                H.div ! class_ "project-grid" $ do 
                    mapM_ projectEntry projects
