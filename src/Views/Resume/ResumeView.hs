{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeView where

import qualified Data.Text                      as T

import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

tToHTML = H.toHtml . T.pack

data IconBtn = IconBtn { iconText :: T.Text, iconURL :: T.Text, icon :: T.Text }

data Project = Project { projName :: T.Text, projectURL :: T.Text, projectThumbnail :: T.Text, projectDisc :: T.Text }

iconBtn :: IconBtn -> H.Html
iconBtn (IconBtn { iconText=text, iconURL=url, icon=icon }) = 
    a ! href urlV $ H.div ! class_ "icon-btn" ! dataAttribute "icon" iconV ! dataAttribute "size" "m" $ textV
        where urlV = H.toValue url
              iconV = H.toValue icon
              textV = H.toHtml text

projectEntry :: Project -> H.Html
projectEntry (Project { projName=name, projectURL=url, projectThumbnail=thumbnail, projectDisc=projectDisc }) = 
    H.div ! class_ "project" $ a ! href urlV $ do
        H.span ! class_ "align-center" $ h1 nameHtml
        img ! src thumbnailV
        H.div ! class_ "project-desc" $ discHtml
    where urlV = H.toValue url
          nameHtml = H.toHtml name
          thumbnailV = H.toValue thumbnail
          discHtml = H.toHtml projectDisc

projects :: [Project]
projects = [
                Project {
                    projName = "SIMple Mechanics",
                    projectURL = "https://github.com/mkhan45/SIMple-Mechanics",
                    projectThumbnail = "https://mikail-khan.com/img/mech.png",
                    projectDisc = "A 2D rigidbody simulator made to help learn and teach physics online."
                },
                Project {
                    projName = "mikail-khan.com",
                    projectURL = "https://github.com/mkhan45/mikail-khan.com",
                    projectThumbnail = "https://mikail-khan.com/img/site.png",
                    projectDisc = "My personal website. Built with Haskell's Scotty and Blaze-HTML"
                },
                Project {
                    projName = "SIMple Gravity",
                    projectURL = "https://github.com/mkhan45/SIMple-Gravity",
                    projectThumbnail = "https://mikail-khan.com/img/gravity.png",
                    projectDisc = "A universal gravitation simulator made to help learn and teach physics online."
                },
                Project {
                    projName = "Pumice",
                    projectURL = "https://github.com/mkhan45/pumice",
                    projectThumbnail = "https://mikail-khan.com/img/pumice.png",
                    projectDisc = "A vulkano-made light and brittle game engine that rocks."
                },
                Project {
                    projName = "Tetrs",
                    projectURL = "https://github.com/mkhan45/tetrs",
                    projectThumbnail = "https://mikail-khan.com/img/tetrs.png",
                    projectDisc = "Tetris using Rust and ggez."
                },
                Project {
                    projName = "ssshmup",
                    projectURL = "https://github.com/mkhan45/ssshmup",
                    projectThumbnail = "https://mikail-khan.com/img/ssshmup.png",
                    projectDisc = "A super small shoot 'em up"
                }
           ]

resume :: H.Html
resume = 
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
                iconBtn (IconBtn { iconText="GitHub", iconURL="https://github.com/mkhan45", icon="ei-sc-github" })
                iconBtn (IconBtn { iconText="LinkedIn", iconURL="https://www.linkedin.com/in/mikail-khan-6121921a1/", icon="ei-sc-linkedin" })
            H.div ! class_ "body-center" $ do
                H.span ! class_ "align-center" $ h1 "Projects"
                H.div ! class_ "project-grid" $ do 
                    mapM_ projectEntry projects
                    -- H.div ! class_ "project" $ a ! href "https://github.com/mkhan45/SIMple-Mechanics" $ do
                    --     H.span ! class_ "align-center" $ h1 "SIMple Mechanics"
                    --     img ! src "https://mikail-khan.com/img/mech.png"
                    --     H.div ! class_ "project-desc" $ "A 2D rigidbody simulator made to help learn and teach physics online."
                    -- H.div ! class_ "project" $ do
                    --     H.span ! class_ "align-center" $ h1 "mikail-khan.com"
                    --     img ! src "https://mikail-khan.com/img/site.png"
                    --     H.div ! class_ "project-desc" $ "My personal website."
                    -- H.div ! class_ "project" $ do
                    --     H.span ! class_ "align-center" $ h1 "SIMple Gravity"
                    --     img ! src "https://mikail-khan.com/img/gravity.png"
                    --     H.div ! class_ "project-desc" $ "A universal gravitation simulator made to help learn and teach physics online."
                    -- H.div ! class_ "project" $ do
                    --     H.span ! class_ "align-center" $ h1 "Pumice"
                    --     img ! src "https://mikail-khan.com/img/pumice.png"
                    --     H.div ! class_ "project-desc" $ "A holey, vulkano-built, 2D game engine that rocks."
                    -- H.div ! class_ "project" $ do
                    --     H.span ! class_ "align-center" $ h1 "Tetrs"
                    --     img ! src "https://mikail-khan.com/img/tetrs.png"
                    --     H.div ! class_ "project-desc" $ "Tetris using Rust and ggez."
                    -- H.div ! class_ "project" $ do
                    --     H.span ! class_ "align-center" $ h1 "ssshmup"
                    --     img ! src "https://mikail-khan.com/img/ssshmup.png"
                    --     H.div ! class_ "project-desc" $ "A super small shoot 'em up."
