{-# LANGUAGE OverloadedStrings #-}

module Views.Util where

import           Data.Text                      as T hiding (foldl)
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5                as H
import           Text.Blaze.Html                (preEscapedToHtml)
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           Views.Portfolio.PortfolioData

type SVGPath = T.Text

data IconBtn = IconBtn { 
                            iconText :: T.Text,
                            iconURL :: T.Text,
                            iconPaths :: [SVGPath]
                       }

linkButton :: T.Text -> T.Text -> H.Html
linkButton url text = do
    a ! class_ "linkButton" ! href (H.toValue url) $ H.toHtml text

iconBtn :: IconBtn -> H.Html
iconBtn icon = 
    a ! href url $ H.span ! class_ "icon-btn" $ (preEscapedToHtml $ svgFromPaths $ iconPaths icon)
        where url = H.toValue $ iconURL icon
              textFallback = H.toHtml $ iconText icon

githubIconBtn :: H.Html
githubIconBtn = iconBtn (IconBtn { iconText="GitHub", iconURL="https://github.com/mkhan45", iconPaths=githubSVGPaths})

linkedinIconBtn :: H.Html
linkedinIconBtn = iconBtn (IconBtn { iconText="LinkedIn", iconURL="https://www.linkedin.com/in/mikail-khan-6121921a1/", iconPaths=linkedInSVGPaths})

blogIconBtn :: H.Html
blogIconBtn = iconBtn (IconBtn { iconText="Blog", iconURL="https://mkhan45.github.io", iconPaths=pencilSVGPaths})

-- paths from evil icons
-- https://evil-icons.io/
githubSVGPaths :: [SVGPath]
githubSVGPaths = ["M25 10c-8.3 0-15 6.7-15 15 0 6.6 4.3 12.2 10.3 14.2.8.1 1-.3 1-.7v-2.6c-4.2.9-5.1-2-5.1-2-.7-1.7-1.7-2.2-1.7-2.2-1.4-.9.1-.9.1-.9 1.5.1 2.3 1.5 2.3 1.5 1.3 2.3 3.5 1.6 4.4 1.2.1-1 .5-1.6 1-2-3.3-.4-6.8-1.7-6.8-7.4 0-1.6.6-3 1.5-4-.2-.4-.7-1.9.1-4 0 0 1.3-.4 4.1 1.5 1.2-.3 2.5-.5 3.8-.5 1.3 0 2.6.2 3.8.5 2.9-1.9 4.1-1.5 4.1-1.5.8 2.1.3 3.6.1 4 1 1 1.5 2.4 1.5 4 0 5.8-3.5 7-6.8 7.4.5.5 1 1.4 1 2.8v4.1c0 .4.3.9 1 .7 6-2 10.2-7.6 10.2-14.2C40 16.7 33.3 10 25 10z"]

linkedInSVGPaths :: [SVGPath]
linkedInSVGPaths = ["M36.1 12H13.9c-1.1 0-1.9.8-1.9 1.9v22.2c0 1 .9 1.9 1.9 1.9h22.2c1.1 0 1.9-.8 1.9-1.9V13.9c0-1.1-.9-1.9-1.9-1.9zM20 34h-4V22h4v12zm-2-13.6c-1.3 0-2.4-1.1-2.4-2.4 0-1.3 1.1-2.4 2.4-2.4 1.3 0 2.4 1.1 2.4 2.4 0 1.3-1.1 2.4-2.4 2.4zM34 34h-4v-6c0-1.6-.4-3.2-2-3.2s-2 1.6-2 3.2v6h-4V22h4v1.4h.2c.5-1 1.8-1.8 3.3-1.8 3.7 0 4.5 2.4 4.5 5.4v7z"]

pencilSVGPaths :: [SVGPath]
pencilSVGPaths = [
                "M9.6 40.4l2.5-9.9L27 15.6l7.4 7.4-14.9 14.9-9.9 2.5zm4.3-8.9l-1.5 6.1 6.1-1.5L31.6 23 27 18.4 13.9 31.5z",
                "M17.8 37.3c-.6-2.5-2.6-4.5-5.1-5.1l.5-1.9c3.2.8 5.7 3.3 6.5 6.5l-1.9.5z",
                "M29.298 19.287l1.414 1.414-13.01 13.02-1.414-1.41z",
                "M11 39l2.9-.7c-.3-1.1-1.1-1.9-2.2-2.2L11 39z",
                "M35 22.4L27.6 15l3-3 .5.1c3.6.5 6.4 3.3 6.9 6.9l.1.5-3.1 2.9zM30.4 15l4.6 4.6.9-.9c-.5-2.3-2.3-4.1-4.6-4.6l-.9.9z"
                 ]

linkSVGPaths :: [SVGPath]
linkSVGPaths = [
               "M24 30.2c0 .2.1.5.1.8 0 1.4-.5 2.6-1.5 3.6l-2 2c-1 1-2.2 1.5-3.6 1.5-2.8 0-5.1-2.3-5.1-5.1 0-1.4.5-2.6 1.5-3.6l2-2c1-1 2.2-1.5 3.6-1.5.3 0 .5 0 .8.1l1.5-1.5c-.7-.3-1.5-.4-2.3-.4-1.9 0-3.6.7-4.9 2l-2 2c-1.3 1.3-2 3-2 4.9 0 3.8 3.1 6.9 6.9 6.9 1.9 0 3.6-.7 4.9-2l2-2c1.3-1.3 2-3 2-4.9 0-.8-.1-1.6-.4-2.3L24 30.2z",
               "M33 10.1c-1.9 0-3.6.7-4.9 2l-2 2c-1.3 1.3-2 3-2 4.9 0 .8.1 1.6.4 2.3l1.5-1.5c0-.2-.1-.5-.1-.8 0-1.4.5-2.6 1.5-3.6l2-2c1-1 2.2-1.5 3.6-1.5 2.8 0 5.1 2.3 5.1 5.1 0 1.4-.5 2.6-1.5 3.6l-2 2c-1 1-2.2 1.5-3.6 1.5-.3 0-.5 0-.8-.1l-1.5 1.5c.7.3 1.5.4 2.3.4 1.9 0 3.6-.7 4.9-2l2-2c1.3-1.3 2-3 2-4.9 0-3.8-3.1-6.9-6.9-6.9z",
               "M20 31c-.3 0-.5-.1-.7-.3-.4-.4-.4-1 0-1.4l10-10c.4-.4 1-.4 1.4 0s.4 1 0 1.4l-10 10c-.2.2-.4.3-.7.3z"
               ]

starSVGPaths :: [SVGPath]
starSVGPaths = [
               "M15.2 40.6c-.2 0-.4-.1-.6-.2-.4-.3-.5-.7-.4-1.1l3.9-12-10.2-7.5c-.4-.3-.5-.7-.4-1.1s.5-.7 1-.7h12.7L25 5.9c.1-.4.5-.7 1-.7s.8.3 1 .7L30.9 18h12.7c.4 0 .8.2 1 .6s0 .9-.4 1.1L34 27.1l3.9 12c.1.4 0 .9-.4 1.1s-.8.3-1.2 0L26 33l-10.2 7.4c-.2.1-.4.2-.6.2zM26 30.7c.2 0 .4.1.6.2l8.3 6.1-3.2-9.8c-.1-.4 0-.9.4-1.1l8.3-6.1H30.1c-.4 0-.8-.3-1-.7L26 9.5l-3.2 9.8c-.1.4-.5.7-1 .7H11.5l8.3 6.1c.4.3.5.7.4 1.1L17.1 37l8.3-6.1c.2-.1.4-.2.6-.2z"
               ]

svgFromPaths :: [SVGPath] -> T.Text
svgFromPaths ls = "<svg class='icon' viewBox='0 0 50 50'>" <> pathsHTML <> "</svg>"
    where pathToHtml p = "<path d='" <> p <> "'></path>"
          pathsHTML = foldl (\a p -> a <> pathToHtml p) "" ls 
