{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeView where

import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import           Data.List                      (partition)

import           Text.Blaze
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html                (preEscapedToHtml)
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           System.IO                      (writeFile)

import           Control.Monad                  (forM_)

import           Views.Resume.ResumeData
import           Views.Util

repeatStars :: Int -> H.Html
repeatStars n = do
    H.span $ forM_ [1..n] (const starHTML)
    where starID = H.toValue $ T.pack "ei-star"
          starHTML :: H.Html
          starHTML = H.span ! class_ "star-icon" $ preEscapedToHtml $ svgFromPaths starSVGPaths

skillsTab :: T.Text -> (Skill -> Bool) -> [Skill] -> Bool -> H.Html
skillsTab name pred skillsUnfiltered isDefault = 
    H.div ! A.class_ "skillTab" $ do
        H.input ! A.type_ "radio" ! A.id tabID ! A.name "skill-tabs" ! checked
        H.label ! A.for tabID $ H.toHtml name
        H.div ! A.class_ "skillTabContent" $ do
            if not $ null five_year then
                H.ul $ do
                    H.li $ H.h3 $ repeatStars 5
                    H.li $ H.toHtml $ skillString five_year
            else mempty

            if not $ null three_year then
                H.ul $ do
                    H.li $ H.h3 $ repeatStars 4
                    H.li $ H.toHtml $ skillString three_year
            else mempty

            if not $ null one_year then
                H.ul $ do
                    H.li $ H.h3 $ repeatStars 3
                    H.li $ H.toHtml $ skillString one_year
            else mempty

    where tabID = H.toValue $ name <> "_tab"
          skills = filter pred skillsUnfiltered

          (five_year, less_five) = partition ((>=5) . skillSkillLevel) skills
          (three_year, less_three) = partition ((>=3) . skillSkillLevel) less_five
          one_year = filter ((>=1) . skillSkillLevel) less_three

          skillString :: [Skill] -> T.Text
          skillString ls = T.intercalate ", " $ map skillName ls

          checked = if isDefault then A.checked mempty
                                 else mempty

instance ToMarkup Skills where
    toMarkup (Skills skills) = do
        H.div ! A.class_ "resumeSection skills" $ do
            H.h2 "Skills"
            H.hr
            H.div ! A.class_ "skillTabs" $ do
                skillsTab "All" (const True) skills True
                skillsTab "Programming Languages" ((elem "Programming Language") . skillCategories) skills False
                skillsTab "Object Oriented" ((elem "Object Oriented") . skillCategories) skills False
                skillsTab "Web Dev" ((elem "Web Development") . skillCategories) skills False

descEntry :: T.Text -> H.Html
descEntry desc = do
    H.li $ H.toHtml desc

linkIcon :: H.Html
linkIcon = H.span ! A.class_ "link-icon" $ preEscapedToHtml $ svgFromPaths linkSVGPaths

addLink :: Maybe T.Text -> H.Html -> H.Html
addLink Nothing h = h
addLink (Just url) h = H.span ! A.class_ "lift" $
                        H.a ! A.href (H.toValue url) $ (h <> linkIcon)

timeframeHTML :: T.Text -> H.Html
timeframeHTML timeframe = H.span ! A.class_ "timeframe" $ H.toHtml $ "(" <> timeframe <> ")"

instance ToMarkup Experiences where
    toMarkup (Experiences experiences) = do
        H.div ! A.class_ "resumeSection experienceList" $ do
            H.h2 "Experience"
            H.hr
            mapM_ H.toHtml experiences

instance ToMarkup Experience where
    toMarkup experience = do
        H.div ! A.class_ "experienceEntry" $ do
            H.ul $ do
                H.li $ H.h3 $ addLink (experienceURL experience) (name <> timeframe)
                mapM_ descEntry (experienceDescription experience)
        where name = H.toHtml $ experienceName experience
              timeframe = timeframeHTML $ experienceTimeframe experience

instance ToMarkup Educations where
    toMarkup (Educations educations) = do
        H.div ! A.class_ "resumeSection educationList" $ do
            H.h2 "Education"
            H.hr
        mapM_ H.toHtml educations

instance ToMarkup Education where
    toMarkup education = do
        H.div ! A.class_ "educationEntry" $ do
            H.ul $ do
                H.li $ H.h3 $ addLink (educationURL education) (name <> timeframe)
                mapM_ descEntry (educationDescription education)
        where name = H.toHtml $ educationName education
              timeframe = maybe "" timeframeHTML (educationTimeframe education)

resumeHTML :: Resume -> H.Html
resumeHTML (Resume skills experiences educations) =
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            link ! rel "stylesheet" ! href "/CSS/base.css"
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        body ! A.class_ "resumeContainer flex-center" $ do
            H.div ! class_ "resumeHeader" $ do
                H.h1 "Mikail Khan"
                H.h3 "mikail [at] mikail-khan [dot] com"
                githubIconBtn
                linkedinIconBtn
                H.nav ! class_ "body-center smallmenu" $ do
                    H.div $ linkButton "/" "Home"
                    H.div $ linkButton "/portfolio" "Portfolio"
            H.div ! A.class_ "resume" $ do
                H.toHtml skills
                H.toHtml experiences
                H.toHtml educations

reloadResumeCache :: IO ()
reloadResumeCache = do
    resume <- readResume
    let htmlOut = LT.unpack $ renderHtml $ resumeHTML resume
    writeFile "generated/resume.html" htmlOut
