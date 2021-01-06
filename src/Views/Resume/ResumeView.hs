{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeView where

import qualified Data.Text                      as T
import           Data.List                      (partition)
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           Control.Monad                  (forM_)

import           Views.Resume.ResumeData
import           Views.Index                    (githubIconBtn, linkedinIconBtn)

repeatStars :: Int -> H.Html
repeatStars n = H.span $ forM_ [1..n] (\_ -> starHTML)
    where starID = H.toValue $ T.pack "ei-star"
          starHTML :: H.Html
          starHTML = H.span ! class_ "star-icon" ! dataAttribute "icon" starID ! dataAttribute "size" "s" $ "★"

skillsHTML :: [Skill] -> H.Html
skillsHTML skills = do
    H.div ! A.class_ "resumeSection skills" $ do
        H.h2 "Skills"
        H.hr
        H.ul $ do
            H.li $ H.h3 $ repeatStars 5
            H.li $ H.toHtml $ skillString five_year
        H.ul $ do
            H.li $ H.h3 $ repeatStars 3
            H.li $ H.toHtml $ skillString three_year
        H.ul $ do
            H.li $ H.h3 $ repeatStars 1
            H.li $ H.toHtml $ skillString one_year
    where 
        (five_year, less_five) = partition ((>=5) . skillExperience) skills
        (three_year, less_three) = partition ((>=3) . skillExperience) less_five
        one_year = filter ((>=1) . skillExperience) less_three

        skillString :: [Skill] -> T.Text
        skillString ls = T.intercalate ", " $ map skillName ls

descEntry :: T.Text -> H.Html
descEntry desc = do
    H.li $ H.toHtml desc

experiencesHTML :: [Experience] -> H.Html
experiencesHTML experiences = do
    H.div ! A.class_ "resumeSection experienceList" $ do
        H.h2 "Experience"
        H.hr
        mapM_ experienceEntry experiences

experienceEntry :: Experience -> H.Html
experienceEntry experience = do
    H.div ! A.class_ "experienceEntry" $ do
        H.ul $ do
            H.li $ H.h3 name
            mapM_ descEntry (experienceDescription experience)
    where name = H.toHtml $ experienceName experience

educationsHTML :: [Education] -> H.Html
educationsHTML educations = do
    H.div ! A.class_ "resumeSection educationList" $ do
        H.h2 "Education"
        H.hr
        mapM_ educationEntry educations

educationEntry :: Education -> H.Html
educationEntry education = do
    H.div ! A.class_ "educationEntry" $ do
        H.ul $ do
            H.li $ H.h3 name
            mapM_ descEntry (educationDescription education)
    where name = H.toHtml $ educationName education

resumeHTML :: Resume -> H.Html
resumeHTML (Resume skills experiences educations) =
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            link ! rel "stylesheet" ! href "/CSS/base.css"
            link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/evil-icons@1.9.0/assets/evil-icons.min.css"
            script ! src "https://cdn.jsdelivr.net/npm/evil-icons@1.9.0/assets/evil-icons.min.js" $ mempty
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        body ! A.class_ "resumeContainer flex-center" $ do
            H.div ! class_ "resumeHeader" $ do
                H.h1 "Mikail Khan"
                H.h3 "mikail [at] mikail-khan [dot] com"
                githubIconBtn
                linkedinIconBtn
            H.div ! A.class_ "resume" $ do
                skillsHTML skills
                experiencesHTML experiences
                educationsHTML educations
