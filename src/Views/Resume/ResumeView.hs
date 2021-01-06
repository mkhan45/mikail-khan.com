{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeView where

import qualified Data.Text                      as T
import           Text.Blaze.Html5               (script, meta, dataAttribute, h1, h2, img, html, a, li, ul, input, body, nav, link, (!))
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    (class_, rel, href, for, type_, src, content, name)
import qualified Text.Blaze.Html5.Attributes    as A

import           Views.Resume.ResumeData

-- I feel like this should exist
-- elements matching go on the lft
-- others go on the right
sieve :: (a -> Bool) -> [a] -> ([a], [a])
sieve f ls = go f ls ([], [])
    where go :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
          go _ []     (lhs, rhs) = (lhs, rhs)
          go f (x:xs) (lhs, rhs) = if (f x) then go f xs (x:lhs, rhs)
                                            else go f xs (lhs, x:rhs)

skillsHTML :: [Skill] -> H.Html
skillsHTML skills = do
    H.div ! A.class_ "resumeSection skills" $ do
        H.h2 "Skills"
        H.hr
        H.span $ H.toHtml ("5+ Years: " <> (skillString five_year))
        H.br
        H.span $ H.toHtml ("3+ Years: " <> (skillString three_year))
        H.br
        H.span $ H.toHtml ("1+ Years: " <> (skillString one_year))
    where 
        (five_year, less_five) = sieve ((>=5) . skillExperience) skills
        (three_year, less_three) = sieve ((>=3) . skillExperience) less_five
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
        H.h3 name
        H.ul $ do
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
        H.h3 name
        H.ul $ do
            mapM_ descEntry (educationDescription education)
    where name = H.toHtml $ educationName education

resumeHTML :: Resume -> H.Html
resumeHTML (Resume skills experiences educations) =
    html $ do
        H.head $ do
            H.title "Mikail Khan"
            link ! rel "stylesheet" ! href "/CSS/base.css"
            meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        body ! A.class_ "resumeContainer flex-center" $ do
            H.br
            H.h1 "Mikail Khan"
            H.div ! A.class_ "resume" $ do
                skillsHTML skills
                experiencesHTML experiences
                educationsHTML educations
