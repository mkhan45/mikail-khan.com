{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeData where

import qualified Data.Text as T

import Data.Aeson.Types
import Data.Scientific
import Data.HashMap.Strict hiding (map)
import qualified Data.Vector as V

import qualified System.IO.Strict as SIO

import Text.Toml
import Text.Toml.Types

data Category = WebDev | ObjectOriented | Python | Misc deriving Show

data Skill = Skill {
                        skillName :: T.Text,
                        skillCategories :: [Category],
                        skillExperience :: Int
                   } deriving Show

data Experience = Experience {
                        experienceName :: T.Text,
                        experienceDescription :: [T.Text],
                        experienceCategories :: [Category]
                  } deriving Show

data Education = Education {
                        educationName :: T.Text,
                        educationDescription :: [T.Text]
                 } deriving Show

data Resume = Resume [Skill] [Experience] [Education] deriving Show

parseCategory :: Value -> Category
parseCategory (String "Object Oriented") = ObjectOriented
parseCategory (String "Python") = Python
parseCategory (String "Web Development") = WebDev
parseCategory _ = Misc

parseSkill :: Value -> Skill
parseSkill (Object o) =
    let String  name            = o ! T.pack "name"
        Array   categoriesText  = o ! T.pack "categories"
        Number  expSci          = o ! T.pack "years_experience"
        Right   experience      = floatingOrInteger expSci
    in  
        Skill {
                skillName = name,
                skillExperience = experience,
                skillCategories = map parseCategory (V.toList categoriesText)
        }

parseExperience :: Value -> Experience
parseExperience (Object o) =
    let String  name            = o ! T.pack "name"
        Array   categoriesText  = o ! T.pack "category"
        Array   descriptionText = o ! T.pack "description"
    in  
        Experience {
                experienceName        = name,
                experienceCategories  = map parseCategory (V.toList categoriesText),
                experienceDescription = map (\(String s) -> s) (V.toList descriptionText)
        }

parseEducation :: Value -> Education
parseEducation (Object o) =
    let String  name            = o ! T.pack "name"
        Array   descriptionText = o ! T.pack "description"
    in  
        Education {
                educationName        = name,
                educationDescription = map (\(String s) -> s) (V.toList descriptionText)
        }

parseResume :: Value -> Resume
parseResume (Object o) =
    let Array skillsVector      = o ! T.pack "skills"
        Array experiencesVector = o ! T.pack "experience"
        Array educationsVector  = o ! T.pack "education"
        skills      = V.toList skillsVector
        experiences = V.toList experiencesVector
        educations  = V.toList educationsVector
    in
        Resume (map parseSkill skills) (map parseExperience experiences) (map parseEducation educations)

readResume :: IO Resume
readResume = do
    contents <- SIO.readFile "static/Assets/resume.toml"
    let Right t = parseTomlDoc "" $ T.pack contents
        resumeJSON = toJSON t
    return $ parseResume resumeJSON
