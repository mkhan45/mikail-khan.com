{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.ResumeData where

import qualified Data.Text as T
import Data.List hiding (lookup)
import Data.Char
import Prelude hiding (lookup)

import Data.Aeson.Types
import Data.HashMap.Strict hiding (map, foldr)
import qualified Data.Vector as V

import GHC.Generics

import qualified System.IO.Strict as SIO

import Text.Toml
import Text.Toml.Types

-- thanks @Noughtmare

customOptions = defaultOptions
                    { 
                      fieldLabelModifier = map toLower . intercalate "_" . tail . splitCamel,
                      constructorTagModifier = unwords . splitCamel
                    }

splitCamel :: String -> [String]
splitCamel = finish . foldr step (True, [""]) where
    finish (_, "" : xs) = xs
    finish (_, xs) = xs

    step x ~(b', ys') = (b, stepList ys')
        where
            b = isUpper x
            stepList
                | b && not b' = newWord . newLetter x
                | not b && b' = newLetter x . newWord
                | otherwise = newLetter x

    newWord ("" : xs) = "" : xs
    newWord (x : xs) = "" : (x : xs)

    newLetter c (x : xs) = (c : x) : xs

type Category = T.Text

data Skill = Skill {
                        skillName :: T.Text,
                        skillCategories :: [Category],
                        skillSkillLevel :: Int
                   } deriving (Generic, Show)
instance FromJSON Skill
    where parseJSON = genericParseJSON customOptions


data Experience = Experience {
                        experienceName :: T.Text,
                        experienceDescription :: [T.Text],
                        experienceCategories :: [Category],
                        experienceURL :: Maybe T.Text,
                        experienceTimeframe :: T.Text
                  } deriving (Generic, Show)
instance FromJSON Experience
    where parseJSON = genericParseJSON customOptions


data Education = Education {
                        educationName :: T.Text,
                        educationDescription :: [T.Text],
                        educationURL :: Maybe T.Text,
                        educationTimeframe :: Maybe T.Text
                 } deriving (Generic, Show)
instance FromJSON Education
    where parseJSON = genericParseJSON customOptions


data Resume = Resume [Skill] [Experience] [Education] deriving (Show)
instance FromJSON Resume
    where parseJSON (Object o) = Resume <$> o .: "skills" <*> o .: "experience" <*> o.: "education"

readResume :: IO Resume
readResume = do
    contents <- SIO.readFile "static/Assets/resume.toml"
    let Right t = parseTomlDoc "" $ T.pack contents
        resumeJSON = toJSON t
        Success r = fromJSON resumeJSON
    return r
