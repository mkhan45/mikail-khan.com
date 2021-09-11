{-# LANGUAGE DeriveGeneric #-}

module Views.Portfolio.PortfolioData where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T


data Project = Project { 
                            projectName :: T.Text,
                            projectURL :: T.Text,
                            projectThumbnail :: T.Text,
                            projectDesc :: T.Text,
                            projectDetailDesc :: T.Text 
                       } deriving (Generic, Show)

instance FromJSON Project
