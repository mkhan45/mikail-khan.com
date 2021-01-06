{-# LANGUAGE DeriveGeneric #-}

module Views.Portfolio.PortfolioData where

import Data.Aeson

import qualified Data.Text as T

data IconBtn = IconBtn { 
                            iconText :: T.Text,
                            iconURL :: T.Text,
                            iconID :: T.Text 
                       }

data Project = Project { 
                            projectName :: T.Text,
                            projectURL :: T.Text,
                            projectThumbnail :: T.Text,
                            projectDesc :: T.Text 
                       }
