{-# LANGUAGE OverloadedStrings #-}

module Views.Portfolio.Projects where

import qualified Data.Text as T
import qualified System.IO.Strict as SIO

import Text.Toml
import Text.Toml.Types

import Data.Aeson.Types

import Data.HashMap.Strict
import qualified Data.Vector as V

import Views.Portfolio.PortfolioData

getProjectsTOML :: IO Table
getProjectsTOML = do
    contents <- SIO.readFile "static/Assets/projects.toml"
    let Right t = parseTomlDoc "" $ T.pack contents
    return t

filterResults :: [Result a] -> [a]
filterResults ls = [getSuccess r | r <- ls, isSuccess r]
    where isSuccess  (Success _) = True
          isSuccess  _           = False
          getSuccess (Success s) = s

readProjects :: IO [Project]
readProjects = do
    tomlIn <- getProjectsTOML
    let Object jsObj = toJSON tomlIn
        Array jsArr = jsObj ! T.pack "projects"
        projects = fmap fromJSON (V.toList jsArr) :: [Result Project]
    return $ filterResults projects
