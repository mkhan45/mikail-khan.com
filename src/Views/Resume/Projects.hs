{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.Projects where

import qualified Data.Text as T
import Views.Resume.ResumeData
import qualified System.IO.Strict as SIO

import Text.Toml
import Text.Toml.Types

import Data.Aeson.Types

import Data.HashMap.Strict
import qualified Data.Vector as V

getTOML :: IO Table
getTOML = do
    contents <- SIO.readFile "static/Assets/projects.toml"
    let Right t = parseTomlDoc "" $ T.pack contents
    return t

parseProject :: Value -> IO Project
parseProject (Object o) = do
    let String name = o ! T.pack "projName"
        String url = o ! T.pack "projectURL"
        String thumbnail = o ! T.pack "projectThumbnail"
        String desc = o ! T.pack "projectDesc"
    return Project { projectName=name, projectURL=url, projectThumbnail=thumbnail, projectDesc=desc }

readProjects :: IO [Project]
readProjects = do
    tomlIn <- getTOML
    let Object jsObj = toJSON tomlIn
        Array jsArr = jsObj ! T.pack "projects"
    projects <- mapM parseProject jsArr
    return $ V.toList projects
