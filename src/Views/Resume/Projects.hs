{-# LANGUAGE OverloadedStrings #-}

module Views.Resume.Projects where

import qualified Data.Text as T
import Views.Resume.ResumeData

projects :: [Project]
projects = [
                Project {
                    projName = "SIMple Mechanics",
                    projectURL = "https://github.com/mkhan45/SIMple-Mechanics",
                    projectThumbnail = "https://mikail-khan.com/img/mech.png",
                    projectDisc = "A 2D rigidbody simulator made to help learn and teach physics online."
                },
                Project {
                    projName = "mikail-khan.com",
                    projectURL = "https://mikail-khan.com",
                    projectThumbnail = "https://mikail-khan.com/img/site.png",
                    projectDisc = "My personal website. Built with Haskell's Scotty and Blaze-HTML"
                },
                Project {
                    projName = "SIMple Gravity",
                    projectURL = "https://github.com/mkhan45/SIMple-Gravity",
                    projectThumbnail = "https://mikail-khan.com/img/gravity.png",
                    projectDisc = "A universal gravitation simulator made to help learn and teach physics online."
                },
                Project {
                    projName = "Pumice",
                    projectURL = "https://github.com/mkhan45/pumice",
                    projectThumbnail = "https://mikail-khan.com/img/pumice.png",
                    projectDisc = "A vulkano-made light and brittle game engine that rocks."
                },
                Project {
                    projName = "Tetrs",
                    projectURL = "https://github.com/mkhan45/tetrs",
                    projectThumbnail = "https://mikail-khan.com/img/tetrs.png",
                    projectDisc = "Tetris using Rust and ggez."
                },
                Project {
                    projName = "ssshmup",
                    projectURL = "https://github.com/mkhan45/ssshmup",
                    projectThumbnail = "https://mikail-khan.com/img/ssshmup.png",
                    projectDisc = "A super small shoot 'em up"
                }
           ]
