module Main where

import Controller
import Model
import Level
import LevelModel
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
        fallingRegions <- readLevelFile "levels\\lvl0.txt"
        levelPaths <- getLevelPaths
        playIO (InWindow "Counter" (1366, 768) (0, 0)) -- Or FullScreen
              black                -- Background color
              60                   -- Frames per second
              (initializeState fallingRegions levelPaths)-- Initial state
              view                 -- View function
              input                -- Event function
              step                 -- Step function