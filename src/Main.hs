module Main where

import Controller
import Model
import Level
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
        fr <- readLevelFile "lvl.txt"
        playIO (InWindow "Counter" (1366, 768) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              (initializedState fr)     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function