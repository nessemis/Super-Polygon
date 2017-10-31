-- | This module contains the data types
--   which represent the state of the game
module Model where

--import GraphicsModel

import System.IO
import Control.Monad
import Control.Monad.IO.Class

fallspeed :: Float
fallspeed = 1

drawingDistance :: Float
drawingDistance = 20

data Player          = Player Float Float        --(Region in terms of float) Animation.
data FallingShape    = FallingShape Float Float  --(Distance bottom to floor) Height 
type FallingRegion   = [FallingShape]

data InputState = InputState {
  keyEnter :: Bool,
  keyEscape:: Bool,
  keyPause :: Bool,
  keyLeft  :: Bool,
  keyRight :: Bool,
  keyA     :: Bool
}

data MenuState = MenuState {
                   


}

data GameState = GameState {
                   menuState      :: MenuState
                 , menu           :: Bool
                 , hit            :: Bool
                 , player         :: Player
                 , fallingRegions :: [FallingRegion]
                 , inputState     :: InputState
                 , elapsedTime    :: Float
                 , score          :: Float
                 }


                 
initialState :: [FallingRegion] -> GameState
initialState fr = GameState MenuState                                              --MenuState data
                            False                                                   --Is the menu active 
                            False                                                  --Is the player hit
                            (Player 0 0)                                           --Player position and animation
                            fr                                                     --Level data loaded from file 
                            (InputState False False False False False False)             --Inputstate
                            0                                                      --Elapsed time                            
                            0                                                      --Score



readLevelFile :: FilePath -> IO [FallingRegion]
readLevelFile f = do
            fileContent <-  readFile f
            return (map lineToFallingRegion (lines fileContent))

lineToFallingRegion ::  String -> FallingRegion
lineToFallingRegion s = map textShapeToFallingShape (splitOn' (==',') s) 
                        
textShapeToFallingShape :: String ->  FallingShape
textShapeToFallingShape s = FallingShape (read $ head numbers) (read $ head $ tail numbers)
                        where numbers = splitOn' (==' ') s


--https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitOn'     :: (Char -> Bool) -> String -> [String]
splitOn' p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn' p s''
                            where (w, s'') = break p s'