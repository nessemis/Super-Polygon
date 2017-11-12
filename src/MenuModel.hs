module MenuModel where
    
data MenuState = MenuState {
    visible           :: Bool,
    screen            :: Screen
}

data Screen = 
    LevelSelect {
        levels         :: [String],
        selectedButton :: Int,
        displacement   :: Float        
    }
    | EndGameMessage String Screen
    | LevelOptionsSelect LevelParameters Screen

extractLevelSelectScreen :: Screen -> Screen
extractLevelSelectScreen screen = case screen of
    LevelSelect{}                    -> screen
    EndGameMessage _ levelScreen     -> levelScreen
    LevelOptionsSelect _ levelScreen -> levelScreen

data LevelParameters = LevelParameters {
    randomOrLoad :: Either Int String, --int is the seed, string the path
    levelOption  :: LevelOptions
}

data LevelOptions = SinglePlayer | MultiPlayer | Ai deriving Show
    

initialScreen :: [String] -> Screen
initialScreen buttonStrings = LevelSelect buttonStrings 0 0

initialMenuState :: [String] -> MenuState
initialMenuState buttonStrings = MenuState True $ initialScreen buttonStrings

endGameMenuState :: String -> Screen -> MenuState
endGameMenuState message levelSelectScreen = MenuState True $ EndGameMessage message levelSelectScreen
