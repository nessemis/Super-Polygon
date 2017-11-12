module MenuModel where
    
data MenuState = MenuState {
    visible           :: Bool,
    screen            :: Screen
}

data Screen = 
    LevelSelect {
        buttons         :: [String],
        selectedButton  :: Int,
        displacement    :: Float        
    }
    | EndGameMessage String Screen
    | LevelOptionsSelect LevelOptions Screen

extractLevelSelectScreen :: Screen -> Screen
extractLevelSelectScreen screen = case screen of
    LevelSelect{}                    -> screen
    EndGameMessage _ levelScreen     -> levelScreen
    LevelOptionsSelect _ levelScreen -> levelScreen

data LevelOptions = LevelOptions {
    randomOrLoad :: Either Int String, --int is the seed, string the path
    playOptions  :: PlayOptions
}

data PlayOptions = SinglePlayer | MultiPlayer | Ai deriving Show
    

initialScreen :: [String] -> Screen
initialScreen buttonStrings = LevelSelect buttonStrings 0 0

initialMenuState :: [String] -> MenuState
initialMenuState buttonStrings = MenuState True (initialScreen buttonStrings)

endGameMenuState :: String -> Screen -> MenuState
endGameMenuState message levelSelectScreen = MenuState True (EndGameMessage message levelSelectScreen)
