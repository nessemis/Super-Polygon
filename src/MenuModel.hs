module MenuModel where
    
data MenuState = MenuState {
    visible         :: Bool,
    screen          :: Screen
}

data Screen = 
    LevelSelect {
        selectedButton  :: Int,
        displacement    :: Float        
    }
    | EndGameMessage String
    | LevelOptionsSelect LevelOptions

data LevelOptions = LevelOptions {
    randomOrLoad :: Either Int String, --int is the seed, string the path
    playOptions  :: PlayOptions
}

data PlayOptions = SinglePlayer | MultiPlayer | Ai deriving Show
    

initialScreen :: Screen
initialScreen = LevelSelect 0 0

initialMenuState :: MenuState
initialMenuState = MenuState True initialScreen

endGameMenuState :: String -> MenuState
endGameMenuState = MenuState True . EndGameMessage
