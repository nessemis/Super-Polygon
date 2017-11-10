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

initialScreen :: Screen
initialScreen = LevelSelect 0 0

initialMenuState :: MenuState
initialMenuState = MenuState True initialScreen

endGameMenuState :: String -> MenuState
endGameMenuState = MenuState True . EndGameMessage
