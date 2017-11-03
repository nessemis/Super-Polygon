module MenuModel where
    
data MenuState = MenuState {
visible    :: Bool
}

initialMenuState :: MenuState
initialMenuState = MenuState True
