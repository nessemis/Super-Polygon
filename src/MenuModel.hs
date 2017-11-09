module MenuModel where
    
data MenuState = MenuState {
visible         :: Bool,
selectedButton  :: Int,
displacement    :: Float
}



initialMenuState :: MenuState
initialMenuState = MenuState True 0 0
