module Menu where
        
import MenuModel
import InputModel
import Model

--at the moment, menu doesn't do anything
updateMenuState :: InputState -> MenuState -> Caller MenuState
updateMenuState is menuState = Caller (const menuState is) Nothing