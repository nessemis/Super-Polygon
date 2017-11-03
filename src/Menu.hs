module Menu where
        
import MenuModel
import InputModel

--at the moment, menu doesn't do anything
updateMenuState :: InputState -> MenuState -> MenuState
updateMenuState is menuState = const menuState is