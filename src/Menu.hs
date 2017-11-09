module Menu where
        
import MenuModel
import InputModel
import Model

--at the moment, menu doesn't do anything
updateMenuState :: InputState -> MenuState -> Caller MenuState
updateMenuState is menuState    
                                | keyRightPress is = Caller menuState {selectedButton = (selectedButton menuState) + 1} Nothing 
                                | keyLeftPress  is = Caller menuState {selectedButton = (selectedButton menuState) - 1} Nothing 
                                
                                | keyEnterPress is = Caller menuState $ Just (StartLevel (LevelOptions (Right "lvl.txt") undefined undefined))
                                | otherwise        = Caller menuState {displacement =  if (((displacement menuState)*100) == (fromIntegral (selectedButton menuState))) then (displacement menuState) else 
                                                                                       (if (((displacement menuState) * 100 ) < (fromIntegral (selectedButton menuState)) )
                                                                                       then ((displacement menuState) + 0.001) 
                                                                                       else ((displacement menuState) - 0.001)) } Nothing 
                                                                                       

                                
