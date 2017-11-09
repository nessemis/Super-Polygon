module Menu where
        
import MenuModel
import InputModel
import Model

--at the moment, menu doesn't do anything
updateMenuState :: InputState -> MenuState -> Caller MenuState
updateMenuState is menuState    
                                | keyRightPress is = Caller menuState {selectedButton = if ((sel + 1) < 3) then sel + 1 else sel} Nothing 
                                | keyLeftPress  is = Caller menuState {selectedButton = if ((sel - 1) >= 0) then sel - 1 else sel} Nothing         
                                | keyEnterPress is = Caller menuState {visible = False} $ Just (StartLevel (LevelOptions (Right ("lvl" ++ (show sel) ++ ".txt" )) undefined undefined))
                                | otherwise        = Caller (updateDisplacement menuState) Nothing
                                                 where sel = selectedButton menuState         



                                                 
updateDisplacement :: MenuState -> MenuState
updateDisplacement ms = ms{displacement = dis + (if abs(dif) <= 0.01 then 0 else (dif / abs(dif) ) / 100)}
                                    where sel' = fromIntegral (selectedButton ms)
                                          dis  = displacement ms
                                          dif  = (sel' - dis)