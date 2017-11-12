module Menu where
        
import MenuModel
import InputModel
import Model

updateMenuState :: InputState -> MenuState -> Caller MenuState
updateMenuState is menuState
      | not $ visible menuState = Caller menuState Nothing
      | otherwise = let currentScreen = screen menuState
                        updatedScreen = updateCurrentScreen is currentScreen in
                  Caller menuState{screen = fst updatedScreen} (snd updatedScreen)


updateCurrentScreen :: InputState -> Screen -> (Screen, Maybe Call)
updateCurrentScreen (InputState{keyEscPress = True}) s   = (s, Just ResumeGame)
updateCurrentScreen is (s@LevelSelect{}) 
      | keyRightPress is = (s{selectedButton = if ((sel + 1) < length menuButtons + 1) then sel + 1 else sel}, Nothing)
      | keyLeftPress  is = (s{selectedButton = if ((sel - 1) >= 0) then sel - 1 else sel}, Nothing)        
      | keyEnterPress is = if sel == 0 then (LevelOptionsSelect (LevelOptions (Left sel) SinglePlayer) s, Nothing)
                            else            (LevelOptionsSelect (LevelOptions (Right ("levels\\" ++ (menuButtons !! (sel - 1)) ++ ".txt" )) SinglePlayer ) s, Nothing)
      | otherwise        = ((updateDisplacement s), Nothing)
      where sel     = selectedButton s
            menuButtons = buttons s
updateCurrentScreen is s@(EndGameMessage m levelSelectScreen) 
      | keyEnterPress is = (levelSelectScreen, Nothing)
      | otherwise        = (s, Nothing)
updateCurrentScreen is s@(LevelOptionsSelect levelOptions levelSelectScreen)
      | keyLeftPress is  = (LevelOptionsSelect levelOptions{playOptions = cycleOptions (Left options)} levelSelectScreen, Nothing)
      | keyRightPress is = (LevelOptionsSelect levelOptions{playOptions = cycleOptions (Right options)} levelSelectScreen, Nothing)
      | keyEnterPress is = (levelSelectScreen, Just (StartLevel levelOptions))
      | otherwise        = (s, Nothing)
      where options = playOptions levelOptions
      
cycleOptions :: Either PlayOptions PlayOptions -> PlayOptions
cycleOptions (Left option) = case option of
      SinglePlayer -> Ai
      MultiPlayer  -> SinglePlayer
      Ai           -> MultiPlayer
cycleOptions (Right option) = case option of
      SinglePlayer -> MultiPlayer
      MultiPlayer  -> Ai
      Ai           -> SinglePlayer
      
                                                 
updateDisplacement :: Screen -> Screen
updateDisplacement ms = ms{displacement = dis + (if abs(dif) <= 0.01 then 0 else (dif / abs(dif) ) / 25)}
                                    where sel' = fromIntegral (selectedButton ms)
                                          dis  = displacement ms
                                          dif  = (sel' - dis)