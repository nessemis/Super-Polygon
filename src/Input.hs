module Input where

import InputModel
import Graphics.Gloss.Interface.IO.Game
import Model

--Nullifies any presses. Basically only allowing for a key to be pressed one frame
updateInputState :: InputState -> Caller InputState
updateInputState inputState = 
    let 
        call = if keyQPress inputState then Just QuitGame else Nothing
        updatedInputState = inputState {
            keyLeftPress  = False,
            keyRightPress = False,
            keyPausePress = False,
            keyEscPress   = False,
            keyEnterPress = False
        }
        in Caller updatedInputState call

inputKey :: Event -> InputState -> InputState
inputKey (EventKey (SpecialKey c) d _ _) iState
  = case c of
      KeyLeft  -> iState {keyLeft       = down,
                          keyLeftPress  = down}
      KeyRight -> iState {keyRight      = down,
                          keyRightPress = down}
      KeyEsc   -> iState {keyEscPress   = down}
      KeyEnter -> iState {keyEnterPress = down}
      _        -> iState
  where down = d == Down
      

--Check for normal keys
inputKey (EventKey (Char c) d _ _) iState
   = case c of
        'a' -> iState {keyA            = down}
        'd' -> iState {keyD            = down}
        'p' -> iState {keyPause        = down,
                       keyPausePress   = down}
        'q' -> iState {keyQPress       = down}                       
        _   -> iState 
    where down = d == Down
        
inputKey _ iState = iState -- Otherwise keep the same