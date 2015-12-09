module Arduino where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import System.Hardware.Arduino as Arduino

data ArduinoInput = ArduinoInput
  {  buttons :: [Bool] }
 deriving Show

buttonIndices = [2..8] ++ [10..12]

data ArduinoOutput = ArduinoOutput
  { led13 :: Bool
  , led9  :: Bool
  }
 deriving Show

defaultArduinoInput :: IO (MVar ArduinoInput)
defaultArduinoInput = newMVar $ ArduinoInput
  {  buttons = map (const False) buttonIndices }

defaultArduinoOutput :: IO (MVar ArduinoOutput)
defaultArduinoOutput = newMVar $ 
  ArduinoOutput { led13 = False, led9 = False }

type ArduinoIRef = MVar ArduinoInput
type ArduinoORef = MVar ArduinoOutput

-- | This thread connects to the arduino and continuously
-- polls the buttons and pushes the led values.
arduinoThread :: ArduinoIRef -> ArduinoORef -> IO ThreadId
arduinoThread refI refO = forkIO $
  withArduino False "/dev/ttyUSB0" $ do
    -- Inputs
    let led_13 = digital 13
        led_9  = digital 9
    setPinMode led_13 OUTPUT
    setPinMode led_9 OUTPUT

    -- Outputs
    let digButtons = map digital buttonIndices
    -- mapM_ (`setPinMode` INPUT) digButtons

    forever $ do
      output <- liftIO $ readMVar refO
      -- liftIO $ print output

      digitalWrite led_13 (led13 output)
      digitalWrite led_9 (led9 output)
      -- analogWrite led_9 (led9 output)
      ---- Put outputs

      -- Read Inputs
      -- bInputs <- mapM digitalRead digButtons
      -- let input' = ArduinoInput bInputs
      
      -- void $ liftIO $ swapMVar refI input'
      return ()
