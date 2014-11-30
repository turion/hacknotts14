module Arduino where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import System.Hardware.Arduino as Arduino


data ArduinoInput = ArduinoInput
  {  buttons :: [Bool] }
 deriving Show

data ArduinoOutput = ArduinoOutput
  { led13 :: Bool
  }
 deriving Show

defaultArduinoInput :: IO (MVar ArduinoInput)
defaultArduinoInput = newMVar $ ArduinoInput
  {  buttons = map (const False) buttonIndices }

buttonIndices = [2..12]

defaultArduinoOutput :: IO (MVar ArduinoOutput)
defaultArduinoOutput = newMVar $ 
  ArduinoOutput { led13 = False }

type ArduinoIRef = MVar ArduinoInput
type ArduinoORef = MVar ArduinoOutput

arduinoThread :: ArduinoIRef -> ArduinoORef -> IO ThreadId
arduinoThread refI refO = forkIO $
  withArduino True "/dev/ttyUSB0" $ do
    let led_13 = digital 13
        digButtons = map digital buttonIndices
    setPinMode led_13 OUTPUT
    mapM_ (`setPinMode` INPUT) digButtons
    forever $ do
      output <- liftIO $ readMVar refO
      -- liftIO $ print output

      digitalWrite led_13 (led13 output)
      ---- Put outputs

      -- Read Inputs
      bInputs <- mapM digitalRead digButtons
      let input' = ArduinoInput bInputs
      
      void $ liftIO $ swapMVar refI input'
