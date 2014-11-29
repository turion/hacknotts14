{-# LANGUAGE Arrows #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import System.Hardware.Arduino as Arduino
import FRP.Yampa as Yampa
import System.Hardware.Arduino.SamplePrograms.Blink

--main = withArduino True "/dev/ttyUSB0" $ do
--           let led = digital 13
--           setPinMode led OUTPUT
--           forever $ do digitalWrite led True
--                        Arduino.delay 1000
--                        digitalWrite led False
--                        Arduino.delay 1000

main = do
  input <- defaultArduinoInput
  output <- defaultArduinoOutput
  thrd <- arduinoThread input output
  reactimate (readMVar input)
             (\_ -> do i <- readMVar input
                       return $ (1, Just i))
             (\_ o -> do swapMVar output o
                         putStrLn $ "Yampa: " ++ show o
                         threadDelay 1000
                         return False)
             sf
       

sf = proc _ -> do
  t <- localTime -< ()
  let output = ArduinoOutput ((round t `div` timeToFlick) `mod` 2 == 0)
  returnA -< output
 where timeToFlick = 500 

data ArduinoInput = ArduinoInput
  { }
 deriving Show

data ArduinoOutput = ArduinoOutput
  { led13 :: Bool
  }
 deriving Show

defaultArduinoInput :: IO (MVar ArduinoInput)
defaultArduinoInput = newMVar ArduinoInput

defaultArduinoOutput :: IO (MVar ArduinoOutput)
defaultArduinoOutput = newMVar $ 
  ArduinoOutput { led13 = False }

type ArduinoIRef = MVar ArduinoInput
type ArduinoORef = MVar ArduinoOutput

arduinoThread :: ArduinoIRef -> ArduinoORef -> IO ThreadId
arduinoThread refI refO = forkIO $
  withArduino True "/dev/ttyUSB0" $ do
    let led_13 = digital 13
    setPinMode led_13 OUTPUT
    forever $ do
      output <- liftIO $ readMVar refO
      liftIO $ print output

      digitalWrite led_13 (led13 output)
      ---- Put outputs

      -- Read Inputs
      let input' = ArduinoInput
      
      void $ liftIO $ swapMVar refI input'
