{-# LANGUAGE Arrows #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import System.Hardware.Arduino as Arduino
import FRP.Yampa as Yampa

main = do
  input  <- defaultArduinoInput
  output <- defaultArduinoOutput
  thrd   <- arduinoThread input output
  reactimate (readMVar input)
             (\_ -> do i <- readMVar input
                       putStrLn $ "Yampa input: " ++ show i
                       return $ (1, Just i))
             (\_ o -> do swapMVar output o
                         -- putStrLn $ "Yampa output: " ++ show o
                         threadDelay 1000
                         return False)
             sf
       

sf = proc _ -> do
  t <- localTime -< ()
  let output = ArduinoOutput ((round t `div` timeToFlick) `mod` 2 == 0)
  returnA -< output
 where timeToFlick = 500 

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

buttonIndices = [2..10]

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
