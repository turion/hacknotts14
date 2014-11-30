{-# LANGUAGE Arrows #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import FRP.Yampa as Yampa
import Arduino
import Parrot
import Data.IORef
import System.IO

main = do
  input  <- defaultArduinoInput
  output <- defaultArduinoOutput
  thrd   <- arduinoThread input output
  putStrLn "Initializing drone..."
  drone  <- initDrone
  putStrLn "Launching..."
  launch drone 3
  counter<- newIORef 5
  reactimate (readMVar input)
             (\_ -> do i <- readMVar input
                       putStrLn $ "Yampa input: " ++ show i
                       emerg <- hReady stdin
                       when emerg $ do
                         putStrLn "Emergency shutdown" 
                         emergencyShutdown drone counter
                       return $ (1, Just i))
             (\_ (i, o) -> do swapMVar output o
                              -- putStrLn $ "Yampa output: " ++ show o
                              sendDroneCommands drone counter i
                              threadDelay 1000
                              return False)
             sf
       

sf = proc i -> do
  t <- localTime -< ()
  let output = ArduinoOutput ((round t `div` timeToFlick) `mod` 2 == 0)
  returnA -< (i, output)
 where timeToFlick = 500 

sendDroneCommands drone counter i = do
    n <- readIORef counter
    let command = elevenButtons $ buttons i
    putStrLn $ "Sending " ++ (renderARCommand command n) ++ " to the drone"
    arAction (renderARCommand command n) drone
    writeIORef counter $ n+1

emergencyShutdown drone counter = do
	n <- readIORef counter
	arActionFlood (renderARCommand Down n) drone
	writeIORef counter $ n+1

