{-# LANGUAGE Arrows #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import FRP.Yampa as Yampa
import Arduino
import Parrot

main = do
  input  <- defaultArduinoInput
  output <- defaultArduinoOutput
  thrd   <- arduinoThread input output
  drone  <- initDrone
  launch drone 3
  counter<- newMVar 5
  reactimate (readMVar input)
             (\_ -> do i <- readMVar input
                       putStrLn $ "Yampa input: " ++ show i
                       return $ (1, Just i))
             (\_ (i, o) -> do swapMVar output o
                              -- putStrLn $ "Yampa output: " ++ show o
                              sendDroneCommands drone counter i
                              --threadDelay 1000
                              return False)
             sf
       

sf = proc i -> do
  t <- localTime -< ()
  let output = ArduinoOutput ((round t `div` timeToFlick) `mod` 2 == 0)
  returnA -< (i, output)
 where timeToFlick = 500 

sendDroneCommands drone counter i = do
	n <- takeMVar counter
	let command = nineButtons $ buttons i
	arAction (renderARCommand command n) drone
	putMVar counter $ n+1
