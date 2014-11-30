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
  -- Initialise arduino
  input  <- defaultArduinoInput
  output <- defaultArduinoOutput
  thrd   <- arduinoThread input output

  -- Intialise and launch drone
  putStrLn "Initializing drone..."
  drone  <- initDrone defaultDroneConf
  putStrLn "Launching..."
  launch drone 3
  counter <- newIORef 5 -- Shit!

  reactimate (readMVar input)
             (\_ -> do i <- readMVar input
                       putStrLn $ "Yampa input: " ++ show i
                       -- Secondary emergency shutdown
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

-- FRP Signal function. Does something pretty basic, but
-- you can complicate is as much as you want.
sf = proc i -> do
  t <- localTime -< ()
  let output = ArduinoOutput ((round t `div` timeToFlick) `mod` 2 == 0)
  returnA -< (i, output)
 where timeToFlick = 500 

-- | Send drone commands keeping a counter.
sendDroneCommands drone counter i = do
    n <- readIORef counter
    let command = elevenButtons $ buttons i
    putStrLn $ "Sending " ++ (renderARCommand command n) ++ " to the drone"
    arAction (renderARCommand command n) drone
    writeIORef counter $ n+1

-- | Emergency shutdown using the fucking counter.
emergencyShutdown drone counter = do
	n <- readIORef counter
	arActionFlood (renderARCommand Down n) drone
	writeIORef counter $ n+1

-- Precondition: There are exactly 11 buttons. May fail to pattern match of
-- there are more or less
elevenButtons :: [Bool] -> ARCommand
elevenButtons (True:_)					= Down
elevenButtons (False:True:_)			= ToggleEmergency
elevenButtons (False:False:True:_)		= Up
elevenButtons (False:False:False:bs)	= fourButtonsToARCommand bs

fourButtonsToARCommand :: [Bool] -> ARCommand
fourButtonsToARCommand [b1,b2,b3,b4,b5,b6,b7,b8] =
  ARCCustom (buttonsToTristate (b1, b2))
            (buttonsToTristate (b3, b4))
            (buttonsToTristate (b5, b6))
            (buttonsToTristate (b7, b8))
fourButtonsToARCommand _ = error "Need 8 bools for 4 buttons"

buttonsToTristate :: (Bool, Bool) -> Tristate
buttonsToTristate (False, False) = Zero
buttonsToTristate (True, True) = Zero
buttonsToTristate (True, False) = Plus
buttonsToTristate (False, True) = Minus
