{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import FRP.Yampa as Yampa
import Arduino
import Data.IORef
import System.IO

main = do
  -- Initialise arduino
  input  <- defaultArduinoInput
  output <- defaultArduinoOutput
  thrd   <- arduinoThread input output

  reactimate (readMVar input)
             (\_ -> do i <- readMVar input
                       -- putStrLn $ "Yampa input: " ++ show i
                       return $ (1, Just i))
             (\_ (i, o) -> do swapMVar output o
                              -- putStrLn $ "Yampa output: " ++ show o
                              threadDelay 10
                              return False)
             sf

sf = proc i -> do
  s0 <- fastPFW 200 400 -< ()
  s1 <- fastPFW 3   5   -< ()
  let output = ArduinoOutput s0 s1
  returnA -< (i, output)

fastPFW n mx = 
 arr ((< n).(`mod` mx).round)  <<< localTime
  -- proc i -> do
  -- t <- round ^<< localTime -< ()
  -- let v = t `mod` mx < n
  -- returnA -< v
