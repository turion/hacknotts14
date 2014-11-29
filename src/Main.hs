import Control.Monad
import System.Hardware.Arduino

main = withArduino False "/dev/ttyUSB0" $ do
           let led = digital 13
           setPinMode led OUTPUT
           forever $ do digitalWrite led True
                        delay 1000
                        digitalWrite led False
                        delay 1000 

data ArduinoInput = ArduinoInput {}

data ArduinoOutput = ArduinoOutput
  { led13 :: Bool
  }


