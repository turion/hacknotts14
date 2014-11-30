{-# LANGUAGE Arrows #-}
-- * This module enables communication with Parrot AR.Drones 2.0
module Parrot where

-- Ruthlessly stolen from Joe Nash: https://github.com/jdNash/haskopter/blob/master/src/Client.hs
-- Todo
-- Bind two ports, control and data
-- send 1 byte, send empty pac
-- continue sending
-- send two true commands to true data

import Network.Socket
import System.IO
import Control.Monad (forever, liftM, replicateM_)
import Control.Monad.Loops (whileM_)
import FRP.Yampa as Yampa
import Data.List (intercalate) 
import Unsafe.Coerce
import Data.Word

-- * Low level Communication protocol

-- | Raw Parrot commands
data ARRawCommand = Ref String | PCMD String | FTrim
	deriving Show

renderARRawCommand :: ARRawCommand -> Int -> String
renderARRawCommand (Ref s) i = "AT*REF=" ++ show i ++ "," ++ s
renderARRawCommand (PCMD s) i = "AT*PCMD=" ++ show i ++ ",1," ++ s
renderARRawCommand FTrim i = "AT*FTRIM=" ++ show i

-- * Higher level communication protocol

-- | Movement parrot commands
data ARCommand = 
	  Down
	| ToggleEmergency
	| Up
	| ARCh
	| ARCgh
	| ARCgf
	| ARCp
	| ARCpo
	| ARCyu
	| ARCyt
	| ARCrt
	| ARCre
	| ARCCustom Tristate Tristate Tristate Tristate -- might not work for yaw
	| Trim
	deriving Show

data Tristate = Zero | Plus | Minus
	deriving Show

rawCommand :: ARCommand -> ARRawCommand
rawCommand Down						= Ref "290717696"
rawCommand ToggleEmergency			= Ref "290717952"
rawCommand Up						= Ref "290718208"
rawCommand ARCh						= PCMD "0,0,0,0"
rawCommand ARCgh					= PCMD "0,0,1036831949,0"
rawCommand ARCgf					= PCMD "0,0,-1110651699,0"
rawCommand ARCp						= PCMD "1036831949,0,0,0"
rawCommand ARCpo					= PCMD "-1110651699,0,0,0"
rawCommand ARCyu					= PCMD "0,0,0,1056964608" -- ?
rawCommand ARCyt					= PCMD "0,0,0,-3204448256"
rawCommand ARCrt					= PCMD "0,1036831949,0,0"
rawCommand ARCre					= PCMD "0,-1110651699,0,0"
rawCommand (ARCCustom t1 t2 t3 t4)	= PCMD (tristrateString [t1, t2, t3, t4])
rawCommand Trim						= FTrim

tristrateString = (intercalate ",") . (map tristateToObscureNumber)

tristateToObscureNumber :: Tristate -> String
tristateToObscureNumber Zero  = "0"
tristateToObscureNumber Plus  = "1056964608"
tristateToObscureNumber Minus =  "-1090519040"
-- tristateToObscureNumber Plus  = "133169152" --"1036831949" -- Hopefully that's 1.0 instead of whatever small number
-- tristateToObscureNumber Minus = "-1082130432" --"-1110651699"
-- 	(float)0.05 = (int)1028443341		(float)-0.05 = (int)-1119040307
-- 	(float)0.1  = (int)1036831949		(float)-0.1  = (int)-1110651699
-- 	(float)0.2  = (int)1045220557		(float)-0.2  = (int)-1102263091
-- 	(float)0.5  = (int)1056964608		(float)-0.5  = (int)-1090519040

renderARCommand :: ARCommand -> Int -> String
renderARCommand command i = renderARRawCommand (rawCommand command) i

-- * Drones

-- | Running drone config
data ARDroneController = ARDroneController
  { ardroneSocket      :: Socket
  , ardroneHostAddress :: HostAddress
  , ardronePort        :: PortNumber
  }

-- | Initial drone config
data ARDroneConf = ARDroneConf { droneHost :: String
                               , dronePort :: PortNumber
                               }

defaultDroneConf :: ARDroneConf
defaultDroneConf = ARDroneConf { droneHost = "192.168.1.1"
                               , dronePort = 5556
                               }
-- Parrot IP and port
-- navport = 5554 -- unused
-- port = 5556


-- | Initialise drone. Sets configuration parameters, trims.
initDrone :: ARDroneConf -> IO ARDroneController
initDrone conf = do
  -- Open socket
  s <- socket AF_INET Datagram defaultProtocol
  hostAddr <- inet_addr (droneHost conf)
  let sockAdd = SockAddrInet (dronePort conf) hostAddr

  -- Create controller
  let controller = ARDroneController s hostAddr (dronePort conf)

  -- Send config and trim
  sendTo s "AT*CONFIG=1,\"control:altitude_max\",\"2000\"" sockAdd
  actCommand Trim controller 2

  return controller

-- | Print a debug message and send a command to the drone
actCommand :: ARCommand -> ARDroneController -> Int -> IO ()
actCommand command controller i = do
  putStrLn $ "Sending " ++ show command ++ " (" ++ (renderARCommand command i) ++ ")"
  arAction (renderARCommand command i) controller

-- | Send a *message* until there's input ready from the terminal
-- (old stop condition, a reminiscent from Joe's code, which was
-- terminal-based)
arActionFlood msg controller = whileM_ (liftM not $ hReady stdin) $ do
  arAction msg controller
  arAction msg controller

-- | Send a *message* to the drone.
-- NOTE: Currently sends it 5 times.
arAction msg (ARDroneController s hostAddr port) =
  replicateM_ 5 $ sendTo s (msg ++ "\r") (SockAddrInet port hostAddr)

-- Launch the drone
launch :: ARDroneController -> Int -> IO ()
launch controller i = do
  arAction (renderARCommand Trim i) controller
  arAction (renderARCommand Up (i+1)) controller

--main = withSocketsDo $ do -- need for windows
-- test = do
-- 	controller <- initDrone
-- 	mapM_ (consoleCommand controller) [3..] -- successive number
-- 	sClose $ ardroneSocket controller
-- 	return ()
--
-- consoleCommand :: ARDroneController -> Int -> IO ()
-- consoleCommand controller i = do
-- 	msg <- getLine
-- 	case lookup msg [
-- 			("d",	Down),
-- 			("e",	ToggleEmergency),
-- 			("u",	Up),
-- 			("h",	ARCh),
-- 			("gh",	ARCgh), -- gaz
-- 			("gf",	ARCgf),
-- 			("p",	ARCp),  -- pitch
-- 			("po",	ARCpo),
-- 			("yu",	ARCyu), -- yaw
-- 			("yt",	ARCyt),
-- 			("rt",	ARCrt), -- roll! ? rather pitch
-- 			("re",	ARCre),
-- 			("t",	Trim)
-- 		] of
-- 		Just command	-> actCommand command controller i
-- 		Nothing			-> putStrLn $ "Unrecognised input: " ++ msg

{-port = "5554"

main = withSocketsDo $ bracket getSocket sClose talk
        where getSocket = do
                (serveraddr:_) <- getAddrInfo Nothing (Just "127.0.1.1") (Just port)
                s <- socket (addrFamily serveraddr) Datagram defaultProtocol
                --bindSocket s (addrAddress serveraddr)
                connect s (addrAddress serveraddr) >> return s
              talk s = do
                send s "AT*CONFIG=1,\"control:altitude_max\",\"2000\""
                send s "AT*REF=101,290718208\r"
                recv s 1024 >>= \msg -> putStrLn $ "Received " ++ msg-}
{-
    do
        handle <- connectTo "localhost" (PortNumber 5556)
        input <- getContents
        sequence_ $ map (\a -> do
            hPutStr handle $ a ++ "\n"
            hFlush handle) $ lines input
        hClose handle
-}


{-
firstTwoButtonsToTristate :: [Bool] -> Tristate
firstTwoButtonsToTristate [b1,b2]	= buttonsToTristate (b1, b2)
firstTwoButtonsToTristate _			= error "Must be two bools for buttons"
-}
