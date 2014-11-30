{-# LANGUAGE Arrows #-}
module Parrot where

{-
Ruthlessly stolen from Joe Nash: https://github.com/jdNash/haskopter/blob/master/src/Client.hs
-}

import Network.Socket
import System.IO
import Control.Monad (forever, liftM, replicateM_)
import Control.Monad.Loops (whileM_)
import FRP.Yampa as Yampa
import Data.List (intercalate) 

port = 5556
navport = 5554
host = "192.168.1.1"

{-0

Todo
Bind two ports, control and data
send 1 byte, send empty pac
continue sending
send two true commands to true data



-}


data ARRawCommand = Ref String | PCMD String | FTrim
	deriving Show

renderARRawCommand :: ARRawCommand -> Int -> String
renderARRawCommand (Ref s) i = "AT*REF=" ++ show i ++ "," ++ s
renderARRawCommand (PCMD s) i = "AT*PCMD=" ++ show i ++ ",1," ++ s
renderARRawCommand FTrim i = "AT*FTRIM=" ++ show i


data Tristate = Zero | Plus | Minus
	deriving Show
tristateToObscureNumber :: Tristate -> String
tristateToObscureNumber Zero = "0"
tristateToObscureNumber Plus = "1036831949"
tristateToObscureNumber Minus = "-1110651699"

tristrateString = (intercalate ",") . (map tristateToObscureNumber)

buttonsToTristate :: (Bool, Bool) -> Tristate
buttonsToTristate (False, False) = Zero
buttonsToTristate (True, True) = Zero
buttonsToTristate (True, False) = Plus
buttonsToTristate (False, True) = Minus

{-
firstTwoButtonsToTristate :: [Bool] -> Tristate
firstTwoButtonsToTristate [b1,b2]	= buttonsToTristate (b1, b2)
firstTwoButtonsToTristate _			= error "Must be two bools for buttons"
-}

fourButtonsToARCommand :: [Bool] -> ARCommand
fourButtonsToARCommand [b1,b2,b3,b4,b5,b6,b7,b8]	= ARCCustom (buttonsToTristate (b1, b2)) (buttonsToTristate (b3, b4)) (buttonsToTristate (b5, b6)) (buttonsToTristate (b7, b8))
fourButtonsToARCommand _							= error "Need 8 bools for 4 buttons"

elevenButtons (True:_)					= Down
elevenButtons (False:True:_)			= ToggleEmergency
elevenButtons (False:False:True:_)		= Up
elevenButtons (False:False:False:bs)	= fourButtonsToARCommand bs

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

renderARCommand :: ARCommand -> Int -> String
renderARCommand command i = renderARRawCommand (rawCommand command) i

actCommand :: ARCommand -> ARDroneController -> Int -> IO ()
actCommand command controller i = do
	putStrLn $ "Sending " ++ show command ++ " (" ++ (renderARCommand command i) ++ ")"
	arAction (renderARCommand command i) controller

consoleCommand :: ARDroneController -> Int -> IO ()
consoleCommand controller i = do
	msg <- getLine
	case lookup msg [
			("d",	Down),
			("e",	ToggleEmergency),
			("u",	Up),
			("h",	ARCh),
			("gh",	ARCgh), -- gaz
			("gf",	ARCgf),
			("p",	ARCp),  -- pitch
			("po",	ARCpo),
			("yu",	ARCyu), -- yaw
			("yt",	ARCyt),
			("rt",	ARCrt), -- roll! ? rather pitch
			("re",	ARCre),
			("t",	Trim)
		] of
		Just command	-> actCommand command controller i
		Nothing			-> putStrLn $ "Unrecognised input: " ++ msg

data ARDroneController = ARDroneController { ardroneSocket :: Socket, ardroneHostAddress :: HostAddress }

initDrone :: IO ARDroneController
initDrone = do
	s <- socket AF_INET Datagram defaultProtocol
	hostAddr <- inet_addr host
	sendTo s "AT*CONFIG=1,\"control:altitude_max\",\"2000\"" (SockAddrInet port hostAddr)
	let controller = ARDroneController s hostAddr
	actCommand Trim controller 2
	return controller

launch controller i = do
	arAction (renderARCommand Trim i) controller
	arAction (renderARCommand Up (i+1)) controller

--main = withSocketsDo $ do -- need for windows
test = do
	controller <- initDrone
	mapM_ (consoleCommand controller) [3..] -- successive number
	sClose $ ardroneSocket controller
	return ()

arActionFlood msg controller = whileM_ (liftM not $ hReady stdin) $ do
                        arAction msg controller
                        arAction msg controller
arAction msg (ARDroneController s hostAddr) = replicateM_ 5 $ sendTo s (msg ++ "\r") (SockAddrInet port hostAddr)

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
