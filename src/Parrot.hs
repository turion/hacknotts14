module Main where

{-
Ruthlessly stolen from Joe Nash: https://github.com/jdNash/haskopter/blob/master/src/Client.hs
-}

import Network.Socket
import System.IO
import Control.Monad (forever, liftM)
import Control.Monad.Loops (whileM_)

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


data ARRawCommand = Ref String | PCMD String
	deriving Show

renderARRawCommand :: ARRawCommand -> Int -> String
renderARRawCommand (Ref s) i = "AT*REF=" ++ show i ++ "," ++ s ++ "\r"
renderARRawCommand (PCMD s) i = "AT*PCMD=" ++ show i ++ "," ++ s ++ "\r"

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
	deriving Show


rawCommand :: ARCommand -> ARRawCommand
rawCommand Down				= Ref "290717696"
rawCommand ToggleEmergency	= Ref "290717952"
rawCommand Up				= Ref "290718208"
rawCommand ARCh				= PCMD "1,0,0,0,0"
rawCommand ARCgh			= PCMD "1,0,0,1036831949,0"
rawCommand ARCgf			= PCMD "1,0,0,-1110651699,0"
rawCommand ARCp				= PCMD "1,1036831949,0,0,0"
rawCommand ARCpo			= PCMD "1,-1110651699,0,0,0"
rawCommand ARCyu			= PCMD "1,0,0,0,1056964608"
rawCommand ARCyt			= PCMD "1,0,0,0,-3204448256"
rawCommand ARCrt			= PCMD "1,0,1036831949,0,0"
rawCommand ARCre			= PCMD "1,0,-1110651699,0,0"

renderARCommand :: ARCommand -> Int -> String
renderARCommand command i = renderARRawCommand (rawCommand command) i

actCommand :: ARCommand -> Socket -> HostAddress -> Int -> IO ()
actCommand command s h i = do
	putStrLn $ "Sending " ++ show command ++ " (" ++ (renderARCommand command i) ++ ")"
	arAction (renderARCommand command i) s h

consoleCommand :: Socket -> HostAddress ->Int -> IO ()
consoleCommand s h i = do
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
			("rt",	ARCrt), -- roll!
			("re",	ARCre)
		] of
		Just command	-> actCommand command s h i
		Nothing			-> putStrLn $ "Unrecognised input: " ++ msg


main = withSocketsDo $ do
	s <- socket AF_INET Datagram defaultProtocol
	hostAddr <- inet_addr host
	sendTo s "AT*CONFIG=1,\"control:altitude_max\",\"2000\"" (SockAddrInet port hostAddr)
	sendTo s "AT*CONFIG=1,\"control:altitude_max\",\"2000\"" (SockAddrInet port hostAddr)  
	mapM_ (consoleCommand s hostAddr) [1..]
	sClose s
	return ()

arAction msg s hostAddr = whileM_ (liftM not $ hReady stdin) $ do
                        sendTo s msg (SockAddrInet port hostAddr)
                        sendTo s msg (SockAddrInet port hostAddr)

--arMessage msg ref

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
