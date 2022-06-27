module Main where

import System.IO (hFlush, stdout)
import Data.Maybe 

-- Main Program
mainProgram :: ValinsCont -> Valins -> IO()

mainProgram valinsCont valinsCurrent mode = do
	case mode of 
		"new_valins" -> do
			putStrLn "\n\n\n----------- VALINS PROCESSOR -----------"
			putStrLn "----------------------------------------"
			userInput <- getUserInput "Masukkan ID Valins atau '-' untuk mengakhiri: "
			case userInput of
				"-" -> do
					putStrLn "Selesai"
				_ -> do
					portNum <- getUserInput "Masukkan Port Number: "
					portSN <- getUserInput "Masukkan SN: "
					maybePortSN <- portSN >>= (\x -> getPortSN x)
					let new_valins_port = createValinsPort (length valinsCurrent.portList) maybePortSN
					-- Disini ubah portSN dari IO String, menjadi Just "Serial numbernya" atau Nothing kalau kosong
					-- create ValinsPort
					-- masukkan ValinsPort ke Valins
					putStrLn $ portNum ++ portSN

		"current_valins" -> do
			putStrLn "Mengisi port dari sebuah valins"

		"process_valins" -> do 
			putStrLn "Proses terakhir valins"


getUserInput:: String -> IO String
getUserInput str = do
	putStr str
	hFlush stdout
	getLine

-- ADT
data ValinsPort = ValinsPort { portNum :: String  
                     , portSN :: Maybe String  
                     } deriving (Show)

data Valins = Valins {
				valinsID :: Maybe String,
				portList :: [ValinsPort],
				contributeCount :: Int
			} deriving (Show)

data ValinsCont = ValinsCont {
				portListCont :: [ValinsPort],
				unmappedSN :: [String],
				significantValins :: Maybe String,
				valinsList :: [Valins]
			} deriving (Show)

-- Function Constructor
createValins::Maybe String -> [ValinsPort] -> Valins
createValins (Just x) _ = Valins (Just x) [] 0
createValins Nothing _ = Valins Nothing [] 0

createValinsCont::ValinsCont
createValinsCont = ValinsCont [] [] Nothing []

createValinsPort::String -> Maybe String -> ValinsPort
createValinsPort port_num (Just sn) = ValinsPort port_num (Just sn)
createValinsPort port_num (Nothing) = ValinsPort port_num (Nothing)

-- Function Processing
getPortSN::String -> Maybe String
getPortSN x
	| x /= "-" = Just x
	| otherwise = Nothing

-- Main
main :: IO ()
main = do
	let valins_cont = createValinsCont
	let valins_current = createValins Nothing []
	mainProgram valins_cont valins_current


