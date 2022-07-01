module Main where

import System.IO (hFlush, stdout)
import Data.Maybe 

-- Main Program
mainProgram :: Valins -> String -> IO()

mainProgram valinsCurrent mode = do
	case mode of 
		"new_valins" -> do
			putStrLn "\n\n\n----------- VALINS PROCESSOR -----------"
			putStrLn "----------------------------------------"
			userInput <- getUserInput "Masukkan ID Valins atau '-' untuk mengakhiri: "
			case userInput of
				"-" -> do
					putStrLn "Selesai"
				_ -> do
					let currentNum = getPortLength valinsCurrent
					putStrLn ("- Port  " ++ (show currentNum) ++ "-")
					portSN <- getUserInput "Masukkan SN: "
					let maybePortSN = getPortSN portSN
					
					-- let portNum = length currentList
					let valinsPort = ValinsPort currentNum maybePortSN
					let newValins = Valins (Just userInput) [valinsPort] (checkSignificant maybePortSN)
					let newValinsCont = ValinsCont [valinsPort] [] Nothing [newValins]
					mainProgram newValins "current_valins"
		"current_valins" -> do
			let currentNum = getPortLength valinsCurrent
			if (currentNum <= 8)
				then 
					do
						putStrLn ("- Port  " ++ (show currentNum) ++ "-")
						portSN <- getUserInput "Masukkan SN: "
						let currentPortList = getPortList valinsCurrent
						let currentValID = getValinsID valinsCurrent
						let maybePortSN = getPortSN portSN
						let valinsPort = ValinsPort currentNum maybePortSN
						let newValins = Valins (currentValID) (appendValins valinsPort currentPortList) (checkSignificant maybePortSN)
						mainProgram newValins "current_valins"
				else 
					do 
						putStrLn "FINISH"
						-- Write Valins to File
						-- Print ke layar hasil yang sudah diinputkan
						confirm <- getUserInput "Pengisian valins selesai, ingin menambahkan valins baru lagi?"
						case confirm of
							"Y" -> do
								let newValins = createValins Nothing []
								mainProgram (newValins) "new_valins"
							"N" -> do
								putStrLn "Bersiap untuk memproses valins"
		"process_valins" -> do
			putStrLn "Memproses summary valins"



getUserInput:: String -> IO String
getUserInput str = do
	putStr str
	hFlush stdout
	getLine

-- ADT
data ValinsPort = ValinsPort { portNum :: Int  
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

createValinsPort::Int -> Maybe String -> ValinsPort
createValinsPort port_num (Just sn) = ValinsPort port_num (Just sn)
createValinsPort port_num (Nothing) = ValinsPort port_num (Nothing)

-- Function Processing
getPortSN :: String -> Maybe String
getPortSN x
	| x /= "-" = Just x
	| otherwise = Nothing

getPortLength :: Valins -> Int
getPortLength (Valins _ portL _) = (length portL) + 1

getPortList :: Valins -> [ValinsPort]
getPortList (Valins _ portL _) = portL

getValinsID :: Valins -> Maybe String
getValinsID (Valins valID _ _) = valID

checkSignificant :: Maybe String -> Int
checkSignificant (Just x) = 1
checkSignificant Nothing = 0

appendValins:: ValinsPort -> [ValinsPort] -> [ValinsPort]
appendValins new [] = [new]
appendValins new (x:xs) = x : appendValins new xs

-- Main
main :: IO ()
main = do
	let valins_current = createValins Nothing []
	mainProgram valins_current "new_valins"


