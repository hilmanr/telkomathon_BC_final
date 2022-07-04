module Main where

import System.IO
import Data.Maybe 
import Helper
import Model


-- Main Program
mainProgram :: Valins -> String -> IO()

mainProgram valinsCurrent mode = do
	case mode of 
		"new_valins" -> do
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
					let newValins = Valins (Just userInput) [valinsPort]
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
						let newValins = Valins (currentValID) (appendValins valinsPort currentPortList)
						mainProgram newValins "current_valins"
				else 
					do 
						-- Write Valins to File
						-- Print ke layar hasil yang sudah diinputkan
						confirm <- getUserInput "Pengisian valins selesai, ingin menambahkan valins baru lagi? (Y/N)"
						case confirm of
							"Y" -> do
								putStrLn "FINISH"
								putStrLn (valinsToString valinsCurrent)
								appendFile "valins_list.txt" (valinsToString valinsCurrent)
								putStrLn "Write To File Done"
								let newValins = Valins Nothing []
								mainProgram (newValins) "new_valins"
							"N" -> do
								putStrLn "FINISH"
								putStrLn (valinsToString valinsCurrent)
								appendFile "valins_list.txt" (valinsToString valinsCurrent)
								putStrLn "Write To File Done"
								putStrLn "Bersiap untuk memproses valins"
								let newValins = Valins Nothing []
								mainProgram (newValins) "process_valins"
							_ -> do
								putStrLn "Input Salah, ingin menambahkan valins baru lagi? (Y/N)"
								mainProgram valinsCurrent "current_valins"
		"process_valins" -> do
			putStrLn "Memproses summary valins"
			fileContent <- openFile "valins_list.txt" ReadMode
			let valinsCont = ValinsCont [] [] Nothing []
			putStrLn "Reading File ..."
			processValins valinsCont valinsCurrent fileContent


-- Process Data Valins. Read dari file, create valins container, cek significant valins, cari unmapped SN, populate valins List



processValins :: ValinsCont -> Valins -> Handle -> IO ()
processValins vCont v inH = do 
    ineof <- hIsEOF inH
    if ineof
    	then 
    		putStrLn "N OF FILE"
    	else
    		do 
    			inputStr <- hGetLine inH
	    		case inputStr of
	    			"---" -> do
	    				putStrLn $ inputStr ++ " (IGNORE)"
	    			(['v']['a']['l']['i']['n']['s']['_']['i']['d']['=']:id) -> do
	    				putStrLn $ inputStr ++ " (FOUND VALINS ID)"
	    			_ -> do 
	    				putStrLn "End Of File"
	    		processValins vCont v inH
   --     if ineof
   --         then {-return ()-} putStrLn "END"
   --         else do 
   --         	inpStr <- hGetLine inH
			-- putStrLn inpStr
			-- 	   -- processValins vCont v inH



-- Main
main :: IO ()
main = do
	putStrLn "----------------------------------------"
	putStrLn "----------- VALINS PROCESSOR -----------"
	putStrLn "----------------------------------------"
	let valins_current = Valins Nothing []
	mainProgram valins_current "new_valins"


