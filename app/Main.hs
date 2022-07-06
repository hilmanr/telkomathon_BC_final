module Main where

import System.IO
import Data.Maybe 
import Helper
import Model
import ADT


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
			let valinsCont = ValinsCont (initPortListCont 1) [] []
			putStrLn "Reading File ..."
			processValins valinsCont fileContent


-- Process Data Valins. Read dari file, create valins container, cek significant valins, cari unmapped SN, populate valins List



processValins :: ValinsCont -> Handle -> IO()
processValins vCont inH = do 
    ineof <- hIsEOF inH
    if ineof
    	then
    		do
	    		putStrLn "Processing Valins Done..."
	    		putStrLn "Creating Summary...\n\n"
	    		putStrLn "========================="
	    		putStrLn "ODP PORT LIST:"
	    		let portList = printValinsContPortList (getPortListCont vCont)
	    		putStrLn portList
	    		let sigValins = summaryValinsCount (getPortListCont vCont) (getValinsPortContVal vCont)
    			
    			putStrLn "========================="
    			putStrLn "Significant Valins:"
    			let maxTuple = findMaxTuple sigValins ("-",0)
    			putStrLn $ "Valins ID = " ++ (fst maxTuple)
    			putStrLn $ "SN Count = " ++ (show (snd maxTuple))

    			putStrLn "========================="
    			putStrLn "\nUnmapped SN:"
    			let unmappedList = printValinsContPortList (getUnmapPortListCont vCont)
    			putStrLn unmappedList
    	else
    		do 
    			-- First Line = separator
    			inputStr <- hGetLine inH
    			-- Second Line = Valins ID
    			valIDStr <- hGetLine inH
    			let valID = matchValinsID valIDStr

    			-- port 1
    			valPortStr <- hGetLine inH
    			let valPort1 = matchPortNum valPortStr
    			

    			-- port 2
    			valPortStr <- hGetLine inH
    			let valPort2 = matchPortNum valPortStr
    			

    			-- port 3
    			valPortStr <- hGetLine inH
    			let valPort3 = matchPortNum valPortStr
    			

    			-- port 4
    			valPortStr <- hGetLine inH
    			let valPort4 = matchPortNum valPortStr
    			

    			-- port 5
    			valPortStr <- hGetLine inH
    			let valPort5 = matchPortNum valPortStr
    			

    			-- port 6
    			valPortStr <- hGetLine inH
    			let valPort6 = matchPortNum valPortStr
    			

    			-- port 7
    			valPortStr <- hGetLine inH
    			let valPort7 = matchPortNum valPortStr
    			

    			-- port 8
    			valPortStr <- hGetLine inH
    			let valPort8 = matchPortNum valPortStr
    			
    			-- rekap port list
    			let valinsPortList = (valPort1:valPort2:valPort3:valPort4:valPort5:valPort6:valPort7:valPort8:[])

    			-- create new valins
    			let newValins = Valins (Just valID) valinsPortList
    			
    			let valinsContPortList = processValinsPortList valID valinsPortList (getPortListCont vCont)
    			
    			let unmappedList = processUnmappedPort valID valinsPortList (getPortListCont vCont)

    			let valinsList = (getValinsPortContVal vCont) ++ [(valID,0)]

    			let newValinsCont = ValinsCont valinsContPortList unmappedList valinsList

	    		processValins newValinsCont inH



-- Main
main :: IO ()
main = do
	putStrLn "----------------------------------------"
	putStrLn "----------- VALINS PROCESSOR -----------"
	putStrLn "----------------------------------------"
	let valins_current = Valins Nothing []
	writeFile "valins_list.txt" ""
	mainProgram valins_current "new_valins"


