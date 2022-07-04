module Helper where

import System.IO (hFlush, stdout)
import Model

maybeToString :: Maybe String -> String
maybeToString (Just x) = x
maybeToString Nothing = ""

appendValins:: ValinsPort -> [ValinsPort] -> [ValinsPort]
appendValins new [] = [new]
appendValins new (x:xs) = x : appendValins new xs

valinsToString:: Valins -> String
valinsToString v = "---\n" ++ "valins_id=" ++ (maybeToString $ getValinsID v) ++ "\n" ++ (valinsPortListToString $ getPortList v)

valinsPortListToString :: [ValinsPort] -> String
valinsPortListToString [] = ""
valinsPortListToString (x:xs) = "PortNum=" ++ (show $ getValinsPortPNum x) ++ "|" ++ (maybeToString $ getValinsPortPSN x) ++ "\n" ++ (valinsPortListToString xs)

getUserInput:: String -> IO String
getUserInput str = do
	putStr str
	hFlush stdout
	getLine