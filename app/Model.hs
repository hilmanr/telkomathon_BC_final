module Model where

import ADT
import Helper

initPortListCont :: Int -> [ValinsContPort]
initPortListCont n
    | n <= 8 = (ValinsContPort n Nothing Nothing) : (initPortListCont (n+1))
    | otherwise = []

-- Function Processing

checkSignificant :: Maybe String -> Int
checkSignificant (Just x) = 1
checkSignificant Nothing = 0

matchValinsID :: String -> String
matchValinsID ('v':'a':'l':'i':'n':'s':'_':'i':'d':'=':xs) = xs
matchValinsID (x:xs) = ""


matchPortNum :: String -> ValinsPort
matchPortNum ('P':'o':'r':'t':'N':'u':'m':'=':xs) = matchSN xs

matchSN :: String -> ValinsPort
matchSN (x:'|':xs) = ValinsPort (read (x:[]) :: Int) (getPortSN xs)

processValinsPortList :: String -> [ValinsPort] -> [ValinsContPort] -> [ValinsContPort]
processValinsPortList valID (x:xs) (y:ys) = (compareValinsPort valID x y) : (processValinsPortList valID xs ys)
processValinsPortList valID [] [] = []

compareValinsPort ::String -> ValinsPort -> ValinsContPort -> ValinsContPort
compareValinsPort valID x y = 
    if ( (checkNotEqualSN x y) && (not ((getValinsPortPSN x) == Nothing)) && ((getValinsPortContPSN y) == Nothing))
	    then 
			(ValinsContPort (getValinsPortContPNum y) (getValinsPortPSN x) (Just valID))
		else 
			(ValinsContPort (getValinsPortContPNum y) (getValinsPortContPSN y) (getValinsPortContPValID y))

checkNotEqualSN :: ValinsPort -> ValinsContPort -> Bool
checkNotEqualSN x y = if ((maybeToString (getValinsPortPSN x)) == (maybeToString (getValinsPortContPSN y))) then False else True


processUnmappedPort :: String -> [ValinsPort] -> [ValinsContPort] -> [ValinsContPort]
processUnmappedPort valID (x:xs) (y:ys) = (compareUnmappedPort valID x y) ++ (processUnmappedPort valID xs ys)
processUnmappedPort valID [] [] = []

compareUnmappedPort ::String -> ValinsPort -> ValinsContPort -> [ValinsContPort]
compareUnmappedPort valID x y = 
    if ( (checkNotEqualSN x y) && (not ((getValinsPortPSN x) == Nothing)) && ((getValinsPortContPSN y) == Nothing))
	    then 
			[]
		else 
			if (not ((getValinsPortPSN x) == Nothing))
				then [(ValinsContPort (getValinsPortPNum x) (getValinsPortPSN x) (Just valID))]
				else []

summaryValinsCount :: [ValinsContPort] -> [(String, Int)] -> [(String, Int)]
summaryValinsCount (x:xs) tupleL = 
	let 
		valID = maybeToString (getValinsPortContPValID x)
	in summaryValinsCount xs (updateValinsTuple valID tupleL)

summaryValinsCount [x] tupleL =
	let 
		valID = maybeToString (getValinsPortContPValID x)
	in updateValinsTuple valID tupleL

summaryValinsCount [] tupleL = tupleL


updateValinsTuple :: String -> [(String, Int)] -> [(String, Int)]
updateValinsTuple valID (x:xs) = if (valID == (fst x)) 
        then ((fst x,((snd x)+1)) : (updateValinsTuple valID xs)) 
        else ((fst x, snd x) : (updateValinsTuple valID xs))
updateValinsTuple valID [] = []

printValinsContPortList :: [ValinsContPort] -> String
printValinsContPortList (x:xs) = "Port Num = " ++ (show (getValinsPortContPNum x)) ++ " | " ++ "SN = " ++ (maybeToString (getValinsPortContPSN x)) ++ " (Valins " ++ (maybeToString (getValinsPortContPValID x)) ++ ")\n" ++ (printValinsContPortList xs)
printValinsContPortList [] = ""



findMaxTuple :: [(String, Int)] -> (String, Int) -> (String, Int)
findMaxTuple (x:xs) localMax =
	let 
		maxT = findMaxTuple xs localMax
	in if ((snd x) >= (snd maxT)) then x else maxT

findMaxTuple [x] localMax = if ((snd x) >= (snd localMax)) then x else localMax
findMaxTuple [] _ = ("-",0)