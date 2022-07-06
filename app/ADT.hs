module ADT where

-- ADT
data ValinsPort = ValinsPort { 
					portNum :: Int,
                    portSN :: Maybe String
                     } deriving (Show)

data ValinsContPort = ValinsContPort { 
						contPortNum :: Int,
						contPortSN :: Maybe String,
						contValinsID :: Maybe String
                     } deriving (Show)

data Valins = Valins {
				valinsID :: Maybe String,
				portList :: [ValinsPort]
			} deriving (Show)

data ValinsCont = ValinsCont {
				portListCont :: [ValinsContPort],
				unmappedSN :: [ValinsContPort],
				valinsList :: [(String, Int)]
			} deriving (Show)

getPortSN :: String -> Maybe String
getPortSN x
	| x /= "-" = Just x
	| otherwise = Nothing

getPortLength :: Valins -> Int
getPortLength (Valins _ portL ) = (length portL) + 1

getPortList :: Valins -> [ValinsPort]
getPortList (Valins _ portL ) = portL

getPortListCont :: ValinsCont -> [ValinsContPort]
getPortListCont (ValinsCont portL _ _ ) = portL

getUnmapPortListCont :: ValinsCont -> [ValinsContPort]
getUnmapPortListCont (ValinsCont _ unmapL _ ) = unmapL

getValinsID :: Valins -> Maybe String
getValinsID (Valins valID _ ) = valID

getValinsPortPNum :: ValinsPort -> Int
getValinsPortPNum (ValinsPort num _) = num

getValinsPortPSN :: ValinsPort -> Maybe String
getValinsPortPSN (ValinsPort _ sn) = sn

getValinsPortContPSN :: ValinsContPort -> Maybe String
getValinsPortContPSN (ValinsContPort _ sn _ ) = sn

getValinsPortContPNum :: ValinsContPort -> Int
getValinsPortContPNum (ValinsContPort n _ _ ) = n

getValinsPortContPValID :: ValinsContPort -> Maybe String
getValinsPortContPValID (ValinsContPort _ _ valID ) = valID

getValinsPortContVal :: ValinsCont -> [(String,Int)]
getValinsPortContVal (ValinsCont _ _ valList) = valList