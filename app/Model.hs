module Model where

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
				unmappedSN :: [String],
				significantValins :: Maybe String,
				valinsList :: [Valins]
			} deriving (Show)

-- Function Processing
getPortSN :: String -> Maybe String
getPortSN x
	| x /= "-" = Just x
	| otherwise = Nothing

getPortLength :: Valins -> Int
getPortLength (Valins _ portL ) = (length portL) + 1

getPortList :: Valins -> [ValinsPort]
getPortList (Valins _ portL ) = portL

getValinsID :: Valins -> Maybe String
getValinsID (Valins valID _ ) = valID

getValinsPortPNum :: ValinsPort -> Int
getValinsPortPNum (ValinsPort num _) = num

getValinsPortPSN :: ValinsPort -> Maybe String
getValinsPortPSN (ValinsPort _ sn) = sn


checkSignificant :: Maybe String -> Int
checkSignificant (Just x) = 1
checkSignificant Nothing = 0