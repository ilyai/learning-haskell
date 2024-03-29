import qualified Data.Map as Map
import Control.Applicative

type LatLong = (Double,Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [ ("Arkham",(42.6054,-70.7829))
                          , ("Innsmouth",(42.8250,-70.8150))
                          , ("Carcosa",(29.9714,-90.7694))
                          , ("New York", (40.7776,-73.9691)) ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
    where rlat = toRadians lat
          rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where (rlat1,rlong1) = latLongToRads coords1
          (rlat2,rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
          c = 2 * atan2 (sqrt a) (sqrt (1-a))
          earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO coords1 coords2 = do
    val1 <- coords1
    val2 <- coords2
    return (haversine val1 val2)

haversineIO2 :: IO LatLong -> IO LatLong -> IO Double
haversineIO2 coords1 coords2 = haversine <$> coords1 <*> coords2

maybeInc = (+) <$> Just 1 <*> Just 2

main :: IO ()
main = do
    putStrLn "Enter the starting city name:"
    startingInput <- getLine
    let startingCity = Map.lookup startingInput locationDB
    putStrLn "Enter the destination city name:"
    destInput <- getLine
    let destCity = Map.lookup destInput locationDB
    let distance = haversine <$> startingCity <*> destCity
    printDistance distance

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main3 :: IO ()
main3 = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")

data User = User
    { name :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

-- sue :: User
-- sue = User {name = "Sue", gamerId = 1337, score = 9001}

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

sue = User <$> serverUsername <*> serverGamerId <*> serverScore

mainGame :: IO ()
mainGame = do
    putStrLn "Enter a username, gamerId and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user
