import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [ (1,"nYarlaphoTep")
                          , (2,"KINGinYELLOW")
                          , (3, "dragon1997") ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [ ("nYarlaphoTep",2000)
                         , ("KINGinYELLOW",15000)
                         , ("dragon1997",300) ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

creditsFromId2 :: GamerId -> Maybe PlayerCredits
creditsFromId2 id = lookupUserName id >>= lookupCredits

creditsFromId3 :: GamerId -> Maybe PlayerCredits
creditsFromId3 id = Map.lookup id userNameDB >>= (flip Map.lookup) creditsDB 

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [ (1001,1)
                         , (1002,2)
                         , (1003,3) ]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

echo :: IO ()
echo = getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "What is your name?"

makeStatement :: String -> String
makeStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            return . makeStatement >>= -- (\name -> return (makeStatement name)) >>=
            putStrLn

allFMapM :: Monad m => (a -> b) -> m a -> m b
allFMapM f m = m >>= (\a -> return (f a))

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mc m = mc >>= (\f -> (m >>= (\a -> return (f a))))

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) f = f a

