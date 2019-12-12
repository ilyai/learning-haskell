module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import qualified Data.Text as T
import Lib

data Tool = Tool
    { toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }

data User = User
    { userId :: Int
    , userName :: String
    }

instance Show User where
    show user = mconcat [ show $ userId user
                        , ".) "
                        , userName user ]

instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                      , ".)"
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed: "
                      , show $ timesBorrowed tool
                      , "\n" ]

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

-- addUser :: String -> IO ()
-- addUser userName = do
--     conn <- open "tools.db"
--     execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
--     print "user added"
--     close conn

addUser :: String -> IO ()
addUser userName = withConn "tools.db" $
    \conn -> do
        execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
        print "user added"

addTool :: String -> String -> IO ()
addTool name description = withConn "tools.db" $
    \conn -> do
        execute conn "INSERT INTO tools (name,description) VALUES (?,?)" (name,description)
        print "tool added"

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
    \conn -> do
        execute conn "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)" (userId,toolId)

instance FromRow User where
    fromRow = User <$> field
                   <*> field

instance FromRow Tool where
    fromRow = Tool <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

printUsers :: IO ()
printUsers = withConn "tools.db" $
    \conn -> do
        resp <- query_ conn "SELECT * FROM users;" :: IO [User]
        mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
    \conn -> do
        resp <- query_ conn q :: IO [Tool]
        mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat [ "select * from tools "
                                          , "where id not in "
                                          , "(select tool_id from"
                                          , "checkedout);" ]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
    mconcat [ "select * from tools "
            , "where id in "
            , "(select tool_id from checkedout);" ]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn
            "SELECT * FROM tools WHERE id = (?)"
            (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    { lastReturned = date
    , timesBorrowed = 1 + timesBorrowed tool
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
    \conn -> do
        let q = mconcat [ "UPDATE tools SET "
                        , "lastReturned = ?, "
                        , "timesBorrowed = ? "
                        , "WHERE id = ?;" ]

        execute conn q (lastReturned tool
                       , timesBorrowed tool
                       , toolId tool)

        print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
    \conn -> do
        tool <- selectTool conn toolId
        currentDay <- utctDay <$> getCurrentTime
        let updatedTool = updateTool <$> tool
                                     <*> pure currentDay
        updateOrWarn updatedTool

instance ToRow Tool where
    toRow tool = [ SQLInteger $ fromIntegral $ toolId tool
                 , SQLText $ T.pack $ name tool
                 , SQLText $ T.pack $ description tool
                 , SQLText $ T.pack $ show $ lastReturned tool
                 , SQLInteger $ fromIntegral $ timesBorrowed tool ]

checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" $
    \conn -> do
        execute conn
            "DELETE FROM checkedout WHERE tool_id = (?);"
            (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
    print "Enter new user name"
    userName <- getLine
    addUser userName

promptAndAddTool :: IO ()
promptAndAddTool = do
    print "Enter the name of tool"
    toolName <- getLine
    print "Enter the description of tool"
    toolDescription <- getLine
    addTool toolName toolDescription

promptAndCheckout :: IO ()
promptAndCheckout = do
    print "Enter the id of the user"
    userId <- pure read <*> getLine
    print "Enter the id of the tool"
    toolId <- pure read <*> getLine
    checkout userId toolId

promptAndCheckin = do
    print "enter the id of tool"
    toolId <- pure read <*> getLine
    checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "addtool" = promptAndAddTool >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckin >> main
    | command == "in" = printAvailable >> main
    | command == "out" = printCheckedout >> main
    | command == "quit" = print "bye!"
    | otherwise = print "Sorry command not found" >> main

main :: IO ()
main = do
    print "Enter a command"
    command <- getLine
    performCommand command
