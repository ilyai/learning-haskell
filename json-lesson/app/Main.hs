module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad
import Lib

data Book = Book { title :: T.Text
                 , author :: T.Text
                 , year :: Int
                 } deriving (Show,Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "Will Kurt"
              , title = "Learn Haskell"
              , year = 2017 }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

bookFromJSON :: Maybe Book
bookFromJSON = decode (myBookJSON)

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil\"}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

bookFromWrongJSON2 :: Either String Book
bookFromWrongJSON2 = eitherDecode wrongJSON

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\":123}"

data ErrorMessage = ErrorMessage { message :: T.Text
                                 , errorCode :: Int
                                 } deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) =
        ErrorMessage <$> v .: "message"
                     <*> v .: "error"

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) =
        object [ "message" .= message
               , "error" .= errorCode
               ]

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

data NOAAResult = NOAAResult
                  { uid :: T.Text
                  , mindate :: T.Text
                  , maxdate :: T.Text
                  , name :: T.Text
                  , datacoverage :: Int
                  , resultId :: T.Text
                  } deriving (Show,Generic)

instance ToJSON NOAAResult

instance FromJSON NOAAResult where
    parseJSON (Object v) =
        NOAAResult <$> v .: "uid"
                   <*> v .: "mindate"
                   <*> v .: "maxdate"
                   <*> v .: "name"
                   <*> v .: "datacoverage"
                   <*> v .: "id"

data Resultset = Resultset
                 { offset :: Int
                 , count :: Int
                 , limit :: Int
                 } deriving (Show,Generic)

instance FromJSON Resultset
instance ToJSON Resultset

data Metadata = Metadata
                { resultset :: Resultset
                } deriving (Show,Generic)

instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResponse = NOAAResponse
                    { metadata :: Metadata
                    , results :: [NOAAResult]
                    } deriving (Show,Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
    forM_ results (print . name)

printNoaaResponse :: Maybe NOAAResponse -> IO ()
printNoaaResponse Nothing = print "Error, no data"
printNoaaResponse (Just response) = print (encode response)

encodeMain :: IO ()
encodeMain = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    printNoaaResponse noaaResponse

main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse
    printResults noaaResults
