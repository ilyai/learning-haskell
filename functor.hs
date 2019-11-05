import Data.Monoid
import Control.Applicative
import qualified Data.Map as Map

printInt :: Maybe String -> IO ()
printInt Nothing = putStrLn "value missing"
printInt (Just val) = putStrLn val

incMaybe :: Maybe Int -> Maybe Int      -- member of Functor type
incMaybe (Just n) = Just (n + 1)
incMaybe Nothing = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just s) = Just (reverse s)

-- instance Functor Maybe where
--     fmap func (Just n) = Just (func n)
--     fmap func Nothing = Nothing

successfullRequest :: Maybe Int
successfullRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

successStr :: Maybe String
successStr = fmap show successfullRequest

failStr :: Maybe String
failStr = fmap show failedRequest

data RobotPart = RobotPart { name :: String
                           , description :: String
                           , cost :: Double
                           , count :: Int } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
    { name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.0
    , count = 3 }

rightArm :: RobotPart
rightArm = RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5 }

robotHead :: RobotPart
robotHead = RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2 }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [
        "<h2>", partName,"</h2>"
       , "<p><h3>desc</h3>",partDesc
       , "</p><p><h3>cost</h3>"
       , partCost
       , "</p><p><h3>count</h3>"
       , partCount,"</p>" ]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)


partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keyVals = zip keys vals
          keys = [1,2,3]
          vals = [leftArm,rightArm,robotHead]

-- insertSnippet :: Maybe Html > IO ()

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts
-- allPartsHtml = map renderHtml allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

data Box a = Box a deriving Show

instance Functor Box where
    fmap func (Box v) = Box (func v)
