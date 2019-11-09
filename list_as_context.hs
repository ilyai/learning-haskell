import Control.Applicative

hello :: IO String
hello = pure "Hello World"

data Blah a b = Blah a b        -- container, not context

data ResourceContrained a = NoResources | Okay a        -- context

doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [500,20000]

-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize

-- totalPrize :: [Int]
-- totalPrize = (pure +) <*> doorPrize <*> boxPrize

boxMultiplier = [10,50]
newOutcomes = pure (*) <*> doorPrize <*> boxMultiplier

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where twoThroughN = [2 .. n]
          composite = pure (*) <*> twoThroughN <*> twoThroughN
          isNotComposite = not . (`elem` composite)

data User = User {
    name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show

testNames :: [String]
testNames = [ "John Smith"
            , "Robert'; DROP TABLE Students; --"
            , "Christina NULL"
            , "Randall Munroe"
            , "Will Kurt" ]

testIds :: [Int]
testIds = [1337,0123,999999]

testScores :: [Int]
testScores = [0,100000,-99999]

testData :: [User]
testData = pure User <*> testNames
                     <*> testIds
                     <*> testScores

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f x = (pure f) <*> x

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure ((*) ((+) 2 4) 6)
