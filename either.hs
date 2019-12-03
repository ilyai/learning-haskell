import Data.Maybe
import Control.Applicative
import Data.Char

myTakeUnsafe :: Int -> [a] -> [a]
myTakeUnsafe 0 _ = []
myTakeUnsafe n xs = head xs : myTakeUnsafe (n-1) (tail xs)

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

-- isPrime :: Int -> Maybe Bool
-- isPrime n
--     | n < 2 = Nothing
--     | n > maxN = Nothing
--     | otherwise = Just (n `elem` primes)

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
    show TooLarge = "Value exceed max bound"
    show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left InvalidValue
    | n > maxN = Left TooLarge
    | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It is prime"
displayResult (Right False) = "It is composite"
displayResult (Left primeError) = show primeError


-- data Either a b = Left a | Right b
-- data Either a b = Fail a | Correct b

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""


addStrInts :: String -> String -> Either String Int
addStrInts x y
    | not (all isDigit x) && not (all isDigit y) = Left "Neither value can be parsed"
    | not (all isDigit x) = Left "First value can't be parsed"
    | not (all isDigit y) = Left "Second value can't be parsed"
    | otherwise = Right ((read x) + (read y))

main :: IO ()
main = do
    print "Enter a number to test for primality:"
    n <- read <$> getLine
    let result = isPrime n
    print (displayResult result)
