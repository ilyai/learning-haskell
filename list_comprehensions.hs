import Control.Monad
import Data.Char

evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n^2
    guard (even nSquared)
    return nSquared

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2^value)

powersOfTwoC :: Int -> [Int]
powersOfTwoC n = [2^value | value <- [1 .. n]]

powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n = [ (powersOfTwo, powersOfThree)
                        | value <- [1 .. n]
                        , let powersOfTwo = 2^value
                        , let powersOfThree = 3^value ]

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = [(evenValue,oddValue) | evenValue <- [2,4 .. n]
                                      , oddValue <- [1,3 .. n]]

evensGuard :: Int -> [Int]
evensGuard n = [ value | value <- [1 .. n], even value ]

mrWords :: [String] -> [String]
mrWords words = [ w
                | word <- words
                , let w = "Mr. " ++ [toUpper (head word)] ++ (tail word) ]

mrWords2 :: [String]
mrWords2 = [ "Mr. " ++ capVal
           | val <- ["brown","blue","pink","orange","white"]
           , let capVal = (\(x:xs) -> toUpper x:xs) val ]

calendarDates :: Int -> [Int]
calendarDates month = [ day
                      | day <- [1 .. 31]
                      , month /= 2 || day < 29 ]

calendarDatesDo :: Int -> [Int]
calendarDatesDo month = do
    day <- [1 .. 31]
    guard (month /= 2 || day < 29)
    return day

calendarDatesMonad :: Int -> [Int]
calendarDatesMonad month =
    [1 .. 31] >>=
    (\day -> guard (month /= 2 || day < 29) >> return day)

