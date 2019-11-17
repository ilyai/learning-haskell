import Control.Monad

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2^value)

powersOfTwo2 :: Int -> [Int]
powersOfTwo2 n = map (\x -> 2^x) [1 .. n]

powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2 ^ value
    let powersOfThree = 3 ^ value
    return (powersOfTwo,powersOfThree)

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue <- [1,3 .. n]
    return (evenValue,oddValue)

valAndSquare :: [(Integer,Integer)]
valAndSquare = do
    val <- [1 .. 10]
    return (val, val^2)

-- evensGuard :: Int -> [Int]
evensGuard = do
    value <- [1 .. 10]
    guard (even value)
    return value

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = do
    x <- xs
    guard (f x)
    return x
