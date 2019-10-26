import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

nameData :: Map.Map Int String
nameData = Map.fromList [(1,"Ilya")]

maybeMain :: Maybe String
maybeMain = do
    name <- Map.lookup 1 nameData
    let statement = helloPerson name
    return statement

fibMain :: IO ()
fibMain = do
    putStrLn "Please enter a number"
    n <- getLine
    let result = fib (read n)
    print result
