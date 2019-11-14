askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            return . nameStatement >>=
            putStrLn

maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM m = m >>= (\(x,y) -> return (max x y))

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

mainM :: IO ()
mainM = getLine >>=
        return . helloPerson >>=
        putStrLn

echo :: IO ()
echo = do
    input <- getLine
    putStrLn input
