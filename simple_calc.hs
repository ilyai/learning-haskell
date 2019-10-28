import Data.List.Split

evaluateExpr s = if length parts == 2
                 then read (head parts) + read (head (tail parts))
                 else 0
    where parts = splitOn "+" s

-- toInts :: String -> [String]
compute = map evaluateExpr . lines

main :: IO ()
main = do
    userInput <- getContents
    let numbers = compute userInput
    print (numbers)
