import System.Environment
import Control.Monad

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n act = mapM (\_ -> act) [1 .. n]

main :: IO ()
main = do
    args <- getArgs
    -- lines <- mapM (\_ -> getLine) [1..3]
    -- mapM_ putStrLn lines
    -- mapM_ putStrLn args
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    numbers <- myReplicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)
