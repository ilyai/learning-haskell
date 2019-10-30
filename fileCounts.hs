import Data.Monoid
import System.IO
import System.Environment

getCounts :: String -> (Int,Int,Int)
getCounts input = (charCount,wordCount,lineCount)
    where charCount = length input
          wordCount = (length . words) input
          lineCount = (length . lines) input

countText :: (Int,Int,Int) -> String
countText (cc,wc,lc) = unwords [ "chars: "
                               , show cc
                               , " words: "
                               , show wc
                               , " lines:"
                               , show lc ]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    -- input <- readFile fileName
    file <- openFile fileName ReadMode
    input <- hGetContents file
    let summary = (countText . getCounts) input
    putStrLn summary
    hClose file
    appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
