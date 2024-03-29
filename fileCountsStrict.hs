{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getCounts :: T.Text -> (Int,Int,Int)
getCounts input = (charCount,wordCount,lineCount)
    where charCount = T.length input
          wordCount = (length . T.words) input
          lineCount = (length . T.lines) input

countText :: (Int,Int,Int) -> T.Text
countText (cc,wc,lc) = T.pack $ unwords [ "chars: "
                                        , show cc
                                        , " words: "
                                        , show wc
                                        , " lines:"
                                        , show lc ]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TIO.readFile fileName
    let summary = (countText . getCounts) input
    TIO.appendFile "stats.dat" (mconcat [(T.pack fileName), " ", summary, "\n"])
    TIO.putStrLn summary
