{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    file <- openFile filename ReadWriteMode
    input <- TIO.hGetContents file
    TIO.writeFile filename (T.toUpper input)
