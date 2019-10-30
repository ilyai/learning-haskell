{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    let srcFile = head args
    let dstFile = head (tail args)
    input <- TIO.readFile srcFile
    TIO.writeFile dstFile input
