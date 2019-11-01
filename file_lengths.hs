-- {-# LANGUAGE OverloadedStrings #-}

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

-- String
-- B.ByteString
-- BC.ByteString
-- T.Text

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    fileS <- readFile fileName
    fileB <- B.readFile fileName
    fileBC <- BC.readFile fileName
    fileT <- TIO.readFile fileName
    let lenS = length fileS
    let lenB = B.length fileB
    let lenBC = BC.length fileBC
    let lenT = T.length fileT
    putStrLn $ "S:" ++ show lenS ++
               " B:" ++ show lenB ++
               " BC:" ++ show lenBC ++
               " T:" ++ show lenT
