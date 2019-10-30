import System.IO

-- openFile :: FilePath -> IOMode -> IO Handle
-- type FilePath String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    isFirstEOF <- hIsEOF helloFile
    firstLine <- if not isFirstEOF
                 then hGetLine helloFile
                 else return "empty"
    putStrLn firstLine
    isSecondEOF <- hIsEOF helloFile
    secondLine <- if not isSecondEOF
                  then hGetLine helloFile
                  else return ""
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine
    hClose helloFile
    hClose goodbyeFile
    putStrLn "done!"
