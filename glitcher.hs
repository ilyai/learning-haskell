import Data.Monoid
import Control.Monad
import System.Random
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before,newChar,after]
    where (before,rest) = BC.splitAt loc bytes
          after = BC.drop 1 rest
          newChar = intToBC charVal

randromChar :: IO Char
randromChar = do
    randomInt <- randomRIO (0,255)      -- could also use max and min bound
    return (toEnum randomInt)

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1,bytesLength)
    charVal <- randomRIO (0,255)
    return (replaceByte location charVal bytes)

type Transformer = (BC.ByteString -> BC.ByteString)

transformSection :: Transformer -> Int -> Int -> BC.ByteString -> BC.ByteString
transformSection trans start size bytes = mconcat [before,changed,after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          -- changed = BC.reverse (BC.sort target)
          changed = trans target

randomTransformSection :: Transformer -> BC.ByteString -> IO BC.ByteString
randomTransformSection trans bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0,bytesLength - sectionSize)
    return (transformSection trans start sectionSize bytes)

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    -- glitched <- randomReplaceByte imageFile
    -- glitched <- randomSortSection imageFile
    glitched <- foldM (\bytes func -> func bytes) imageFile
        [ randomReplaceByte
        , randomTransformSection BC.sort
        , randomReplaceByte
        , randomTransformSection BC.reverse
        , randomReplaceByte ]
    let glitchedFileName = mconcat ["glitched_",fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"
