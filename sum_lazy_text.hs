import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

toInts :: T.Text -> [Int]
toInts = map (\s -> read (T.unpack s)) . T.lines

main :: IO ()
main = do
    userInput <- TIO.getContents
    let numbers = toInts userInput
    print (sum numbers)
