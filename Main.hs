module Main where

import qualified Data.Text as T
import qualified Palindrome as P

main :: IO ()
main = do
    print "Enter a word and I'll let you know if it's a palindrome!"
    text <- getLine
    let response = if P.isPalindrome (T.pack text)
                   then "it is!"
                   else "it's not!"
    print response
