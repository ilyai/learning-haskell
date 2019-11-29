module Lib
    ( isPalindrome
    ) where

import Data.Char(isPunctuation)
import Data.Text as T

preprocess :: T.Text -> T.Text
preprocess text = T.filter (not . isPunctuation) text
-- preprocess text = filter (not . (`elem` ['!','.'])) text

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text
