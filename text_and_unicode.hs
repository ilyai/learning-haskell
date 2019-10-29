{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Text as T
import Data.Monoid

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

simpleInput :: T.Text
simpleInput = "this\nis\ninput"

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

-- combinedTextSemigroup :: T.Text
-- combinedTextSemigroup = "some" <> " " <> "text"

myLines :: T.Text -> [T.Text]
myLines s = T.splitOn "\n" s

myUnlines :: [T.Text] -> T.Text
myUnlines lines = T.intercalate "\n" lines
