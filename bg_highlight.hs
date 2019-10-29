{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Monoid

breakText :: T.Text
breakText = "არ"

fullText :: T.Text
fullText = "არა, მასწავლებელი არ არის."

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
    where highlighted = mconcat ["{",query,"}"]
          pieces = T.splitOn query fullText

main = do
    TIO.putStrLn (highlight breakText fullText)
