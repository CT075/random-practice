-- An implementation of /r/dailyprogrammer's problem spec on 2016-11-24
-- I tried the bonus for a bit, but meh

import Data.Char
import Data.Foldable

data Wire = White | Purple | Red | Green | Orange | Black deriving (Read)

cut :: Int -> Wire -> Maybe Int
cut 0 White = Just 1
cut 0 Red = Just 2
cut 1 White = Just 2
cut 1 Orange = Just 3
cut 2 Red = Just 0
cut 2 Black = Just 3
cut 3 Black = Just 3
cut 3 Orange = Just 4
cut 3 Green = Just 5
cut 4 Green = Nothing
cut 5 Orange = Nothing
cut _ _ = Just (-1)

out :: Maybe Int -> String
out Nothing = "Bomb defused"
out _ = "Boom!"

capitalize :: String -> String
capitalize (x:xs) = toUpper x:xs

main :: IO ()
main = interact $ out . foldlM cut 0 . map (read . capitalize) . words

