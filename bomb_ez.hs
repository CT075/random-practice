-- An implementation of /r/dailyprogrammer's problem spec on 2016-11-21

import Data.Char
import Data.Foldable

data Wire = White | Purple | Red | Green | Orange | Black deriving (Read)

capitalize :: String -> String
capitalize (x:xs) = toUpper x:xs

next :: Wire -> Wire -> Maybe Wire
next White  Red    = Just Red
next White  Green  = Just Green
next White  Orange = Just Orange
next White  Purple = Just Purple
next Purple Red    = Just Red
next Purple Black  = Just Black
next Red    Green  = Just Green
next Green  Orange = Just Orange
next Green  White  = Just White
next Orange Red    = Just Red
next Orange Black  = Just Black
next Black  Black  = Just Black
next Black  Red    = Just Red
next Black  Purple = Just Purple
next _      _      = Nothing

out :: Maybe Wire -> String
out Nothing = "Boom!"
out _       = "Bomb defused!"

solve :: [Wire] -> String
solve (x:xs) = out $ foldlM next x xs

main :: IO ()
main = interact $ solve . map (read . capitalize) . words

