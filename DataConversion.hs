------------------------------------------------------------------------------
-- | This module does data-type conversions and general data manipulations.
--   See LICENSE file for copyright and license info.
------------------------------------------------------------------------------
module DataConversion where

--||| Imports
import Data.Default.Class
import Control.Lens hiding (argument)
import Data.List.Split
import Data.List
import Control.Monad (when)
import System.Exit (exitSuccess)

-- ||| Data conversion methods
-- | Parses a list of ;-separated string to a list of strings
-- | Example: ["12;10", "15;5"]
-- | gives ["12", "10", "15", "5"]
parseLinesToStringList :: [String] -> [String]
parseLinesToStringList [] = []
parseLinesToStringList [x] = parseCurrent x
parseLinesToStringList (x:xs) = (parseLinesToStringList $ parseCurrent x) ++ parseLinesToStringList xs

-- | Splits a ;-separated string into a list
-- | Example: "12;10"
-- | gives ["12", "10"]
parseCurrent :: String -> [String]
parseCurrent c = splitOn ";" $ filter (/=' ') c

-- | Convert String to Double datatype
-- TODO: use reads?
convertToDouble :: String -> Double
convertToDouble aString = read aString :: Double

-- ||| List conversions
-- | Converst list of strings to list of double values
convertListStringToDouble :: [String] -> [Double]
convertListStringToDouble = map convertToDouble

-- ||| List manipulations
-- | Remove first element from list with groups [a1, b1, c1, a2, b2, c2, ...]
-- | Example: [20141112;82.3;a comment;20141113;82.1;another comment]
-- | gives: [82.3;82.1]
removeFirstFromGroupedList :: [a] -> [a]
removeFirstFromGroupedList [] = []
removeFirstFromGroupedList [x] = []
removeFirstFromGroupedList (x:y:[]) = [y]
removeFirstFromGroupedList (x:y:z:[]) = [y] ++ [z]
removeFirstFromGroupedList (x:y:z:xs) = [y] ++ [z] ++ (removeFirstFromList xs)

-- | Remove last element from list with groups [a1, b1, c1, a2, b2, c2, ...]
-- | Example: [20141112;82.3;a comment;20141113;82.1;another comment]
-- | gives: [20141112;82.3;20141113;82.1]
removeLastFromGroupedList :: [a] -> [a]
removeLastFromGroupedList [] = []
removeLastFromGroupedList [x] = []
removeLastFromGroupedList (x:y:[]) = [x] ++ [y]
removeLastFromGroupedList (x:y:z:[]) = [x] ++ [y] ++ [z]
removeLastFromGroupedList (x:y:z:xs) = [x] ++ [y] ++ [z] ++ (removeFirstFromList xs)

-- | Turn list into list of lists (2 pairs)
-- | Example: ["12", "10", "15", 5"]
-- | gives [["12", "10"], ["15", 5"]]
convertListToListOfLists :: [a] -> [[a]]
convertListToListOfLists [] = []
convertListToListOfLists [x] = []
convertListToListOfLists (x:y:[]) = [[x] ++ [y]]
convertListToListOfLists (x:y:xs) = [[x] ++ [y]] ++ (convertListToListOfLists xs)

-- | Drop the last n elements from a list
dropLastN :: Int -> [a] -> [a]
dropLastN n xs = reverse $ foldl' (const . drop 1) (reverse xs) (drop n xs)

-- | Get the last n elements from a list
getLastN :: Int -> [a] -> [a]
getLastN n xs = foldl' (const . drop 1) xs (drop n xs)
