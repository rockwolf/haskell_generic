------------------------------------------------------------------------------
-- | This module does data-type conversions and general data manipulations.
--   See LICENSE file for copyright and license info.
------------------------------------------------------------------------------
module DataConversion where

-----------------------------------------------------------------------------
-- ||| Imports
-----------------------------------------------------------------------------
import Data.Default.Class
import Control.Lens hiding (argument)
import Data.List.Split
import Data.List
import Control.Monad (when)
import System.Exit (exitSuccess)

-----------------------------------------------------------------------------
-- ||| Data conversion methods
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | Parses a list of <delimiter>-separated string to a list of 
-- | lists, containing the separated strings.
--
-- Example: ["12;10", "15;5"]
-- gives [ ["12", "10"], ["15", "5"] ]
-----------------------------------------------------------------------------
splitLinesToListOfStrings :: String -> [String] -> [[String]]
splitLinesToListOfStrings delim [] = []
splitLinesToListOfStrings delim [x] = [splitString delim x]
splitLinesToListOfStrings delim (x:xs) = [splitString delim x] ++ splitLinesToListOfStrings delim xs

-----------------------------------------------------------------------------
-- | Splits a ;-separated string into a list
--
-- Example: "12;10"
-- gives ["12", "10"]
-----------------------------------------------------------------------------
splitString :: String -> String -> [String]
splitString delim c = splitOn delim $ filter (/=' ') c

-----------------------------------------------------------------------------
-- | Convert String to Double datatype
-----------------------------------------------------------------------------
-- TODO: use reads?
convertToDouble :: String -> Double
convertToDouble aString = read aString :: Double

-----------------------------------------------------------------------------
-- ||| List conversions
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | Converst list of strings to list of double values
-----------------------------------------------------------------------------
convertListStringToDouble :: [String] -> [Double]
convertListStringToDouble = map convertToDouble

-----------------------------------------------------------------------------
-- ||| List manipulations
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | Drop the last n elements from a list
-----------------------------------------------------------------------------
dropLastN :: Int -> [a] -> [a]
dropLastN n xs = reverse $ drop n (reverse xs)

-----------------------------------------------------------------------------
-- | Get the last n elements from a list
-----------------------------------------------------------------------------
getLastN :: Int -> [a] -> [a]
getLastN n xs = foldl' (const . drop 1) xs (drop n xs)
