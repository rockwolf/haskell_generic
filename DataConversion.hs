-- | ------------------------------------------------------------------------------|
-- | Main module which does data-type conversions and general data manipulations.  |
-- | ------------------------------------------------------------------------------|

module DataConversion where

-- ||| Data conversion methods
-- | Convert String to Double datatype
-- TODO: use reads?
convertToDouble :: String -> Double
convertToDouble aString = read aString :: Double

-- ||| List conversions
-- | Converst list of strings to list of double values
convertListStringToDouble :: [String] -> [Double]
convertListStringToDouble = map convertToDouble

-- ||| List manipulations
-- | Remove first element from list
-- | Example: [20141112;82.3;a comment;20141113;82.1;another comment]
-- | gives: [82.3;82.1]
removeFirstFromList [] = []
removeFirstFromList [x] = []
removeFirstFromList (x:y:[]) = [y]
removeFirstFromList (x:y:z:[]) = [y] ++ [z]
removeFirstFromList (x:y:z:xs) = [y] ++ [z] ++ (removeFirstFromList xs)

-- | Remove last element from list
-- | Example: [20141112;82.3;a comment;20141113;82.1;another comment]
-- | gives: [20141112;82.3;20141113;82.1]
removeLastFromList [] = []
removeLastFromList [x] = []
removeLastFromList (x:y:[]) = [x] ++ [y]
removeLastFromList (x:y:z:[]) = [x] ++ [y] ++ [z]
removeLastFromList (x:y:z:xs) = [x] ++ [y] ++ [z] ++ (removeFirstFromList xs)
