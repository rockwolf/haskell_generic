------------------------------------------------------------------------------
-- | This module does file operations and general data loading/saving.
-- See LICENSE file for copyright and license info.
------------------------------------------------------------------------------
module FileIO where

-- ||| Imports
-- TODO: what imports are needed?

-- | Return file content as a list of (IO) strings.
loadFileToStringList :: FilePath -> IO [String]
loadFileToStringList filename = do
  my_data <- readFile filename
  return $ lines my_data
