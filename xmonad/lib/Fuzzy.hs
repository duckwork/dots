{-# LANGUAGE OverloadedStrings #-}
module Fuzzy
(
  fuzzyMatch
) where

import Data.List (intercalate)
import Text.Regex.Posix

fuzzyMatch :: String -> String -> Bool
fuzzyMatch q s = case s =~ pat :: (Int, Int) of
                   (-1, 0) -> False
                   (_, _)  -> True
                 where pat = intercalate ".*?" $ map (:[]) q
