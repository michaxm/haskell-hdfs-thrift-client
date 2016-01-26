module System.HDFS.InternalUtils where

import Data.List (intersperse)
import Data.List.Split (splitOn)

splitLocation :: String -> (String, String)
splitLocation str = let es = splitOn "/" str in (join "/" $ take 3 es, toAbsolute $ join "/" $ drop 3 es)
  where join delim = concat . intersperse delim
        toAbsolute  = ("/" ++) -- assumes all input is absolute anyway, just restores the delim
