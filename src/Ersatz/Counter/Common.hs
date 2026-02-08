module Ersatz.Counter.Common
  ( parseCounter
  ) where

import Ersatz.Solution
import Text.Read (readMaybe)

parseCounter :: String -> CountResult
parseCounter txt =
    case [n | ('s':' ':rest) <- lines txt
            , Just n <- [parseMc rest]] of
      (n:_) -> Count n
      []    -> CountUnsolved
  where
    parseMc :: String -> Maybe Integer
    parseMc line =
      case words line of
        ("mc":num:_) -> readMaybe num
        _            -> Nothing
