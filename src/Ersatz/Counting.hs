module Ersatz.Counting where

import Ersatz.Bit
import Ersatz.Bits
import Ersatz.Codec
import Ersatz.Equatable
import Ersatz.Orderable

exactly :: Int -> [ Bit ] -> Bit
exactly k bs = encode (fromIntegral k) === sumBit bs

atmost :: Int -> [ Bit ] -> Bit
atmost k bs = encode (fromIntegral k) >=? sumBits bs

atleast :: Int -> [ Bit ] -> Bit
atleast k bs = encode (fromIntegral k) <=? sumBits bs

