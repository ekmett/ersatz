{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module RegexpGrid.Types
( Pos (..)
, Field (..)
) where

import Control.Applicative
import Data.Char (chr, ord)
import Data.Typeable
import Ersatz
import GHC.Generics

data Pos =             P00|P01|P02|P03|P04|P05|P06
         |           P10|P11|P12|P13|P14|P15|P16|P17
         |         P20|P21|P22|P23|P24|P25|P26|P27|P28
         |       P30|P31|P32|P33|P34|P35|P36|P37|P38|P39
         |     P40|P41|P42|P43|P44|P45|P46|P47|P48|P49|P4a
         |   P50|P51|P52|P53|P54|P55|P56|P57|P58|P59|P5a|P5b
         | P60|P61|P62|P63|P64|P65|P66|P67|P68|P69|P6a|P6b|P6c
         |   P70|P71|P72|P73|P74|P75|P76|P77|P78|P79|P7a|P7b
         |     P80|P81|P82|P83|P84|P85|P86|P87|P88|P89|P8a
         |       P90|P91|P92|P93|P94|P95|P96|P97|P98|P99
         |         Pa0|Pa1|Pa2|Pa3|Pa4|Pa5|Pa6|Pa7|Pa8
         |           Pb0|Pb1|Pb2|Pb3|Pb4|Pb5|Pb6|Pb7
         |             Pc0|Pc1|Pc2|Pc3|Pc4|Pc5|Pc6
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

-- 5 bits are enough for A–Z. (The subset of the alphabet used by the regexps
-- also requires 5 bits. For simplicity, just use the full alphabet.)
newtype Field = Field Bit5
  deriving (Show, Typeable, Generic)

instance Boolean   Field
instance Variable  Field
instance Equatable Field

instance Codec Field where
  type Decoded Field = Char
  decode s (Field f) = chr . (+ origin) . fromIntegral <$> decode s f
  encode = Field . encode . fromIntegral . subtract origin . ord

-- Encode 0 as the character preceding A.
origin :: Int
origin = ord 'A' - 1
