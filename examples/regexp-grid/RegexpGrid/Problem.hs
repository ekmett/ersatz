{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generate an Ersatz problem definition for the regexp grid.

module RegexpGrid.Problem (problem) where

import Prelude hiding ((&&), (||), not, and, or, all, any)
import qualified Prelude as P

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.Foldable (asum)
import Data.Map (Map)
import qualified Data.Map as Map
import Ersatz

import RegexpGrid.Regexp
import RegexpGrid.Types

type ReBit = StateT ReBitState []

data ReBitState = ReBitState { _rbsFields    :: [Field]
                             , _rbsLastGroup :: Integer
                             , _rbsGroups    :: Map Integer [Field]
                             }
  deriving Show
makeLenses ''ReBitState

problem :: (Applicative m, MonadState s m, HasSAT s) => m (Map Pos Field)
problem = do
  -- Allocate a literal for each field.
  fieldMap <- Map.fromList <$> mapM (\pos -> (pos,) <$> exists) [minBound..]
  runReaderT problem' fieldMap
  return fieldMap

problem' :: (MonadState s m, HasSAT s) => ReaderT (Map Pos Field) m ()
problem' = do
  r [P00,P01,P02,P03,P04,P05,P06] ".*H.*H.*"
  r [P10,P11,P12,P13,P14,P15,P16,P17] "(DI|NS|TH|OM)*"
  r [P20,P21,P22,P23,P24,P25,P26,P27,P28] "F.*[AO].*[AO].*"
  r [P30,P31,P32,P33,P34,P35,P36,P37,P38,P39] "(O|RHH|MM)*"
  r [P40,P41,P42,P43,P44,P45,P46,P47,P48,P49,P4a] ".*"
  r [P50,P51,P52,P53,P54,P55,P56,P57,P58,P59,P5a,P5b] "C*MC(CCC|MM)*"
  r [P60,P61,P62,P63,P64,P65,P66,P67,P68,P69,P6a,P6b,P6c] "[^C]*[^R]*III.*"
  r [P70,P71,P72,P73,P74,P75,P76,P77,P78,P79,P7a,P7b] "(...?)\\1*"
  r [P80,P81,P82,P83,P84,P85,P86,P87,P88,P89,P8a] "([^X]|XCC)*"
  r [P90,P91,P92,P93,P94,P95,P96,P97,P98,P99] "(RR|HHH)*.?"
  r [Pa0,Pa1,Pa2,Pa3,Pa4,Pa5,Pa6,Pa7,Pa8] "N.*X.X.X.*E"
  r [Pb0,Pb1,Pb2,Pb3,Pb4,Pb5,Pb6,Pb7] "R*D*M*"
  r [Pc0,Pc1,Pc2,Pc3,Pc4,Pc5,Pc6] ".(C|HH)*"

  r [P00,P10,P20,P30,P40,P50,P60] "(ND|ET|IN)[^X]*"
  r [P01,P11,P21,P31,P41,P51,P61,P70] "[CHMNOR]*I[CHMNOR]*"
  r [P02,P12,P22,P32,P42,P52,P62,P71,P80] "P+(..)\\1.*"
  r [P03,P13,P23,P33,P43,P53,P63,P72,P81,P90] "(E|CR|MN)*"
  r [P04,P14,P24,P34,P44,P54,P64,P73,P82,P91,Pa0] "([^MC]|MM|CC)*"
  r [P05,P15,P25,P35,P45,P55,P65,P74,P83,P92,Pa1,Pb0] "[AM]*CM(RC)*R?"
  r [P06,P16,P26,P36,P46,P56,P66,P75,P84,P93,Pa2,Pb1,Pc0] ".*"
  r [P17,P27,P37,P47,P57,P67,P76,P85,P94,Pa3,Pb2,Pc1] ".*PRR.*DDC.*"
  r [P28,P38,P48,P58,P68,P77,P86,P95,Pa4,Pb3,Pc2] "(HHX|[^HX])*"
  r [P39,P49,P59,P69,P78,P87,P96,Pa5,Pb4,Pc3] "([^EMC]|EM)*"
  r [P4a,P5a,P6a,P79,P88,P97,Pa6,Pb5,Pc4] ".*OXR.*"
  r [P5b,P6b,P7a,P89,P98,Pa7,Pb6,Pc5] ".*LR.*RL.*"
  r [P6c,P7b,P8a,P99,Pa8,Pb7,Pc6] ".*SE.*UE.*"

  r [Pc0,Pb0,Pa0,P90,P80,P70,P60] ".*G.*V.*H.*"
  r [Pc1,Pb1,Pa1,P91,P81,P71,P61,P50] "[CR]*"
  r [Pc2,Pb2,Pa2,P92,P82,P72,P62,P51,P40] ".*XEXM*"
  r [Pc3,Pb3,Pa3,P93,P83,P73,P63,P52,P41,P30] ".*DD.*CCM.*"
  r [Pc4,Pb4,Pa4,P94,P84,P74,P64,P53,P42,P31,P20] ".*XHCR.*X.*"
  r [Pc5,Pb5,Pa5,P95,P85,P75,P65,P54,P43,P32,P21,P10] ".*(.)(.)(.)(.)\\4\\3\\2\\1.*"
  r [Pc6,Pb6,Pa6,P96,P86,P76,P66,P55,P44,P33,P22,P11,P00] ".*(IN|SE|HI)"
  r [Pb7,Pa7,P97,P87,P77,P67,P56,P45,P34,P23,P12,P01] "[^C]*MMM[^C]*"
  r [Pa8,P98,P88,P78,P68,P57,P46,P35,P24,P13,P02] ".*(.)C\\1X\\1.*"
  r [P99,P89,P79,P69,P58,P47,P36,P25,P14,P03] "[CEIMU]*OH[AEMOR]*"
  r [P8a,P7a,P6a,P59,P48,P37,P26,P15,P04] "(RX|[^R])*"
  r [P7b,P6b,P5a,P49,P38,P27,P16,P05] "[^M]*M[^M]*"
  r [P6c,P5b,P4a,P39,P28,P17,P06] "(S|MM|HHH)*"

r :: (MonadState s m, HasSAT s)
  => [Pos] -> String -> ReaderT (Map Pos Field) m ()
r poss regexpStr = do
  fieldMap <- ask
  let fields = map (fieldMap Map.!) poss

  regexp <- either (fail . show) return
          $ parseRegexp "RegexpGrid.Problem" regexpStr

  lift . assert $ runReBit regexp fields

runReBit :: Regexp -> [Field] -> Bit
runReBit regexp fields = or (evalStateT go (ReBitState fields 0 Map.empty))
  where
    go = snd <$> reBit regexp <* do { [] <- use rbsFields; return () }

reBit :: Regexp -> ReBit ([Field], Bit)
reBit Nil = pure ([], true)

reBit (AnyCharacter next) = do
  f <- nextField
  reBit next <&> \(gfs, resb) -> (f:gfs, resb)

reBit (Character c next) = do
  f <- nextField
  let b = f === encode c
  reBit next <&> \(gfs, resb) -> (f:gfs, b && resb)

reBit (Accept cs next) = do
  f <- nextField
  let b = any (\c -> f === encode c) cs
  reBit next <&> \(gfs, resb) -> (f:gfs, b && resb)

reBit (Reject cs next) = do
  f <- nextField
  let b = all (\c -> f /== encode c) cs
  reBit next <&> \(gfs, resb) -> (f:gfs, b && resb)

reBit (Choice res next) = do
  (gfs0, b) <- asum (map reBit res)
  reBit next <&> \(gfs1, resb) -> (gfs0 ++ gfs1, b && resb)

reBit (Group re' next) = do
  (gfs0, b) <- reBit re'

  gid <- rbsLastGroup <+= 1
  rbsGroups . at gid ?= gfs0

  reBit next <&> \(gfs1, resb) -> (gfs0 ++ gfs1, b && resb)

reBit (Repeat _ (Just 0) _ next) =
  reBit next
reBit (Repeat 0 mj re' next) =
  reBit next <|> reBit (Repeat 1 mj re' next)
reBit (Repeat i mj re' next) = do
  (gfs0, b) <- reBit re'
  reBit (Repeat (i-1) (subtract 1 <$> mj) re' next)
    <&> \(gfs1, resb) -> (gfs0 ++ gfs1, b && resb)

reBit (Backreference n next) = do
  Just rfs <- use (rbsGroups . at n)
  fs <- mapM (const nextField) rfs
  let b = and (zipWith (===) rfs fs)
  reBit next <&> \(gfs, resb) -> (fs ++ gfs, b && resb)

nextField :: ReBit Field
nextField = do
  (f:fs) <- use rbsFields
  rbsFields .= fs
  return f
