{-# LANGUAGE TypeFamilies #-}
module Data.Logic.Ersatz.Encoding 
    ( Encoding(..)
    ) where

import Data.Logic.Ersatz.Solution
import Data.Logic.Ersatz.Internal.Problem
import Control.Applicative

class Encoding t where
    type Decoded t :: *
    decode :: Solution b -> t b -> Maybe (Decoded t)

instance Encoding (Const b) where
    type Decoded (Const b) = b
    decode _ (Const b) = Just b

instance Encoding Lit where
    type Decoded Lit = Bool
    decode _ (Bool b) = return b
    decode f (Lit l)
        | literalId l < 0 = not <$> f (negateLiteral l)
        | otherwise       = f l

