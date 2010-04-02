{-# LANGUAGE GADTs #-}
module Logic.QSAT.Encoding.Bool where

import Logic.QSAT.Problem


data QBool q b where
    Static :: Bool -> QBool q b 
    Exists :: (Bool -> Bool) -> {-# UNPACK #-} !(Literal b) -> QBool q b
    Forall :: {-# UNPACK #-} !(Literal b) -> QBool Forall b

instance Encoded (QBool Exists b) where
    type Decoded QBool = Bool
    type Brand (QBool Exists b) = b
    decode _ (Static s) = s
    decode f (Exists g l) = g (f l)
    
exists :: QSAT b (QBool q b)
exists = Exists id <$> fresh

forall :: QSAT b (QBool ForAll b)
forall = Forall <$> fresh

class Constant t where
    type Encoding t :: * -> *
    constant :: t -> Encoding t b

instance Constant Bool where
    type Encoding Bool = QBool Exists
    constant = Static 

instance Not (Boolean q b) where
    not (Static b) = Static (not b)
    not (Exists f b) = Exists (not . f) (not b)
    not (Forall b) = Forall (not b)
