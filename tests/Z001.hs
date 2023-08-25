{-# language KindSignatures, DataKinds, FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies, ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language NoMonomorphismRestriction #-}

import Prelude hiding ( not, and, or, (&&), (||) )

import Ersatz

import GHC.TypeLits
import Data.Proxy
import Data.List ( transpose )
import Control.Monad ( replicateM, forM_ )

main = do
  (Satisfied, Just ms) <- solveWith anyminisat $ do
    [ Restricted a, Restricted b ]
        :: [ Restricted 5 (NBV 3) ] <- replicateM 2 unknown
    -- assert $ gt (a^2 * b^2) (b^3 * a^3)
    let a2 = a^2 ; b2 = b^2
    assert $ gt (a2 * b2) (b2 * b * a * a2)
    return [a,b]
  forM_ ms print

unknown_monotone = do
   m <- unknown ; assert $ monotone m ; return m

newtype Restricted d a = Restricted (Matrix d a)

instance (KnownNat dim, Unknown a, Codec a, Num (Decoded a))
  => Unknown (Restricted dim a) where
  unknown = do
    let d = fromIntegral $ natVal (Proxy :: Proxy dim)
        row f = ( encode f : ) <$> replicateM (d-1) unknown
    m <- (:) <$> row 1 <*> replicateM (d-2) (row 0)
    return $ Restricted $ Matrix
       $ m ++ encode [ replicate (d-1) 0 ++ [1] ]

class Unknown a where
  unknown :: MonadSAT s m => m a

-- | square matrices
--
-- Invariant: @dim >= 2@
newtype Matrix (dim::Nat)  a = Matrix [[a]]
  deriving ( Show, Equatable, Orderable )

instance Codec a => Codec (Matrix dim a) where
  type Decoded (Matrix dim a) = Matrix dim (Decoded a)
  decode s (Matrix xss) = Matrix <$> decode s xss
  encode = error "encode not implemented for Matrix"

instance (KnownNat dim, Unknown a) => Unknown (Matrix dim a) where
  unknown = do
    let d = fromIntegral $ natVal (Proxy :: Proxy dim)
    Matrix <$> replicateM d (replicateM d unknown)

instance Num a => Num (Matrix dim a) where
  Matrix xss + Matrix yss
    = Matrix $ zipWith (zipWith (+)) xss yss
  Matrix xss * Matrix yss
    = Matrix $ for xss $ \ row ->
        for (transpose yss) $ \ col ->
          sum $ zipWith (*) row col
  (-)         = error "(-) not implemented for Matrix"
  abs         = error "abs not implemented for Matrix"
  signum      = error "signum not implemented for Matrix"
  fromInteger = error "fromInteger not implemented for Matrix"

for = flip map

topleft (Matrix xss) =
  case xss of
    ((x:_):_) -> x
    _ -> matrixInvariantViolated

botright (Matrix xss)
  | Just (_, xs) <- unsnoc xss
  , Just (_, x)  <- unsnoc xs
  = x
  | otherwise
  = matrixInvariantViolated

topright (Matrix xss)
  | xs:_ <- xss
  , Just (_, x) <- unsnoc xs
  = x
  | otherwise
  = matrixInvariantViolated

unsnoc :: [a] -> Maybe ([a], a)
unsnoc []     = Nothing
unsnoc (x:xs) = case unsnoc xs of
                  Nothing    -> Just ([], x)
                  Just (a,b) -> Just (x:a, b)

matrixInvariantViolated :: a
matrixInvariantViolated = error "Matrix invariant violated"

monotone m = positive (topleft m) && positive (botright m)

ge :: Orderable a => Matrix dim a -> Matrix dim a -> Bit
ge (Matrix xss) (Matrix yss) =
  and $ zipWith (>=?) (concat xss) (concat yss)

gt :: Orderable a => Matrix dim a -> Matrix dim a -> Bit
gt a b = ge a b && topright a >? topright b

-- | NBV = Non-overflowing Bitvector
-- Bitvectors of fixed length, with non-overflowing arithmetics
-- (if overflow occurs, constraint is unsatisfiable)

newtype NBV ( n :: Nat ) = NBV Bits
  deriving ( Show, Equatable, Orderable, HasBits )

instance KnownNat w => Unknown (NBV w) where
  unknown = do
    let n = fromIntegral $ natVal (Proxy :: Proxy w)
    NBV <$> Bits <$> replicateM n exists

positive (NBV (Bits bs)) = or bs

nbv n (Bits bs) =
  let (p : re, post) = splitAt n bs
  in  NBV $ Bits $ Run ( assert (not $ or post) *> return p )
                 : re

instance KnownNat n => Num (NBV n) where
  fromInteger = encode
  NBV a + NBV b =
    nbv (fromIntegral (natVal (Proxy :: Proxy n))) $ a + b
  NBV a * NBV b =
    nbv (fromIntegral (natVal (Proxy :: Proxy n))) $ a * b
  (-)    = error "(-) not implemented for NBV"
  abs    = error "abs not implemented for NBV"
  signum = error "signum not implemented for NBV"

instance KnownNat n => Codec (NBV n) where
  type Decoded (NBV n) = Integer
  decode s (NBV bs) = decode s bs
  encode i =
    let n = fromIntegral $ natVal (Proxy :: Proxy n)
        Bits bs = encode i
        (pre, post) = splitAt n bs
    in  if null post
        then NBV (Bits $ take n $ pre ++ repeat false)
        else error $ unwords
             [ "cannot encode", show i
             , "with given bit width", show n
             ]
