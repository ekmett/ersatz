{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Ersatz.Relation.Op

( mirror
, union
, complement
, product, power
, intersection
) 

where

import Ersatz.Relation.Data
  
import Prelude hiding ( and, or, not, product )
import Ersatz.Bit (and, or, not)
import qualified Prelude

import Control.Monad ( guard )
import Data.Ix

mirror :: ( Ix a , Ix b ) => Relation a b -> Relation b a
mirror r = 
    let ((a,b),(c,d)) = bounds r
    in  build ((b,a),(d,c)) $ do (x,y) <- indices r ; return ((y,x), r!(x,y))

complement :: ( Ix a , Ix b ) => Relation a b -> Relation a b
complement r = 
    build (bounds r) $ do i <- indices r ; return ( i, not $ r!i )


union :: ( Ix a , Ix b )
        => Relation a b -> Relation a b ->  Relation a b
union r s =  build ( bounds r ) $ do
    i <- indices r
    return (i, or [ r!i, s!i ] )

product :: ( Ix a , Ix b, Ix c )
        => Relation a b -> Relation b c ->  Relation a c 
product a b = 
    let ((ao,al),(au,ar)) = bounds a
        ((bo,bl),(bu,br)) = bounds b
        bnd = ((ao,bl),(au,br))
    in  build bnd $ do
          i @ (x,z) <- range bnd
          return (i, or $ do
                y <- range ( al, ar )
                return $ and [ a!(x,y), b!(y,z) ]
                ) 
            
power  :: ( Ix a  ) 
        => Int -> Relation a a -> Relation a a 
power 0 r = identity ( bounds r ) 
power 1 r = r
power e r = 
    let (d,m) = divMod e 2
        s = power d r
        s2 = product s s
    in  case m of
        0 -> s2
        1 -> product s2 r

intersection :: ( Ix a , Ix b)
      => Relation a b -> Relation a b 
      -> Relation a b 
intersection r s = build ( bounds r ) $ do
        i <- indices r
        return (i, and [ r!i, s!i ] )

