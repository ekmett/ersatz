{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Ersatz.Relation.Op

( 
-- * Operations
  mirror
, union
, complement
, difference
, product, power
, intersection
, reflexive_closure
, symmetric_closure
)

where

import Ersatz.Relation.Data

import Prelude hiding ( and, or, not, product )
import Ersatz.Bit (and, or, not)

import Data.Ix

-- | Constructs the converse relation \( R^{-1} \subseteq B \times A \) of a relation 
-- \( R \subseteq A \times B \), which is defined by \( R^{-1} = \{ (y,x) ~|~ (x,y) \in R \} \).
mirror :: ( Ix a , Ix b ) => Relation a b -> Relation b a
mirror r =
    let ((a,b),(c,d)) = bounds r
    in  build ((b,a),(d,c)) $ do (x,y) <- indices r ; return ((y,x), r!(x,y))

-- | Constructs the complement relation \( \overline{R} \) 
-- of a relation \( R \subseteq A \times B \), which is defined by 
-- \( \overline{R}  = \{ (x,y) \in A \times B ~|~ (x,y) \notin R \} \).
complement :: ( Ix a , Ix b ) => Relation a b -> Relation a b
complement r =
    build (bounds r) $ do i <- indices r ; return ( i, not $ r!i )

-- | Constructs the difference \( R \setminus S \) of the relations 
-- \(R\) and \(S\), that contains all elements that are in \(R\) but not in \(S\), i.e.,
-- \( R \setminus S = \{ (x,y) \in R ~|~ (x,y) \notin S \} \).
--
-- Note that if \( R \subseteq A \times B \) and \( S \subseteq C \times D \),
-- then \( A \times B \) must be a subset of \( C \times D \) and
-- \( R \setminus S \subseteq A \times B \).
difference :: ( Ix a , Ix b )
        => Relation a b -> Relation a b ->  Relation a b
difference r s =
    intersection r $ complement s

-- | Constructs the union \( R \cup S \) of the relations \( R \) and \( S \).
--
-- Note that for \( R \subseteq A \times B \) and \( S \subseteq C \times D \),
-- it must hold that \( A \times B \subseteq C \times D \).
union :: ( Ix a , Ix b )
        => Relation a b -> Relation a b ->  Relation a b
union r s =  build ( bounds r ) $ do
    i <- indices r
    return (i, or [ r!i, s!i ] )

-- | Constructs the composition \( R \cdot S \) of the relations 
-- \( R \subseteq A \times B \) and \( S \subseteq B \times C \), which is 
-- defined by \( R \cdot S = \{ (a,c) ~|~ ((a,b) \in R) \land ((b,c) \in S) \} \).
product :: ( Ix a , Ix b, Ix c )
        => Relation a b -> Relation b c ->  Relation a c
product a b =
    let ((ao,al),(au,ar)) = bounds a
        ((_ ,bl),(_ ,br)) = bounds b
        bnd = ((ao,bl),(au,br))
    in  build bnd $ do
          i@(x,z) <- range bnd
          return (i, or $ do
                y <- range ( al, ar )
                return $ and [ a!(x,y), b!(y,z) ]
                )

-- | Constructs the relation \( R^{n} \) that results if a relation
-- \( R \subseteq A \times A \) is composed \(n\) times with itself.
--
-- \( R^{0} \) is the identity relation \( I_{R} = \{ (x,x) ~|~ x \in A \} \) of \(R\).
power  :: ( Ix a  )
        => Int -- ^ \(n\)
        -> Relation a a -> Relation a a
power 0 r = identity ( bounds r )
power 1 r = r
power e r =
    let (d,m) = divMod e 2
        s = power d r
        s2 = product s s
    in  case m of
        0 -> s2
        _ -> product s2 r

-- | Constructs the intersection \( R \cap S \) of the relations \( R \) and \( S \).
--
-- Note that for \( R \subseteq A \times B \) and \( S \subseteq C \times D \),
-- it must hold that \( A \times B \subseteq C \times D \).
intersection :: ( Ix a , Ix b)
      => Relation a b -> Relation a b
      -> Relation a b
intersection r s = build ( bounds r ) $ do
        i <- indices r
        return (i, and [ r!i, s!i ] )

-- | Constructs the reflexive closure \( R \cup I_{R} \) of the relation 
-- \( R \subseteq A \times A \), where \( I_{R} = \{ (x,x) ~|~ x \in A \} \) 
-- is the identity relation of \(R\).
reflexive_closure :: Ix a => Relation a a -> Relation a a
reflexive_closure t =
    union t $ identity $ bounds t

-- | Constructs the symmetric closure \( R \cup R^{-1} \) of the relation 
-- \( R \subseteq A \times A \), where \( R^{-1} = \{ (y,x) ~|~ (x,y) \in R \} \)
-- is converse relation of \(R\).
symmetric_closure :: Ix a => Relation a a -> Relation a a
symmetric_closure r =
    union r $ mirror r
