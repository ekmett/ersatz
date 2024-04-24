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
, transitive_closure
, transitive_reflexive_closure
, equivalence_closure
)

where

import Ersatz.Relation.Data

import Prelude hiding ( (&&), (||), and, or, not, product )
import Ersatz.Bit

import qualified Data.Array as A
import Data.Ix

-- | Constructs the converse relation \( R^{-1} \) of a relation
-- \( R \subseteq A \times B \), which is defined by \( R^{-1} = \{ (y,x) ~|~ (x,y) \in R \} \subseteq B \times A \).
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
-- \(R, S \subseteq A \times B \), that contains all elements that are in \(R\) but not in \(S\), i.e.,
-- \( R \setminus S = \{ (x,y) \in R ~|~ (x,y) \notin S \} \).
difference :: ( Ix a , Ix b )
        => Relation a b -> Relation a b ->  Relation a b
difference r s =
    intersection r $ complement s

-- | Constructs the union \( R \cup S \) of the relations \( R, S \subseteq A \times B \).
union :: ( Ix a , Ix b )
        => Relation a b -> Relation a b -> Relation a b
union r s
    | bounds r == bounds s = build ( bounds r ) $ do
        i <- indices r
        return (i, r!i || s!i)
    | otherwise = error "Relations don't have the same bounds!"

-- | Constructs the composition \( R \circ S \) of the relations
-- \( R \subseteq A \times B \) and \( S \subseteq B \times C \), which is
-- defined by \( R \circ S = \{ (a,c) ~|~ (a,b) \in R \land (b,c) \in S \} \).
--
-- Formula size: linear in \(|A|\cdot|B|\cdot|C|\)
product :: ( Ix a , Ix b, Ix c )
        => Relation a b -> Relation b c -> Relation a c
product a b =
    let ((ao,al),(au,ar)) = bounds a
        ((bo,bl),(bu,br)) = bounds b
        bnd = ((ao,bl),(au,br))
    in  if (al,ar) == (bo,bu)
            then build bnd $ do
                i@(x,z) <- range bnd
                return (i, or $ do
                        y <- range ( al, ar )
                        return $ and [ a!(x,y), b!(y,z) ]
                        )
            else error "Codomain of first relation must equal domain of second relation!"

-- | Constructs the relation \( R^{n} \) that results if a relation
-- \( R \subseteq A \times A \) is composed \(n\) times with itself.
--
-- \( R^{0} \) is the identity relation \( I = \{ (x,x) ~|~ x \in A \} \).
--
-- Formula size: linear in \( |A|^3 \cdot  \log n \)
power  :: ( Ix a ) => Int -> Relation a a -> Relation a a
power 0 r = identity ( bounds r )
power 1 r = r
power e r =
    let (d,m) = divMod e 2
        s = power d r
        s2 = product s s
    in  case m of
        0 -> s2
        _ -> product s2 r

-- | Constructs the intersection \( R \cap S \) of the relations \( R, S \subseteq A \times B \).
intersection :: ( Ix a , Ix b )
      => Relation a b -> Relation a b
      -> Relation a b
intersection r s
    | bounds r == bounds s = build ( bounds r ) $ do
        i <- indices r
        return (i, and [ r!i, s!i ] )
    | otherwise = error "Relations don't have the same bounds!"

-- | Constructs the reflexive closure \( R \cup R^{0} \) of the relation
-- \( R \subseteq A \times A \).
reflexive_closure :: Ix a => Relation a a -> Relation a a
reflexive_closure t =
    union t $ identity $ bounds t

-- | Constructs the symmetric closure \( R \cup R^{-1} \) of the relation
-- \( R \subseteq A \times A \).
symmetric_closure :: Ix a => Relation a a -> Relation a a
symmetric_closure r =
    union r $ mirror r

-- | Constructs the transitive closure \( R^{+} \) of the relation
-- \( R \subseteq A \times A \), which is defined by
-- \( R^{+} = \bigcup^{\infty}_{i = 1} R^{i} \).
--
-- Formula size: linear in \( |A|^3 \)
transitive_closure :: Ix a => Relation a a -> Relation a a
transitive_closure r =
  let n = universeSize r
      -- @a' ! (0,p,q)@ is true if and only if @r ! (p,q)@ is true
      a' = A.listArray ((0,1,1),(n,n,n)) (elems r)
      -- @a ! (0,p,q)@ is true if and only if @a' ! (0,p,q)@ is true
      a = a' A.// do
        -- If x > 0, then @a ! (p,x,q)@ is true if and only if there is a path from p to q via nodes {1,...,x} in r
        i@(x,p,q) <- A.range ((1,1,1),(n,n,n))
        return (i, a A.! (x-1,p,q) || a A.! (x-1,p,x) && a A.! (x-1,x,q))
  in build (bounds r) $ zip (indices r) [a A.! i | i <- A.range ((n,1,1),(n,n,n))]

-- | Constructs the transitive reflexive closure \( R^{*} \) of the relation
-- \( R \subseteq A \times A \), which is defined by
-- \( R^{*} = \bigcup^{\infty}_{i = 0} R^{i} \).
--
-- Formula size: linear in \( |A|^3 \)
transitive_reflexive_closure :: Ix a => Relation a a -> Relation a a
transitive_reflexive_closure r =
    union (transitive_closure r) (identity $ bounds r)

-- | Constructs the equivalence closure \( (R \cup R^{-1})^* \) of the relation
-- \( R \subseteq A \times A \).
--
-- Formula size: linear in \( |A|^3 \)
equivalence_closure :: Ix a => Relation a a -> Relation a a
equivalence_closure r =
  transitive_reflexive_closure $ symmetric_closure r
