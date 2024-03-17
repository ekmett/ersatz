{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Ersatz.Relation.Op

( 
-- * Operations
  union
, complement
, difference
, intersection
, mirror
, product, power
, restrict
, override
, update
, fiber_product
, reflexive_closure
, symmetric_closure
, transitive_closure
, transitive_reflexive_closure
, equivalence_closure
)

where


import Prelude hiding ( (&&), (||), and, any, or, not, product )
import Control.Arrow ( (&&&), (***) )

import Ersatz.Bit ( Bit, (&&), (||), and, any, or, not )
import Ersatz.Relation.Data

import qualified Data.Array as A
import Data.Ix

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

-- | Constructs the converse relation \( R^{-1} \subseteq B \times A \) of a relation
-- \( R \subseteq A \times B \), which is defined by \( R^{-1} = \{ (y,x) ~|~ (x,y) \in R \} \).
mirror :: ( Ix a , Ix b ) => Relation a b -> Relation b a
mirror r =
    let ((a,b),(c,d)) = bounds r
    in  build ((b,a),(d,c)) $ do (x,y) <- indices r ; return ((y,x), r!(x,y))

-- | Constructs the composition \( R \cdot S \) of the relations
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

-- | Constructs the restriction \( R_{\big|S} \) of a relation
-- \( R \subseteq A \times B \) to a subset \( S \subseteq A \) of its domain
-- defined by a membership predicate. \( R_{\big|S} \) is defined by
--  \( R_{\big|S} = \{ (x,y) \in R ~|~ x \in S \} \).
restrict :: ( Ix a, Ix b ) => Relation a b -> (a -> Bit) -> Relation a b
restrict r p =
    buildFrom (bounds r) (\(x,y) -> p x && r ! (x,y))

-- | Constructs the override ("[right] priority union", "preferential union") of
-- the relations \( R, S \subseteq A \times B \), defined as
-- \( R \rhd S = \{ (x,y) \in R | \nexists z \in B . (x,z) \in S \} \cup S \).
--
-- Equivalently, \( R \rhd S \) is the union of \( S \) with the restriction of
-- \( R \) to the complement of \( S \)'s domain of definition.
--
-- Unlike the commutative union of relations, the override of two functional
-- relations is always another functional relation.
--
-- Two more familiar versions of this are the (left-biased) @union@ of
-- [Data.Map](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Lazy.html#g:12)
-- and the function-into-monoid monoid for 'Alt'/'First'. (See e.g. §4 of
-- [Elliot, 2009](http://conal.net/papers/type-class-morphisms/type-class-morphisms-long.pdf)
-- for brief discussion.)
--
-- Returns @Nothing@ iff the arguments do not have the same bounds.
override :: ( Ix a , Ix b ) => Relation a b -> Relation a b -> Maybe (Relation a b)
override r s
    | bounds r /= bounds s = Nothing
    | otherwise = let
           in_dom_def t a = any ((t !) . (a,)) $ codomain t
        in Just $ union r $ restrict s (not . in_dom_def r)

-- | Constructs the update of relation \( R \subseteq A \times B \) by
-- the relation \( S \subseteq A \times B \), defined as
-- \( R[S] = R - S \cup \{ (x,y) \in S | \exists z \in B . (x,z) \in R \} \).
--
-- Note that 'update' cannot change \( R \)'s domain of definition — it only
-- updates the image of \( R \) according to \( S \).
--
-- Returns @Nothing@ iff the arguments do not have the same bounds.
update :: ( Ix a, Ix b ) => Relation a b -> Relation a b -> Maybe (Relation a b)
update r s
    | bounds r /= bounds s = Nothing
    | otherwise = let
           in_dom_def t a = any ((t !) . (a,)) $ codomain t
           f (x,y) =  (r ! (x,y) && not (in_dom_def s x))
                   || (s ! (x,y) &&      in_dom_def r x )
        in Just $ buildFrom (bounds r) f

-- | Constructs a generalization of the fiber product of functions to relations.
--
-- For functions \( f : A \rightarrow C \) and \( g : B \rightarrow C \), their
-- fiber product is the relation
-- \( \{ (a,b) \in A \times B ~|~ f(a) = g(b) \} \).
-- Extending this to relations, given \( F \subseteq A \times C \) and
-- \( G \subseteq B \times C \), the generalization of the fiber product is
-- \( \{ (a,b) \in A \times B ~|~ \exists c \in C . (a,c) \in F \land (b,c) \in G \} \).
--
-- Returns @Nothing@ iff the arguments do not have the same bounds on their codomain.
fiber_product :: (Ix a, Ix b, Ix c)
  => Relation a c -> Relation b c -> Maybe (Relation a b)
fiber_product f g
    | uncurry (/=)
      . ((snd *** snd) *** (snd *** snd))
      . (bounds *** bounds)
      $ (f, g)  = Nothing
    | otherwise = let
           ((aMin,cMin),(aMax,cMax)) = bounds f
           ((bMin,_   ),(bMax,_   )) = bounds g
           r (x,y) = any ( uncurry (&&)
                         . ((f !) . (x,) &&& (g !) . (y,))
                         )
                         $ range (cMin,cMax)
        in Just $ buildFrom ((aMin,bMin),(aMax,bMax)) r

-- | Constructs the reflexive closure \( R \cup I_{R} \) of the relation
-- \( R \subseteq A \times A \), where \( I_{R} = \{ (x,x) ~|~ x \in A \} \) 
-- is the identity relation of \(R\).
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
