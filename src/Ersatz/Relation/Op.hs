{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Ersatz.Relation.Op
  (  -- * Operations
    insert
  , delete
  , map
  , filter
  , partition
  , mirror
  , union
  , complement
  , difference
  , symmetricDifference
  , intersection
  , product, power
  , reflexiveClosure
  , symmetricClosure
  , transitiveClosure
  , transitiveReflexiveClosure
  , equivalenceClosure
  ) where

import Prelude hiding ( (&&), (||), and, or, not, product, map, filter )
import Control.Arrow ((&&&) , (***) , first)

import qualified Data.Array as A
import qualified Data.Ix as Ix
import Data.Ix

import Ersatz.Bit

import Ersatz.Relation.Data


-- | Given a relation \( R \subseteq A \times B \) and a pair of elements
-- \( (a,b) \), construct \( R \cup \{(a,b)\} \).
insert :: (Ix a, Ix b) => (a,b) -> Relation a b -> Relation a b
insert i r
  | not (bounds r `Ix.inRange` i) =
    error "Element out of bounds of the relation!"
  | otherwise =
    buildFrom (bounds r)
              (\x -> bool (x == i) || x `member` r)

-- | Given a relation \( R \subseteq A \times B \) and a pair of elements
-- \( (a,b) \), construct \( R \setminus \{(a,b)\} \).
delete :: (Ix a, Ix b) => (a,b) -> Relation a b -> Relation a b
delete i r
  | not (bounds r `Ix.inRange` i) =
    error "Element out of bounds of the relation!"
  | otherwise = filter (bool . (i /=)) r


-- | 'fmap' function for symbolic relations; note that the bounds of the new
-- 'Relation' are defined by applying the provided function to the bounds of the
-- original 'Relation'.
map :: (Ix a, Ix b, Ix c, Ix d)
    => ((a,b) -> (c,d))
    -> Relation a b
    -> Relation c d
map f =  build . (f *** f) . bounds
     <*> (fmap (first f) . assocs)

-- | Restrict a relation to only the subset that satisfies the provided
-- predicate.
filter :: (Ix a, Ix b) => ((a,b) -> Bit) -> Relation a b -> Relation a b
filter p r = buildFrom (bounds r) ( uncurry (&&)
                                  . ((r !) &&& p))

-- | Partition a set into the subset that satisfies a predicate vs. the subset
-- that does not.
partition :: (Ix a, Ix b)
  => ((a,b) -> Bit)
  -> Relation a b
  -> (Relation a b, Relation a b)
partition p = filter p &&& filter (not . p)


-- | Constructs the converse relation \( R^{-1} \) of a relation
-- \( R \subseteq A \times B \), which is defined by
-- \( R^{-1} = \{ (y,x) ~|~ (x,y) \in R \} \subseteq B \times A \).
mirror :: (Ix a , Ix b) => Relation a b -> Relation b a
mirror r =
  let swap (x,y) = (y,x)
  in  buildFrom ((swap *** swap) $ bounds r) $ (r !) . swap

-- | Constructs the complement relation \( \overline{R} \) 
-- of a relation \( R \subseteq A \times B \), which is defined by 
-- \( \overline{R}  = \{ (x,y) \in A \times B ~|~ (x,y) \notin R \} \).
complement :: (Ix a, Ix b) => Relation a b -> Relation a b
complement r =
  buildFrom (bounds r) $ not . (r !)

-- | Constructs the difference \( R \setminus S \) of the relations 
-- \(R, S \subseteq A \times B \) containing all elements that are in \(R\)
-- but not in \(S\), i.e.,
-- \( R \setminus S = \{ (x,y) \in R ~|~ (x,y) \notin S \} \).
difference :: (Ix a , Ix b) => Relation a b -> Relation a b -> Relation a b
difference r s =
  intersection r $ complement s

-- | Constructs the symmetric difference
-- \( (R \setminus S) \cup (S \setminus R) \) of the arguments.
symmetricDifference :: (Ix a, Ix b)
  => Relation a b -> Relation a b -> Relation a b
symmetricDifference r s
  | bounds r /= bounds s = error "Relations don't have the same bounds!"
  | otherwise =
    buildFrom (bounds r) (\i -> (r ! i) `xor` (s ! i))

-- | Constructs the union \( R \cup S \) of the relations \( R, S \subseteq A \times B \).
union :: (Ix a , Ix b) => Relation a b -> Relation a b -> Relation a b
union r s
  | bounds r /= bounds s = error "Relations don't have the same bounds!"
  | otherwise =
    buildFrom (bounds r) (\i -> (r ! i) || (s ! i))

-- | Constructs the composition \( R \circ S \) of the relations 
-- \( R \subseteq A \times B \) and \( S \subseteq B \times C \), which is 
-- defined by \( R \circ S = \{ (a,c) ~|~ (a,b) \in R \land (b,c) \in S \} \).
--
-- Formula size: linear in \(|A|\cdot|B|\cdot|C|\)
product :: (Ix a, Ix b, Ix c) => Relation a b -> Relation b c -> Relation a c
product a b =
  let ((ao,al),(au,ar)) = bounds a
      ((bo,bl),(bu,br)) = bounds b
      bnd = ((ao,bl),(au,br))
  in  if (al,ar) == (bo,bu)
          then build bnd $ do
              i@(x,z) <- range bnd
              return (i, or $ do
                      y <- range ( al, ar )
                      return $  a!(x,y) && b!(y,z)
                      )
          else error "Codomain of first relation must equal domain of second relation!"

-- | Constructs the relation \( R^{n} \) that results if a relation
-- \( R \subseteq A \times A \) is composed \(n\) times with itself.
--
-- \( R^{0} \) is the identity relation \( I = \{ (x,x) ~|~ x \in A \} \).
--
-- Formula size: linear in \( |A|^3 \cdot  \log n \)
power  :: (Ix a) => Int -> Relation a a -> Relation a a
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
intersection :: (Ix a , Ix b) => Relation a b -> Relation a b -> Relation a b
intersection r s
  | bounds r /= bounds s = error "Relations don't have the same bounds!"
  | otherwise =
    buildFrom (bounds r) (\i -> (r ! i) && (s ! i))

-- | Constructs the reflexive closure \( R \cup R^{0} \) of the relation
-- \( R \subseteq A \times A \).
reflexiveClosure :: Ix a => Relation a a -> Relation a a
reflexiveClosure =
  union <*> identity . bounds

-- | Constructs the symmetric closure \( R \cup R^{-1} \) of the relation 
-- \( R \subseteq A \times A \).
symmetricClosure :: Ix a => Relation a a -> Relation a a
symmetricClosure =
  union <*> mirror

-- | Constructs the transitive closure \( R^{+} \) of the relation 
-- \( R \subseteq A \times A \), which is defined by 
-- \( R^{+} = \bigcup^{\infty}_{i = 1} R^{i} \).
--
-- Formula size: linear in \( |A|^3 \)
transitiveClosure :: Ix a => Relation a a -> Relation a a
transitiveClosure r =
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
transitiveReflexiveClosure :: Ix a => Relation a a -> Relation a a
transitiveReflexiveClosure =
    uncurry union
  . (transitiveClosure &&& identity . bounds)

-- | Constructs the equivalence closure \( (R \cup R^{-1})^* \) of the relation 
-- \( R \subseteq A \times A \).
--
-- Formula size: linear in \( |A|^3 \)
equivalenceClosure :: Ix a => Relation a a -> Relation a a
equivalenceClosure =
  transitiveReflexiveClosure . symmetricClosure
