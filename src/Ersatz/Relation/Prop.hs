{-# OPTIONS_GHC -Wno-orphans #-}
module Ersatz.Relation.Prop

(
-- * Properties
  implies
, symmetric
, anti_symmetric
, transitive
, irreflexive
, reflexive
, regular
, regular_in_degree
, regular_out_degree
, max_in_degree
, min_in_degree
, max_out_degree
, min_out_degree
, empty
, complete
, total
, disjoint
)

where

import Prelude hiding ( and, or, not, product )
import Ersatz.Bit
import Ersatz.Relation.Data
import Ersatz.Relation.Op
import Ersatz.Counting
import Ersatz.Equatable

import Data.Ix


instance (Ix a, Ix b) => Equatable (Relation a b) where
  r === s = and [implies r s, implies s r]
  r /== s = not $ r === s

-- | Given two relations \( R, S \subseteq A \times B \), check if \(R\) is a subset of \(S\).
implies :: ( Ix a, Ix b )
        => Relation a b -> Relation a b -> Bit
implies r s
  | bounds r == bounds s = and $ do
      i <- indices r
      return $ (r ! i) ==> (s ! i)
  | otherwise = error "Relations don't have the same bounds!"

-- | Tests if a relation is empty, i.e., the relation doesn't contain any elements.
empty ::  ( Ix a, Ix b )
        => Relation a b -> Bit
empty r = and $ do
    i <- indices r
    return $ not $ r ! i

-- | Tests if a relation \( R \subseteq A \times B \) is complete,
-- i.e., \(R = A \times B \).
complete :: (Ix a, Ix b) => Relation a b -> Bit
complete r = empty $ complement r

-- | Tests if a relation \( R \subseteq A \times A \) is strongly connected, i.e.,
-- \( R \cup R^{-1} = A \times A \).
total :: ( Ix a ) => Relation a a -> Bit
total r = complete $ symmetric_closure r

-- | Tests if two relations are disjoint, i.e.,
-- there is no element that is contained in both relations.
disjoint :: (Ix a, Ix b) => Relation a b -> Relation a b -> Bit
disjoint r s = empty $ intersection r s

-- | Tests if a relation \( R \subseteq A \times A \) is symmetric,
-- i.e., \( R \cup R^{-1} = R \).
symmetric :: ( Ix a ) => Relation a a -> Bit
symmetric r = implies r ( mirror r )


-- | Tests if a relation \( R \subseteq A \times A \) is antisymmetric,
-- i.e., \( R \cap R^{-1} \subseteq R^{0} \).
anti_symmetric :: ( Ix a ) => Relation a a -> Bit
anti_symmetric r = implies (intersection r (mirror r)) (identity (bounds r))

-- | Tests if a relation \( R \subseteq A \times A \) is irreflexive, i.e.,
-- \( R \cap R^{0} = \emptyset \).
irreflexive :: ( Ix a ) => Relation a a -> Bit
irreflexive r = empty $ intersection (identity $ bounds r) r

-- | Tests if a relation \( R \subseteq A \times A \) is reflexive, i.e.,
-- \( R^{0} \subseteq R \).
reflexive :: ( Ix a ) => Relation a a -> Bit
reflexive r = implies (identity $ bounds r) r

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | = n \) and
-- \( \forall y \in B: | \{ (x,y) \in R \} | = n \) hold.
regular :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall y \in B: | \{ (x,y) \in R \} | = n \) holds.
regular_in_degree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | = n \) holds.
regular_out_degree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall y \in B: | \{ (x,y) \in R \} | \leq n \) holds.
max_in_degree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall y \in B: | \{ (x,y) \in R \} | \geq n \) holds.
min_in_degree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | \leq n \) holds.
max_out_degree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | \geq n \) holds.
min_out_degree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

regular deg r = and [ regular_in_degree deg r, regular_out_degree deg r ]

regular_out_degree = out_degree_helper exactly
max_out_degree = out_degree_helper atmost
min_out_degree = out_degree_helper atleast
regular_in_degree deg r = regular_out_degree deg $ mirror r
max_in_degree deg r = max_out_degree deg $ mirror r
min_in_degree deg r = min_out_degree deg $ mirror r

out_degree_helper ::
  (Boolean b, Ix b1, Ix a) =>
  (t -> [Bit] -> b) -> t -> Relation a b1 -> b
out_degree_helper f deg r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a , c )
    return $ f deg $ do
        y <- range (b,d)
        return $ r ! (x,y)

-- | Tests if a relation \( R \subseteq A \times A \) is transitive, i.e.,
-- \( R \circ R = R \).
--
-- Formula size: linear in \( |A|^3 \)
transitive :: ( Ix a )
           => Relation a a -> Bit
transitive r = implies (product r r) r

