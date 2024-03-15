module Ersatz.Relation.Prop
  (  -- * Properties
    implies
  , isSubsetOf
  , isProperSubsetOf
  , symmetric
  , antisymmetric
  , transitive
  , irreflexive
  , reflexive
  , regular
  , regularInDegree
  , regularOutDegree
  , maxInDegree
  , minInDegree
  , maxOutDegree
  , minOutDegree
  , null
  , complete
  , total
  , disjoint
  ) where

import Prelude hiding ( and, or, not, product, null, (&&) )

import Control.Arrow ((&&&))
import Data.Composition ((.:))

import Data.Ix

import Ersatz.Bit
import Ersatz.Counting
import Ersatz.Equatable

import Ersatz.Relation.Data
import Ersatz.Relation.Op

instance (Ix a, Ix b) => Equatable (Relation a b) where
  r === s
    | bounds r /= bounds s =
      error "Relations don't have the same bounds!"
    | otherwise =
      and $ zipWith (===) (elems r) (elems s)
  r /== s = not $ r === s

-- | Given two relations \( R, S \subseteq A \times B \), check if \(R\) is a
-- subset of \(S\).
implies :: (Ix a, Ix b)
        => Relation a b -> Relation a b -> Bit
implies r s
  | bounds r == bounds s = and $ zipWith (==>) (elems r) (elems s)
  | otherwise = error "Relations don't have the same bounds!"

-- | Synonym for 'implies', provided for a @Set@-like interface.
--
-- Note that this does not do any set-related reasoning about the bounds of the
-- two relations: like other 'Relation' functions, this currently throws an
-- error if the argument relations do not have the same bounds.
isSubsetOf :: (Ix a, Ix b)
           => Relation a b -> Relation a b -> Bit
isSubsetOf = implies

-- | Tests if the first relation is a proper subset of the other.
isProperSubsetOf :: (Ix a, Ix b)
  => Relation a b -> Relation a b -> Bit
isProperSubsetOf =   uncurry (&&)
                 .  (uncurry (/==) &&& uncurry isSubsetOf)
                 .: (,)

-- | Tests if a relation is empty, i.e., the relation doesn't contain any elements.
null :: (Ix a, Ix b) => Relation a b -> Bit
null = nand . elems

-- | Tests if a relation \( R \subseteq A \times B \) is complete,
-- i.e., \(R = A \times B \).
complete :: (Ix a, Ix b) => Relation a b -> Bit
complete = null . complement

-- | Tests if a relation \( R \subseteq A \times A \) is strongly connected, i.e.,
-- \( R \cup R^{-1} = A \times A \).
total :: (Ix a) => Relation a a -> Bit
total = complete . symmetricClosure

-- | Tests if two relations are disjoint, i.e., 
-- there is no element that is contained in both relations.
disjoint :: (Ix a, Ix b) => Relation a b -> Relation a b -> Bit
disjoint = null .: intersection

-- | Tests if a relation \( R \subseteq A \times A \) is symmetric,
-- i.e., \( R \cup R^{-1} = R \).
symmetric :: (Ix a) => Relation a a -> Bit
symmetric = isSubsetOf <*> mirror

-- | Tests if a relation \( R \subseteq A \times A \) is antisymmetric, i.e.,
-- \( R \cap R^{-1} \subseteq R^{0} \).
antisymmetric :: (Ix a) => Relation a a -> Bit
antisymmetric r = intersection r (mirror r)
                  `isSubsetOf` identity (bounds r)

-- | Tests if a relation \( R \subseteq A \times A \) is irreflexive, i.e.,
-- \( R \cap R^{0} = \emptyset \).
irreflexive :: (Ix a) => Relation a a -> Bit
irreflexive = null . (intersection <*> (identity . bounds))

-- | Tests if a relation \( R \subseteq A \times A \) is reflexive, i.e.,
-- \( R^{0} \subseteq R \).
reflexive :: (Ix a) => Relation a a -> Bit
reflexive = isSubsetOf <*> (identity . bounds)

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | = n \) and
-- \( \forall y \in B: | \{ (x,y) \in R \} | = n \) hold.
regular :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall y \in B: | \{ (x,y) \in R \} | = n \) holds.
regularInDegree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | = n \) holds.
regularOutDegree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall y \in B: | \{ (x,y) \in R \} | \leq n \) holds.
maxInDegree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall y \in B: | \{ (x,y) \in R \} | \geq n \) holds.
minInDegree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | \leq n \) holds.
maxOutDegree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

-- | Given an 'Int' \( n \) and a relation \( R \subseteq A \times B \), check if
-- \( \forall x \in A: | \{ (x,y) \in R \} | \geq n \) holds.
minOutDegree :: (Ix a, Ix b) => Int -> Relation a b -> Bit

regular deg r = regularInDegree deg r && regularOutDegree deg r

regularOutDegree = outDegreeHelper exactly
maxOutDegree = outDegreeHelper atmost
minOutDegree = outDegreeHelper atleast
regularInDegree deg r = regularOutDegree deg $ mirror r
maxInDegree deg r = maxOutDegree deg $ mirror r
minInDegree deg r = minOutDegree deg $ mirror r

outDegreeHelper ::
  (Boolean b, Ix b1, Ix a) =>
  (t -> [Bit] -> b) -> t -> Relation a b1 -> b
outDegreeHelper f deg r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a , c )
    return $ f deg $ do
        y <- range (b,d)
        return $ r ! (x,y)

-- | Tests if a relation \( R \subseteq A \times A \) is transitive, i.e.,
-- \( R \circ R = R \).
--
-- Formula size: linear in \( |A|^3 \)
transitive :: (Ix a)
           => Relation a a -> Bit
transitive r = product r r `isSubsetOf` r
