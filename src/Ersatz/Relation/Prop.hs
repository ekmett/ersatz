{-# LANGUAGE TupleSections #-}
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
, in_domain_def
, in_image
, left_total
, constant
, functional
, injective
, surjective
, bijective
)

where

import Prelude hiding ( all, any, and, or, not, product, (&&) )
import Control.Arrow ( (&&&) )
import Control.Lens ( (<&>) )
import Ersatz.Bit
import Ersatz.Relation.Data
import Ersatz.Relation.Op
import Ersatz.Counting
import Ersatz.Equatable
import Ersatz.Codec ( Codec( encode ) )

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

-- | \( x \) is in the domain of /definition/ of a relation
-- \( R \subseteq A \times B \) iff there is some \( y \in B \) such that
-- \( (x, y) \in R \).
in_domain_def :: ( Ix a, Ix b ) => Relation a b -> a -> Bit
in_domain_def r a =
    any ((r !) . (a,)) $ codomain r

-- | \( y \) is in the image of a relation
-- \( R \subseteq A \times B \) iff there is some \( x \in A \) such that
-- \( (x, y) \in R \).
in_image :: ( Ix a, Ix b ) => Relation a b -> b -> Bit
in_image r b =
    any ((r !) . (,b)) $ domain r

-- | A relation \( R \subseteq A \times B \) is /left-total/ iff its domain of
-- definition is identical to its domain; when \( R \) is a functional relation
-- this means it defines a total rather than partial function.
--
-- For the property asserting that an /order/ relation is total ("strongly
-- connected"), see 'total'.
left_total :: ( Ix a, Ix b ) => Relation a b -> Bit
left_total = uncurry (===) . (card &&& encode . fromIntegral . length . domain)

-- | A relation \( R \subseteq A \times B \) is defined as /constant/ iff the
-- image of its domain of definition is a single value in its codomain.
--
-- Note that this means the empty relation is not considered constant, and that
-- a relation need not be left-total to be classified as constant.
constant :: ( Ix a, Ix b ) => Relation a b -> Bit
constant r = exactly 1 (in_image r <$> codomain r)

-- | Tests if a relation \( R \subseteq A \times B \) defines a (partial) function, i.e.,
-- \( \forall x \in A, y, z \in B: (x,y) \in R \land (x,z) \in R \implies y = z \).
functional :: ( Ix a, Ix b ) => Relation a b -> Bit
functional r =
    all (\x -> atmost 1 
                 $ codomain r <&> ((r !) . (x,)))
        $ domain r

-- | Tests if a relation \( R \subseteq A \times B \) is injective, i.e.
-- \( \forall x, y \in A, z \in B: (x,z) \in R \land (y,z) \in R \implies x = y \).
injective :: ( Ix a, Ix b ) => Relation a b -> Bit
injective r =
    all (\y -> atmost 1 
                 $ domain r <&> ((r !) . (,y)))
        $ codomain r

-- | Tests if a relation \( R \subseteq A \times B \) is surjective, i.e.
-- \( \forall y \in B, \exists x \in A: (x,y) \in R\).
surjective :: ( Ix a, Ix b ) => Relation a b -> Bit
surjective r =
    all (\y -> any (\x -> r ! (x,y))
                   $ domain r)
        $ codomain r

-- | Tests if a relation \( R \subseteq A \times B \) is bijective, i.e.
-- both injective and surjective.
bijective :: ( Ix a, Ix b ) => Relation a b -> Bit
bijective = uncurry (&&) . (injective &&& surjective)
