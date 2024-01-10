{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE DerivingStrategies #-}
#endif


-- | This module represents symbolic subsets of a finite subset of some type @a@.
--
-- Internally it is currently @newtype Set a = Set { unSet :: Relation a () }@, and hence
-- under that is an @Array (a, ()) Bit@ — a unary encoding of a set as a bitmap.
--
-- Accordingly, most functions are calls to corresponding 'Data.Relation' operations;
-- see documentation there for formula sizes.
--
-- For the same reasons, if you expect to use both 'Ersatz.Relation' and 'Ersatz.Set' in
-- the same namespace, you will need to import them qualified or selectively hide
-- overlapping names.
--
-- The API also mirrors much of the @containers@ @Set@ API, so again, if you
-- need access to both this module and a concrete @Set@ module in the same namespace, expect
-- to qualify imports or selectively hide overlapping names as appropriate.

module Ersatz.Relation.Set
  ( Set
    -- * Construction
  , set
  , build
  , buildFrom
  , buildFromM
  , universe'
  , empty
  , singleton

    -- ** Alternative concretizations
  , AsOrdSet  (AsOrdSet , unAsOrdSet )
  , AsIntSet  (AsIntSet , unAsIntSet )
  , AsHashSet (AsHashSet, unAsHashSet)

    -- * Concrete queries
  , bounds
  , indices
  , assocs
  , elems
  , universe
  , universeSize

    -- * Symbolic queries/assertions
  , (!)
  , member
  , notMember
  , size
  , isSubsetOf
  , isProperSubsetOf
  , disjoint
  , null

  -- * Operations
  , insert
  , delete
  , map
  , filter
  , partition

    -- ** Boolean operations
  , complement
  , union
  , intersection
  , difference
  , symmetricDifference
  , cartesianProduct

    -- ** Interfacing with 'Relation's
  , dom
  , img
  , toPartialRel

  ) where

import Prelude hiding
  ( null
  , not
  , and
  , (&&)
  , (||)
  , any
  , map
  , filter
  )

import Data.Composition
  ( (.:)
  )
import Control.Arrow
  ( (***)
  , (&&&)
  , first
  )

import GHC.Generics (Generic)
import Data.Coerce (coerce)

import qualified Data.Ix    as Ix
import qualified Data.Array as A
import Data.Array (Array , Ix)

import qualified Data.List     as L
import qualified Data.Set      as OrdSet
import qualified Data.IntSet   as IntSet
import qualified Data.HashSet  as HashSet
import Data.Hashable (Hashable)

import Ersatz.Bit
  ( Bit
  , bool
  , true
  , false
  , any
  , not
  , and
  , (&&)
  , (||)
  )
import Ersatz.Bits (Bits)
import Ersatz.Codec
  ( Codec ( Decoded
          , decode
          , encode
          )
  )
import Ersatz.Equatable (Equatable , (===) , (/==))
import Ersatz.Problem (MonadSAT)
import Ersatz.Solution (Solution)

import qualified Ersatz.Relation.Op   as RO
import qualified Ersatz.Relation.Prop as RP
import qualified Ersatz.Relation.Data as RD
import Ersatz.Relation.Data
  ( Relation
  , relation
  , domain
  , codomain
  , domBounds
  , codBounds
  )


-- Helpers for shuffling between 'Set a' and 'Relation a ()' or components

pad :: a -> (a, ())
pad = (,())

unPad :: (a, ()) -> a
unPad = fst

relBounds_ :: (a,a) -> ((a, ()), (a, ()))
relBounds_ = pad *** pad

unRelBounds_ :: ((a, ()), (a, ())) -> (a,a)
unRelBounds_ = unPad *** unPad

relAssoc_ :: (a, b) -> ((a, ()), b)
relAssoc_ = first pad

unRelAssoc_ :: ((a, ()), b) -> (a, b)
unRelAssoc_ = first unPad

lift_ :: (a -> b) -> ((a, ()) -> b)
lift_ = uncurry . flip . const

relArray_ :: (Ix a) => Array a b -> Array (a, ()) b
relArray_ = uncurry A.array
          . (   relBounds_     . A.bounds
            &&& fmap relAssoc_ . A.assocs)

unRelArray_ :: (Ix a) => Array (a, ()) b -> Array a b
unRelArray_ = uncurry A.array
            . (   unRelBounds_     . A.bounds
              &&& fmap unRelAssoc_ . A.assocs)



-- Constructors

-- | @Set a@ represents a finite subset \( S \subseteq A \) of values of type
-- @a@.
newtype Set a = Set { unSet :: Relation a () }
#if __GLASGOW_HASKELL__ >= 802
  deriving stock (Generic, Show)
#endif
#if __GLASGOW_HASKELL__ <  802
  deriving (Generic, Show)
#endif

instance (Ix a) => Equatable (Set a) where
  r === s
    | bounds r /= bounds s = error "Relations don't have the same bounds!"
    | otherwise =
      and $ zipWith (===) (elems r) (elems s)
  r /== s = not $ r === s


instance (Ix a) => Codec (Set a) where
  type Decoded (Set a) = Array a Bool

  decode :: Ix a => Solution -> Set a -> Maybe (Array a Bool)
  decode sol = fmap unRelArray_ . decode sol . unSet

  encode :: Ix a => Array a Bool -> Set a
  encode = Set . encode . relArray_

-- | A newtype that associates this module's 'Relation'-based encoding of a
-- symbolic set with an 'Ord'-based concrete set from @containers@.
newtype AsOrdSet a = AsOrdSet { unAsOrdSet :: Set a }
#if __GLASGOW_HASKELL__ >= 802
  deriving stock (Generic, Show)
#endif
#if __GLASGOW_HASKELL__ <  802
  deriving (Generic, Show)
#endif

-- | 'Bounded' is only necessary for 'encode': it defines the bounds for the
-- underlying 'Array'.
instance (Bounded a, Ix a) => Codec (AsOrdSet a) where
  type Decoded (AsOrdSet a) = OrdSet.Set a

  decode :: (Ix a) => Solution -> AsOrdSet a -> Maybe (OrdSet.Set a)
  decode sol = fmap ( OrdSet.fromList
                    . L.map fst . L.filter snd
                    . A.assocs  . unRelArray_ )
             . decode sol
             . unSet
             . unAsOrdSet

  encode :: (Ix a) => OrdSet.Set a -> AsOrdSet a
  encode =
    let bnds = (minBound, maxBound)
        arrayFrom f = A.array bnds
                    $ (id &&& f) <$> Ix.range bnds
    in  AsOrdSet . Set
      . encode . relArray_
      . arrayFrom . flip L.elem
      . OrdSet.toList

-- | A newtype that associates this module's 'Relation'-based encoding of a
-- symbolic set with an 'IntSet'-based concrete set from @containers@.
newtype AsIntSet a = AsIntSet { unAsIntSet :: Set a }
#if __GLASGOW_HASKELL__ >= 802
  deriving stock (Generic, Show)
#endif
#if __GLASGOW_HASKELL__ <  802
  deriving (Generic, Show)
#endif

-- | 'Bounded' is only necessary for 'encode': it defines the bounds for the
-- underlying 'Array' and has no effect on 'decode'.
instance (Bounded a, Ix a, Integral a) => Codec (AsIntSet a) where
  type Decoded (AsIntSet a) = IntSet.IntSet

  decode :: (Bounded a, Ix a) => Solution -> AsIntSet a -> Maybe IntSet.IntSet
  decode sol = fmap ( IntSet.fromList
                    . L.map (fromIntegral . fst) . L.filter snd
                    . A.assocs  . unRelArray_ )
             . decode sol
             . unSet
             . unAsIntSet

  encode :: (Bounded a, Ix a) => IntSet.IntSet -> AsIntSet a
  encode =
    let bnds = (minBound, maxBound)
        arrayFrom f = A.array bnds
                    $ (id &&& f) <$> Ix.range bnds
    in  AsIntSet . Set
      . encode . relArray_
      . arrayFrom . flip L.elem
      . L.map fromIntegral . IntSet.toList


-- | A newtype that associates this module's 'Relation'-based encoding of a
-- symbolic set with a 'Hashable'-based concrete set from
-- @unordered-containers@.
newtype AsHashSet a = AsHashSet { unAsHashSet :: Set a }
#if __GLASGOW_HASKELL__ >= 802
  deriving stock (Generic, Show)
#endif
#if __GLASGOW_HASKELL__ <  802
  deriving (Generic, Show)
#endif

-- | 'Bounded' is only necessary for 'encode': it defines the bounds for the
-- underlying 'Array' and has no effect on 'decode'.
instance (Hashable a, Bounded a, Ix a) => Codec (AsHashSet a) where
  type Decoded (AsHashSet a) = HashSet.HashSet a

  decode :: (Hashable a, Ix a) => Solution -> AsHashSet a -> Maybe (HashSet.HashSet a)
  decode sol = fmap ( HashSet.fromList
                    . L.map fst . L.filter snd
                    . A.assocs  . unRelArray_ )
             . decode sol
             . unSet
             . unAsHashSet

  encode :: (Hashable a, Ix a) => HashSet.HashSet a -> AsHashSet a
  encode =
    let bnds = (minBound, maxBound)
        arrayFrom f = A.array bnds
                    $ (id &&& f) <$> Ix.range bnds
    in  AsHashSet . Set
      . encode . relArray_
      . arrayFrom . flip L.elem
      . HashSet.toList


-- | Using the provided bounds \( (a_{min}, a_{max}) \), construct a symbolic
-- Set \( S \subseteq A \) for the universe \( A = a_{min} \ldots a_{max} \).
set :: (Ix a, MonadSAT s m)
  => (a,a)
  -> m (Set a)
set = fmap Set . relation . relBounds_

-- | Construct a set \(S \subseteq A \) from an association list.
build :: (Ix a)
  => (a,a)
  -> [(a,Bit)]
  -> Set a
build =  Set
      .  uncurry RD.build
      .  (relBounds_ *** fmap relAssoc_)
      .: (,)

-- | Construct a set \(S \subseteq A \) from a predicate.
buildFrom :: (Ix a)
  => (a,a)
  -> (a -> Bit)
  -> Set a
buildFrom =  Set
          .  uncurry RD.buildFrom
          .  (relBounds_ *** lift_)
          .: (,)

-- | Construct an indeterminate set \(S \subseteq A \) from a predicate.
buildFromM :: (Ix a, MonadSAT s m)
  => (a,a)
  -> (a -> m Bit)
  -> m (Set a)
buildFromM =  fmap Set
           .  uncurry RD.buildFromM
           .  (relBounds_ *** lift_)
           .: (,)

-- | Construct a symbolic set \( \Omega \subseteq A \) from bounds where, by
-- construction, every element within the bounds is contained in the set.
universe' :: (Ix a) => (a,a) -> Set a
universe' = build
         <*> fmap (, true) . Ix.range

-- | Construct a symbolic set \( \varnothing \subseteq A \) from bounds where, by
-- construction, no elements are contained in the set.
empty :: (Ix a) => (a,a) -> Set a
empty = build
     <*> fmap (, false) . Ix.range

-- | Construct a symbolic set \( \{a\} \subseteq A \) from bounds where, by
-- construction, exactly one particular element \( a \) is contained in the set.
singleton :: (Ix a) => (a,a) -> a -> Set a
singleton bnd a = build bnd
                $ L.map (id &&& (bool . (a ==)))
                        $ Ix.range bnd



-- Concrete queries

-- | The bounds of the array representing the set.
bounds :: (Ix a) => Set a -> (a,a)
bounds = unRelBounds_ . RD.bounds . coerce

-- | The list of indices of the array representing the set.
indices :: (Ix a) => Set a -> [a]
indices = fmap unPad . RD.indices . coerce

-- | The list of tuples for the given set \(S \subseteq A \), where the first
-- element is an index and the second element is a 'Bit' indicating whether the
-- element is contained in the represented set.
assocs :: (Ix a) => Set a -> [(a, Bit)]
assocs = fmap unRelAssoc_ . RD.assocs . coerce

-- | The list of elements of the array that represents the given set.
elems :: (Ix a) => Set a -> [Bit]
elems = RD.elems . coerce

-- | Synonym for 'indices', provided for parallelism with 'Data.Relation'.
universe :: (Ix a) => Set a -> [a]
universe = RD.domain . coerce

-- | The size of the universe that a symbolic (sub)set is a subset of.
universeSize :: (Ix a) => Set a -> Int
universeSize = A.rangeSize . bounds



-- Symbolic queries/assertions

-- | The number of elements \( |S| \) contained in the given symbolic subset
-- \( S \subseteq A \).
size :: (Ix a) => Set a -> Bits
size = RD.size . coerce

-- | @s ! a@ tests if \( a \in S \).
(!) :: (Ix a) => Set a -> a -> Bit
(!) =  uncurry (RD.!)
    .  (coerce *** pad)
    .: (,)

-- | Flipped, non-infix synonym for '!'.
member :: (Ix a) => a -> Set a -> Bit
member = flip (!)

-- | Is the element not in the set?
notMember :: (Ix a) => a -> Set a -> Bit
notMember = not .: member

-- | Tests if the set is empty.
null :: (Ix a) => Set a -> Bit
null = RP.null . coerce

-- | @s \`isSubsetOf\` r@ tests if \( S \subseteq R \).
isSubsetOf :: (Ix a) => Set a -> Set a -> Bit
isSubsetOf =  uncurry RP.implies
           .  (coerce *** coerce)
           .: (,)

-- | @s \`subsetProper\` r@ tests if \( S \subset R \).
isProperSubsetOf :: (Ix a) => Set a -> Set a -> Bit
isProperSubsetOf = not . null .: difference

-- | @s \`disjoint\` r@ tests if \( S \cap R = \varnothing \).
disjoint :: (Ix a) => Set a -> Set a -> Bit
disjoint = null .: intersection



-- Operations

-- | Given a set \( S \subseteq A \) and an element \( a \), construct
-- a new set \( S \cup \{a\} \).
insert :: (Ix a) => a -> Set a -> Set a
insert a s
  | not (bounds s `Ix.inRange` a) = error "Element out of bounds of the set!"
  | otherwise = buildFrom (bounds s)
                          (\x -> bool (x == a) || x `member` s)

-- | Given a set \( S \subseteq A \) and an element \( a \), construct
-- a new set \( S \setminus \{a\} \).
delete :: (Ix a) => a -> Set a -> Set a
delete a s
  | not (bounds s `Ix.inRange` a) = error "Element out of bounds of the set!"
  | otherwise = buildFrom (bounds s)
                          (\x -> bool (x /= a) && x `member` s)


-- | 'fmap' function for symbolic sets; note that the bounds of the new 'Set'
-- are defined by applying the provided function to the bounds of the original
-- 'Set'.
map :: (Ix a, Ix b) => (a -> b) -> Set a -> Set b
map f =  build . (f *** f) . bounds
     <*> (fmap (first f) . assocs)

-- | Restrict a set to only the subset that satisfies the provided predicate.
filter :: (Ix a) => (a -> Bit) -> Set a -> Set a
filter p s = buildFrom (bounds s) ( uncurry (&&)
                                  . ((s !) &&& p))

-- | Partition a set into the subset that satisfies a predicate vs. the subset
-- that does not.
partition :: (Ix a) => (a -> Bit) -> Set a -> (Set a, Set a)
partition p = filter p &&& filter (not . p)


-- | Given a subset \( S \subseteq \Omega \) drawn form some finite universe
-- \( \Omega \subset A \), construct the complement of \( S \) with respect to
-- \( \Omega \), i.e. \( \Omega \setminus S \).
complement :: (Ix a) => Set a -> Set a
complement = Set
           . RO.complement
           . coerce

-- | Construct the union \( S \cup R \) of the two provided sets.
union :: (Ix a) => Set a -> Set a -> Set a
union =  Set
      .  uncurry RO.union
      .  (coerce *** coerce)
      .: (,)

-- | Construct the intersection \( S \cap R \) of the two provided sets.
intersection :: (Ix a) => Set a -> Set a -> Set a
intersection = Set
             .  uncurry RO.intersection
             .  (coerce *** coerce)
             .: (,)

-- | Construct the difference \( S \setminus R \) of the two provided sets.
difference :: (Ix a) => Set a -> Set a -> Set a
difference =  Set
           .  uncurry RO.difference
           .  (coerce *** coerce)
           .: (,)

-- | Construct the symmetric difference
-- \( (S \setminus R) \cup (R \setminus S) \) of the two provided sets.
symmetricDifference :: (Ix a) => Set a -> Set a -> Set a
symmetricDifference =  Set
                    .  uncurry RO.symmetricDifference
                    .  (coerce *** coerce)
                    .: (,)

-- | Construct the cartesian product \( S \times R \) of the two provided sets.
cartesianProduct :: (Ix a, Ix b) => Set a -> Set b -> Relation a b
cartesianProduct s r =
  let (aMin,aMax) = bounds s
      (bMin,bMax) = bounds r
  in  RD.buildFrom ((aMin,bMin),(aMax,bMax))
                   (  uncurry (&&)
                    . ((s !) *** (r !)))



-- Interfacing Relation values and Set values

-- | Construct a set representing the domain of definition of a 'Relation'.
dom :: (Ix a, Ix b) => Relation a b -> Set a
dom r =
  let in_dom_def x = any ((r RD.!) . (x,))
                         $ codomain r
  in  buildFrom (domBounds r) in_dom_def

-- | Construct a set representing the image of the domain of definition of a
-- 'Relation'.
img :: (Ix a, Ix b) => Relation a b -> Set b
img r =
  let in_cod_def y = any ((r RD.!) . (,y))
                         $ domain r
  in  buildFrom (codBounds r) in_cod_def

-- | Given a 'Set' \( S \subseteq A \), construct the corresponding partial
-- 'Relation' \( R \subseteq A \times A \) by restricting the 'identity'
-- relation on \( A \) to \( S \).
toPartialRel :: (Ix a) => Set a -> Relation a a
toPartialRel s =
  let dup                 = id &&& id
      homRelBounds_       = (dup *** dup) . bounds $ s
      p             (x,y) = x `member` s && bool (x == y)
  in  RD.buildFrom homRelBounds_ p
