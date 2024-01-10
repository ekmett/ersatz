{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE DerivingStrategies #-}
#endif

module Ersatz.Relation.Data
  ( -- * The @Relation@ type
    Relation
    -- * Construction
  , relation, symmetricRelation
  , build
  , buildFrom, buildFromM
  , identity, empty, singleton
    -- * Components
  , bounds, domBounds, codBounds
  , (!), member, notMember, indices, assocs, elems
  , domain, codomain, universe
  , universeSize
  , isHomogeneous
  , size
    -- * Pretty printing
  , table
  )  where

import Prelude hiding ( and, (&&), any, not )
import Control.Arrow ((***), (&&&))
import Control.Monad (guard)
import Data.Composition ((.:))

import GHC.Generics (Generic)

import qualified Data.Array as A
import Data.Array ( Array, Ix )

import Ersatz.Bit
import Ersatz.Bits ( Bits, sumBit )
import Ersatz.Codec
import Ersatz.Problem (MonadSAT)
import Ersatz.Variable (exists)



-- | @Relation a b@ represents a binary relation \(R \subseteq A \times B \),
-- where the domain \(A\) is a finite subset of the type @a@,
-- and the codomain \(B\) is a finite subset of the type @b@.
--
-- A relation is stored internally as @Array (a,b) Bit@,
-- so @a@ and @b@ have to be instances of 'Ix',
-- and both \(A\) and \(B\) are intervals.

newtype Relation a b = Relation (Array (a, b) Bit)
#if __GLASGOW_HASKELL__ >= 802
  deriving stock (Generic, Show)
#endif
#if __GLASGOW_HASKELL__ <  802
  deriving (Generic, Show)
#endif

instance (Ix a, Ix b) => Codec (Relation a b) where
  type Decoded (Relation a b) = Array (a, b) Bool
  decode s (Relation a) = decode s a
  encode a = Relation $ encode a


-- | @relation ((amin,bmin),(amax,mbax))@ constructs an indeterminate relation
-- \( R \subseteq A \times B \) where \(A\) is @{amin .. amax}@ and \(B\) is
-- @{bmin .. bmax}@.
relation :: (Ix a, Ix b, MonadSAT s m) =>
  ((a,b),(a,b)) 
  -> m ( Relation a b )
relation bnd = do
  pairs <- sequence $ do
      p <- A.range bnd
      return $ do
          x <- exists
          return ( p, x )
  return $ build bnd pairs

-- | Constructs an indeterminate relation \( R \subseteq B \times B \) that is
-- symmetric, i.e.,
-- \( \forall x, y \in B: ((x,y) \in R) \rightarrow ((y,x) \in R) \).
symmetricRelation ::
  (MonadSAT s m, Ix b) =>
  ((b, b), (b, b)) -- ^ Since a symmetric relation must be homogeneous, the
                   -- domain must equal the codomain. Therefore, given bounds
                   -- @((p,q),(r,s))@, it must hold that @p=q@ and @r=s@.
  -> m (Relation b b)
symmetricRelation bnd = do
  pairs <- sequence $ do
      (p,q) <- A.range bnd
      guard $ p <= q
      return $ do
          x <- exists
          return $   ((p,q), x)
                 : [ ((q,p), x) | p /= q ]
  return $ build bnd $ concat pairs

-- | Constructs a relation \(R \subseteq A \times B \) from a list.
-- 
-- ==== __Example__
--
-- @
-- r = build ((0,'a'),(1,'b')) [ ((0,'a'), true), ((0,'b'), false)
--                         , ((1,'a'), false), ((1,'b'), true) ]
-- @
build :: (Ix a, Ix b)
      => ((a,b),(a,b))
      -> [((a,b), Bit)] -- ^ A list of tuples, where the first element
                        -- represents an element \( (x,y) \in A \times B \) and
                        -- the second element is a positive 'Bit' if
                        -- \( (x,y) \in R \), or a negative 'Bit' if
                        -- \( (x,y) \notin R \).
      -> Relation a b
build = Relation .: A.array

-- | Constructs a relation \(R \subseteq A \times B \) from a function.
buildFrom :: (Ix a, Ix b)
          => ((a,b),(a,b))
          -> ((a,b) -> Bit) -- ^ A function that assigns a 'Bit'-value 
                            -- to each element \( (x,y) \in A \times B \).
          -> Relation a b
buildFrom bnd p = build bnd $ flip map (A.range bnd) $ \ i -> (i, p i)

-- | Constructs an indeterminate relation \(R \subseteq A \times B\) from a
-- function.
buildFromM :: (Ix a, Ix b, MonadSAT s m)
          => ((a,b),(a,b))
          -> ((a,b) -> m Bit)
          -> m (Relation a b)
buildFromM bnd p = do
  pairs <- sequence $ do
      i <- A.range bnd
      return $ do
          x <- p i
          return (i, x)
  return $ build bnd pairs

-- | Constructs the identity relation
-- \(I = \{ (x,x) ~|~ x \in A \} \subseteq A \times A\).
identity :: (Ix a)
         => ((a,a),(a,a)) -- ^ Since the identity relation is homogeneous, the
                          -- domain must equal the codomain. Therefore, given
                          -- bounds @((p,q),(r,s))@, it must hold that @p=q@ and
                          -- @r=s@.
         -> Relation a a
identity ((a,b),(c,d))
  | (a,c) == (b,d) = buildFrom ((a,b),(c,d)) (\ (i,j) -> bool $ i == j)
  | otherwise      = error "The domain must equal the codomain!"

-- | Constructs the empty relation \( \varnothing \subseteq A \times B \).
empty :: (Ix a, Ix b)
      => ((a,b),(a,b))
      -> Relation a b
empty = build <*> map (, false) . A.range

-- | Constructs the relation containing just the provided pair.
singleton :: (Ix a, Ix b)
          => ((a,b),(a,b))
          -> (a,b)
          -> Relation a b
singleton bnds xy = build bnds
                  $ (\ab -> if ab == xy then (xy, true) else (ab, false))
                 <$> A.range bnds

-- | The bounds of the array that correspond to the matrix representation of the
-- given relation.
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false)]
-- >>> bounds r
-- ((0,0),(1,1))
bounds :: (Ix a, Ix b) => Relation a b -> ((a,b),(a,b))
bounds (Relation r) = A.bounds r

-- | The bounds of the domain dimension of the matrix representation of the
-- given relation.
domBounds :: (Ix a, Ix b) => Relation a b -> (a, a)
domBounds = (fst *** fst) . bounds

-- | The bounds of the codomain dimension of the matrix representation of the
-- given relation.
codBounds :: (Ix a, Ix b) => Relation a b -> (b, b)
codBounds = (snd *** snd) . bounds


-- | The list of indices, where each index represents an element
-- \( (x,y) \in A \times B \) that may be contained in the given relation
-- \(R \subseteq A \times B \).
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false)]
-- >>> indices r
-- [(0,0),(0,1),(1,0),(1,1)]
indices :: (Ix a, Ix b) => Relation a b -> [(a, b)]
indices (Relation r) = A.indices r

-- | The list of tuples for the given relation \(R \subseteq A \times B \), 
-- where the first element represents an element \( (x,y) \in A \times B \) 
-- and the second element indicates via a 'Bit' , if \( (x,y) \in R \) or not.
-- 
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false)]
-- >>> assocs r
-- [((0,0),Var (-1)),((0,1),Var 1),((1,0),Var 1),((1,1),Var (-1))]
assocs :: (Ix a, Ix b) => Relation a b -> [((a, b), Bit)]
assocs (Relation r) = A.assocs r

-- | The list of elements of the array
-- that correspond to the matrix representation of the given relation.
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false))]
-- >>> elems r
-- [Var (-1),Var 1,Var 1,Var (-1)]
elems :: (Ix a, Ix b) => Relation a b -> [Bit]
elems (Relation r) = A.elems r

-- | The 'Bit'-value for a given element \( (x,y) \in A \times B \) 
-- and a given relation \(R \subseteq A \times B \) that indicates
-- if \( (x,y) \in R \) or not.
-- 
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false))]
-- >>> r ! (0,0)
-- Var (-1)
-- >>> r ! (0,1)
-- Var 1
(!) :: (Ix a, Ix b) => Relation a b -> (a, b) -> Bit
Relation r ! p = r A.! p

-- | Flipped, non-infix synonym for '!'.
member :: (Ix a, Ix b) => (a, b) -> Relation a b -> Bit
member = flip (!)

-- | Negation of 'member'.
notMember :: (Ix a, Ix b) => (a, b) -> Relation a b -> Bit
notMember = not .: member

-- | The domain \(A\) of a relation \(R \subseteq A \times B\).
domain :: (Ix a, Ix b) => Relation a b -> [a]
domain = A.range . domBounds

-- | The codomain \(B\) of a relation \(R \subseteq A \times B\). 
codomain :: (Ix a, Ix b) => Relation a b -> [b]
codomain = A.range . codBounds

-- | The universe \(A\) of a relation \(R \subseteq A \times A\). 
universe :: Ix a => Relation a a -> [a]
universe r
  | isHomogeneous r = domain r
  | otherwise = error "Relation is not homogeneous!"

-- | The size of the universe \(A\) of a relation \(R \subseteq A \times A\). 
universeSize :: Ix a => Relation a a -> Int
universeSize r 
  | isHomogeneous r = A.rangeSize . domBounds $ r
  | otherwise = error "Relation is not homogeneous!"

-- | Tests if a relation is homogeneous, i.e., if the domain is equal to the
-- codomain.
isHomogeneous :: Ix a => Relation a a -> Bool
isHomogeneous = uncurry (==) . (domBounds &&& codBounds)

-- | The number of pairs \( (x,y) \in R \) for the given relation
-- \( R \subseteq A \times B \).
size :: (Ix a, Ix b) => Relation a b -> Bits
size = sumBit . elems

-- | Print a satisfying assignment from a SAT solver, where the assignment is
-- interpreted as a relation. @putStrLn $ table \</assignment/\>@ corresponds to
-- the matrix representation of this relation.
table :: (Ix a, Ix b)
      => Array (a,b) Bool -> String
table r = unlines $ do
  let ((a,b),(c,d)) = A.bounds r
  x <- A.range (a,c)
  return $ unwords $ do
    y <- A.range (b,d)
    return $ if r A.! (x,y) then "*" else "."
