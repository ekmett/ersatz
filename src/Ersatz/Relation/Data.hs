{-# language TypeFamilies #-}

module Ersatz.Relation.Data ( 
-- * The @Relation@ type
  Relation
-- * Construction
, relation, symmetric_relation
, build
, buildFrom, buildFromM
, identity
-- * Components
, bounds, (!), indices, assocs, elems
, domain, codomain, universe
, universeSize
, is_homogeneous
-- * Pretty printing
, table
)  where

import Prelude hiding ( and, (&&) )

import Ersatz.Bit
import Ersatz.Codec
import Ersatz.Variable (exists)
import Ersatz.Problem (MonadSAT)

import Control.Monad (guard)
import qualified Data.Array as A
import Data.Array ( Array, Ix )


-- | @Relation a b@ represents a binary relation \(R \subseteq A \times B \),
-- where the domain \(A\) is a finite subset of the type @a@,
-- and the codomain \(B\) is a finite subset of the type @b@.
--
-- A relation is stored internally as @Array (a,b) Bit@,
-- so @a@ and @b@ have to be instances of 'Ix',
-- and both \(A\) and \(B\) are intervals.

newtype Relation a b = Relation (Array (a, b) Bit)

instance (Ix a, Ix b) => Codec (Relation a b) where
  type Decoded (Relation a b) = Array (a, b) Bool
  decode s (Relation a) = decode s a
  encode a = Relation $ encode a


-- | @relation ((amin,bmin),(amax,mbax))@ constructs an indeterminate relation \( R \subseteq A \times B \)
-- where \(A\) is @{amin .. amax}@ and \(B\) is @{bmin .. bmax}@.
relation :: ( Ix a, Ix b, MonadSAT s m ) =>
  ((a,b),(a,b)) 
  -> m ( Relation a b )
relation bnd = do
    pairs <- sequence $ do
        p <- A.range bnd
        return $ do
            x <- exists
            return ( p, x )
    return $ build bnd pairs

-- | Constructs an indeterminate relation \( R \subseteq B \times B \)
-- that is symmetric, i.e., \( \forall x, y \in B: ((x,y) \in R) \rightarrow ((y,x) \in R) \).
symmetric_relation ::
  (MonadSAT s m, Ix b) =>
  ((b, b), (b, b)) -- ^ Since a symmetric relation must be homogeneous, the domain must equal the codomain. 
                   -- Therefore, given bounds @((p,q),(r,s))@, it must hold that @p=q@ and @r=s@.
  -> m (Relation b b)
symmetric_relation bnd = do
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
build :: ( Ix a, Ix b )
      => ((a,b),(a,b))
      -> [ ((a,b), Bit ) ] -- ^ A list of tuples, where the first element represents an element
                           -- \( (x,y) \in A \times B \) and the second element is a positive 'Bit'
                           -- if \( (x,y) \in R \), or a negative 'Bit' if \( (x,y) \notin R \).
      -> Relation a b
build bnd pairs = Relation $ A.array bnd pairs

-- | Constructs a relation \(R \subseteq A \times B \) from a function.
buildFrom :: (Ix a, Ix b)
          => ((a,b),(a,b))
          -> ((a,b) -> Bit) -- ^ A function that assigns a 'Bit'-value 
                            -- to each element \( (x,y) \in A \times B \).
          -> Relation a b
buildFrom bnd p = build bnd $ flip map (A.range bnd) $ \ i -> (i, p i)

-- | Constructs an indeterminate relation \(R \subseteq A \times B\) from a function.
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

-- | Constructs the identity relation \(I = \{ (x,x) ~|~ x \in A \} \subseteq A \times A\).
identity :: (Ix a)
         => ((a,a),(a,a)) -- ^ Since the identity relation is homogeneous, the domain must equal the codomain. 
                          -- Therefore, given bounds @((p,q),(r,s))@, it must hold that @p=q@ and @r=s@.
         -> Relation a a
identity ((a,b),(c,d))
    | (a,c) == (b,d) = buildFrom ((a,b),(c,d)) (\ (i,j) -> bool $ i == j)
    | otherwise      = error "The domain must equal the codomain!"


-- | The bounds of the array that correspond to the matrix representation of the given relation.
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false)]
-- >>> bounds r
-- ((0,0),(1,1))
bounds :: (Ix a, Ix b) => Relation a b -> ((a,b),(a,b))
bounds ( Relation r ) = A.bounds r

-- | The list of indices, where each index represents an element \( (x,y) \in A \times B \) 
-- that may be contained in the given relation \(R \subseteq A \times B \).
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false)]
-- >>> indices r
-- [(0,0),(0,1),(1,0),(1,1)]
indices :: (Ix a, Ix b) => Relation a b -> [(a, b)]
indices ( Relation r ) = A.indices r

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
assocs ( Relation r ) = A.assocs r

-- | The list of elements of the array
-- that correspond to the matrix representation of the given relation.
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false))]
-- >>> elems r
-- [Var (-1),Var 1,Var 1,Var (-1)]
elems :: (Ix a, Ix b) => Relation a b -> [Bit]
elems ( Relation r ) = A.elems r

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

-- | The domain \(A\) of a relation \(R \subseteq A \times B\). 
domain :: (Ix a, Ix b) => Relation a b -> [a]
domain r =
  let ((x,_),(x',_)) = bounds r
  in A.range (x,x')

-- | The codomain \(B\) of a relation \(R \subseteq A \times B\). 
codomain :: (Ix a, Ix b) => Relation a b -> [b]
codomain r =
  let ((_,y),(_,y')) = bounds r
  in A.range (y,y')

-- | The universe \(A\) of a relation \(R \subseteq A \times A\). 
universe :: Ix a => Relation a a -> [a]
universe r
  | is_homogeneous r = domain r
  | otherwise = error "Relation is not homogeneous!"

-- | The size of the universe \(A\) of a relation \(R \subseteq A \times A\). 
universeSize :: Ix a => Relation a a -> Int
universeSize r 
  | is_homogeneous r =
      let ((a,_),(c,_)) = bounds r
      in A.rangeSize (a,c)
  | otherwise = error "Relation is not homogeneous!"

-- | Tests if a relation is homogeneous, i.e., if the domain is equal to the codomain.
is_homogeneous :: Ix a => Relation a a -> Bool
is_homogeneous r =
  let ((a,b),(c,d)) = bounds r 
  in (a == b) && (c == d)


-- | Print a satisfying assignment from a SAT solver, where the assignment is interpreted as a relation.
-- @putStrLn $ table \</assignment/\>@ corresponds to the matrix representation of this relation.
table :: (Ix a, Ix b)
      => Array (a,b) Bool -> String
table r = unlines $ do
    let ((a,b),(c,d)) = A.bounds r
    x <- A.range (a,c)
    return $ unwords $ do
        y <- A.range (b,d)
        return $ if r A.! (x,y) then "*" else "."



