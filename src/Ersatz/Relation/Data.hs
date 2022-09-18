{-# language TypeFamilies #-}

module Ersatz.Relation.Data ( 
-- * The 'Relation' type
  Relation
-- * Construction
, relation, symmetric_relation
, build
, buildFrom
, identity
-- * Components
, bounds, (!), indices, assocs, elems
-- *
, table
)  where

import Prelude hiding ( and )

import Ersatz.Bit
import Ersatz.Codec
import Ersatz.Variable (exists)
import Ersatz.Problem (MonadSAT)

import Control.Monad (guard)
import qualified Data.Array as A
import Data.Array ( Array, Ix )


-- | Here, a 'Relation' is thought of as a binary relation \(R \subseteq A \times B \)
-- with domain \(A\) and codomain \(B\). All \( x \in A \) are represented by values in @a@
-- and all \( y \in B \) by values in @b@.
-- Since a relation is stored internally as a matrix, i.e., as an 'Array' of 'Bit'-values, 
-- @a@ and @b@ have to be instances of 'Ix'.
--
-- It can also be thought of a 'Relation', for example, as an abstract rewriting system
-- or a directed graph.
newtype Relation a b = Relation (A.Array (a, b) Bit)

instance (Ix a, Ix b) => Codec (Relation a b) where
  type Decoded (Relation a b) = A.Array (a, b) Bool
  decode s (Relation a) = decode s a
  encode a = Relation $ encode a


-- | Constructs an indeterminate relation \( R \subseteq A \times B \).
relation :: ( Ix a, Ix b, MonadSAT s m ) =>
  ((a,b),(a,b)) -- ^ A tuple of bounds, which are the lowest and highest indices in the array, 
                -- that correspond to the matrix representation of \(R\).
                -- Each index within the bounds represents an element \( (x,y) \in A \times B \) 
                -- that may be contained in \(R\).
  -> m ( Relation a b ) -- ^ An indeterminate relation \(R\) with a fixed domain \(A\) and codomain \(B\). 
                        -- The relation carries a SAT state and can be evaluated by a 'Ersatz.Solution.Solver'.
relation bnd = do
    pairs <- sequence $ do
        p <- A.range bnd
        return $ do
            x <- exists
            return ( p, x )
    return $ build bnd pairs

-- | Constructs an indeterminate relation \( R \subseteq A \times A \) with the constraint, 
-- that it is symmetric, i.e., \( \forall x, y \in A: ((x,y) \in R) \rightarrow ((y,x) \in R) \).
--
-- It can be thought of a symmetric relation as an undirected graph.
symmetric_relation ::
  (MonadSAT s m, Ix b) =>
  ((b, b), (b, b)) -- ^ Since a symmetric relation must be homogeneous, the domain must equals the codomain, 
                   -- hence for the bounds @((p,q),(r,s))@ must hold @p=q@ and @r=s@.
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
-- r = build ((0,'a'),(1,'b')) [((0,'a'), true), ((0,'b'), false), 
--                          ((1,'a'), false), ((1,'b'), true))]
-- @
build :: ( Ix a, Ix b )
      => ((a,b),(a,b))
      -> [ ((a,b), Bit ) ] -- ^ A list of tuples, where the first element represents an element
                           -- \( (x,y) \in A \times B \) and the second element is a positive 'Bit',
                           -- if \( (x,y) \in R \) or a negative 'Bit', if \( (x,y) \notin R \).
      -> Relation a b
build bnd pairs = Relation $ A.array bnd pairs

-- | Constructs a relation \(R \subseteq A \times B \) from a function.
buildFrom :: (Ix a, Ix b)
          => (a -> b -> Bit) -- ^ A function with the specified signature, that assigns a 'Bit'-value 
                             -- to each element \( (x,y) \in A \times B \).
          -> ((a,b),(a,b))
          -> Relation a b
buildFrom p bnd = build bnd $ flip map (A.range bnd) $ \ (i,j) -> ((i, j), p i j)

-- | Constructs the identity relation \(I \subseteq A \times A, I = \{ (x,x) ~|~ x \in A \} \).
identity :: (Ix a)
         => ((a,a),(a,a)) -- ^ Since the identity relation is homogeneous, the domain must equals the codomain, 
                          -- hence for the bounds @((p,q),(r,s))@ must hold @p=q@ and @r=s@.
         -> Relation a a
identity = buildFrom (\ i j -> bool $ i == j)


-- | The bounds of the array, that correspond to the matrix representation of the given relation.
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false))]
-- >>> bounds r
-- ((0,0),(1,1))
bounds :: (Ix a, Ix b) => Relation a b -> ((a,b),(a,b))
bounds ( Relation r ) = A.bounds r

-- | The list of indices, where each index represents an element \( (x,y) \in A \times B \) 
-- that may be contained in the given relation \(R \subseteq A \times B \).
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false))]
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
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false))]
-- >>> assocs r
-- [((0,0),Var (-1)),((0,1),Var 1),((1,0),Var 1),((1,1),Var (-1))]
assocs :: (Ix a, Ix b) => Relation a b -> [((a, b), Bit)]
assocs ( Relation r ) = A.assocs r

-- | The list of elements of the array,
-- that correspont to the matrix representation of the given relation.
--
-- ==== __Example__
--
-- >>> r = build ((0,0),(1,1)) [((0,0), false), ((0,1), true), ((1,0), true), ((1,1), false))]
-- >>> elems r
-- [Var (-1),Var 1,Var 1,Var (-1)]
elems :: (Ix a, Ix b) => Relation a b -> [Bit]
elems ( Relation r ) = A.elems r

-- | The 'Bit'-value for a given element \( (x,y) \in A \times B \) 
-- and a given relation \(R \subseteq A \times B \) that indicates,
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

-- | To print a satisfying assignment from a SAT solver, where the assignment is interpreted as a relation.
-- @putStrLn $ table \</assignment/\>@ corresponds to the matrix representation of this relation.
table :: (Enum a, Ix a, Enum b, Ix b)
      => Array (a,b) Bool -> String
table r = unlines $ do
    let ((a,b),(c,d)) = A.bounds r
    x <- [ a .. c ]
    return $ unwords $ do
        y <- [ b .. d ]
        return $ if r A.! (x,y) then "*" else "."




