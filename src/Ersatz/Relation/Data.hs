{-# language TypeFamilies #-}

module Ersatz.Relation.Data ( Relation
, relation, symmetric_relation
, build
, buildFrom
, identity
, bounds, (!), indices, assocs, elems
, table
)  where

import Ersatz.Bit
import Ersatz.Codec
import Ersatz.Variable (exists)
import Ersatz.Problem (HasSAT)

import qualified Data.Array as A
import Data.Array ( Array, Ix )
import Control.Monad.State

newtype Relation a b = Relation (A.Array (a, b) Bit)

instance (Ix a, Ix b) => Codec (Relation a b) where
  type Decoded (Relation a b) = A.Array (a, b) Bool
  decode s (Relation a) = decode s a
  encode a = Relation $ encode a

relation :: ( Ix a, Ix b, MonadState s m, HasSAT s )
         => ((a,b),(a,b)) -> m ( Relation a b )
relation bnd = do
    pairs <- sequence $ do
        p <- A.range bnd
        return $ do
            x <- exists
            return ( p, x )
    return $ build bnd pairs

symmetric_relation ::
  (HasSAT s, MonadState s m, Ix b) =>
  ((b, b), (b, b)) -> m (Relation b b)
symmetric_relation bnd = do
    pairs <- sequence $ do
        (p,q) <- A.range bnd
        guard $ p <= q
        return $ do
            x <- exists
            return $   ((p,q), x)
                   : [ ((q,p), x) | p /= q ]
    return $ build bnd $ concat pairs

build :: ( Ix a, Ix b )
      => ((a,b),(a,b))
      -> [ ((a,b), Bit ) ]
      -> Relation a b
build bnd pairs = Relation $ A.array bnd pairs

buildFrom :: (Ix a, Ix b)
          => (a -> b -> Bit) -> ((a,b),(a,b)) -> Relation a b
buildFrom p bnd = build bnd $ flip map (A.range bnd) $ \ (i,j) ->
    ((i,j), p i j)

identity :: (Ix a)
         => ((a,a),(a,a)) -> Relation a a
identity = buildFrom (\ i j -> bool $ i == j)


bounds :: (Ix a, Ix b) => Relation a b -> ((a,b),(a,b))
bounds ( Relation r ) = A.bounds r

indices :: (Ix a, Ix b) => Relation a b -> [(a, b)]
indices ( Relation r ) = A.indices r

assocs :: (Ix a, Ix b) => Relation a b -> [((a, b), Bit)]
assocs ( Relation r ) = A.assocs r

elems :: (Ix a, Ix b) => Relation a b -> [Bit]
elems ( Relation r ) = A.elems r

(!) :: (Ix a, Ix b) => Relation a b -> (a, b) -> Bit
Relation r ! p = r A.! p


table :: (Enum a, Ix a, Enum b, Ix b)
      => Array (a,b) Bool -> String
table r = unlines $ do
    let ((a,b),(c,d)) = A.bounds r
    x <- [ a .. c ]
    return $ unwords $ do
        y <- [ b .. d ]
        return $ if r A.! (x,y) then "*" else "."




