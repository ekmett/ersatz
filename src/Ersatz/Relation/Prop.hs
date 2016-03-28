
module Ersatz.Relation.Prop

( implies
, symmetric 
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
, disjoint
, equals
)

where

import Prelude hiding ( and, or, not, product )
import Ersatz.Bit
import Ersatz.Relation.Data
import Ersatz.Relation.Op
import Ersatz.Counting

import qualified Prelude

import Control.Monad ( guard )
import Data.Ix

implies :: ( Ix a, Ix b )
        => Relation a b -> Relation a b -> Bit
implies r s = and $ do
    i <- indices r
    return $ or [ not $ r ! i, s ! i ]

empty ::  ( Ix a, Ix b ) 
        => Relation a b -> Bit
empty r = and $ do
    i <- indices r
    return $ not $ r ! i

complete r = empty $ complement r

disjoint r s = empty $ intersection r s
    
equals r s = and [implies r s, implies s r]

symmetric :: ( Ix a) => Relation a a -> Bit
symmetric r = implies r ( mirror r )

irreflexive :: ( Ix a ) => Relation a a -> Bit
irreflexive r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a, c)
    return $ not $ r ! (x,x) 

reflexive :: ( Ix a ) => Relation a a -> Bit
reflexive r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range (a,c)
    return $ r ! (x,x) 

regular, regular_in_degree, regular_out_degree, max_in_degree, min_in_degree, max_out_degree, min_out_degree :: ( Ix a ) => Int -> Relation a a -> Bit

regular deg r = and [ regular_in_degree deg r, regular_out_degree deg r ]

regular_out_degree = out_degree_helper exactly
max_out_degree = out_degree_helper atmost
min_out_degree = out_degree_helper atleast
regular_in_degree deg r = regular_out_degree deg $ mirror r
max_in_degree deg r = max_out_degree deg $ mirror r
min_in_degree deg r = min_out_degree deg $ mirror r


out_degree_helper f deg r = and $ do
    let ((a,b),(c,d)) = bounds r
    x <- range ( a , c )
    return $ f deg $ do 
        y <- range (b,d)
        return $ r !(x,y)

transitive :: ( Ix a ) 
           => Relation a a -> Bit
transitive r = implies (product r r) r

