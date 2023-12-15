-- | Copyright: Johannes Waldmann, Antonia Swiridoff
-- License: BSD3
--
-- The type @Relation a b@ represents relations
-- between finite subsets of type @a@ and of type @b@.
--
-- A relation is stored internally as @Array (a,b) Bit@,
-- and some methods of @Data.Array@ are provided for managing indices and elements.
--
-- These are rarely needed, because we provide operations and properties
-- in a point-free style, that is, without reference to individual indices and elements.
--
-- Unless otherwise specified, the size of the generated formulas is linear in \( |A| \cdot |B| \),
-- where \(A\) and \(B\) represent the domain and codomain of the involved relation(s).


module Ersatz.Relation
( module Ersatz.Relation.Data
, module Ersatz.Relation.Op
, module Ersatz.Relation.Prop
, module Ersatz.Relation.ARS
) where

import Ersatz.Relation.Data
import Ersatz.Relation.Op
import Ersatz.Relation.Prop
import Ersatz.Relation.ARS

