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


module Ersatz.Relation
( module Ersatz.Relation.Data
, module Ersatz.Relation.Op
, module Ersatz.Relation.Prop
) where

import Ersatz.Relation.Data
import Ersatz.Relation.Op
import Ersatz.Relation.Prop

