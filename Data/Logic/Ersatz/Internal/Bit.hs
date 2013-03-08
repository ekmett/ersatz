{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.Logic.Ersatz.Internal.Bit
  ( Boolean(..)
  , Equatable(..)
  , Bit(..)
  , Circuit(..)
  , assert
  ) where

import Prelude hiding ((&&),(||),not,and,or)
import qualified Prelude

import Control.Applicative
import Data.Foldable (Foldable)
import qualified Data.HashMap.Lazy as HashMap
import Data.Traversable (Traversable, traverse)

import Data.Logic.Ersatz.Encoding (Encoding(..))
import Data.Logic.Ersatz.Internal.Problem
import Data.Logic.Ersatz.Internal.StableName
import Data.Logic.Ersatz.Solution

infix  4 ===, /==
infixr 3 &&
infixr 2 ||
infixr 0 ==>

class Boolean t where
  bool :: Bool -> t
  true :: t
  false :: t
  (&&) :: t -> t -> t
  (||) :: t -> t -> t
  (==>) :: t -> t -> t
  not :: t -> t
  and :: [t] -> t
  or :: [t] -> t
  nand :: [t] -> t
  nor :: [t] -> t
  xor :: t -> t -> t

  -- map a 2x2 multiplexor over the functor
  choose :: Functor f => t -> f (t,t) -> f t
  choose s = fmap (\(a,b) -> (a && not s) || (b && s))

  x ==> y = not x || y
  nand = not . and
  nor = not . or

instance Boolean Bool where
  bool = id
  true = True
  false = False

  (&&) = (Prelude.&&)
  (||) = (Prelude.||)

  not = Prelude.not

  and = Prelude.and
  or = Prelude.or

  False `xor` False = False
  False `xor` True  = True
  True `xor` False  = True
  True `xor` True   = False

  choose False = fmap fst
  choose True = fmap snd

data Circuit c
  = And [c]
  | Or [c]
  | Xor c c
  | Mux c c c  -- ^ False branch, true branch, predicate/selector branch
  | Not c
  | Var !Lit
  deriving (Show, Functor, Foldable, Traversable)

-- does this have to be data?
newtype Bit = Bit (Circuit Bit)
  deriving Show

-- a Bit you don't assert is actually a boolean function that you can evaluate later after compilation
instance Encoding Bit where
  type Decoded Bit = Bool
  decode sol b@(Bit c) = do
    sn <- makeStableName' b
    case HashMap.lookup sn (solSNMap sol) of
      v@(Just _) -> return v
      Nothing ->
        case c of
          And cs  -> andMaybeBools <$> traverse (decode sol) cs
          Or cs   -> orMaybeBools  <$> traverse (decode sol) cs
          Xor x y -> liftA2 xor <$> decode sol x <*> decode sol y
          Mux cf ct cp -> do mp <- decode sol cp
                             case mp of
                               Just p  -> decode sol (if p then ct else cf)
                               Nothing -> return Nothing
          Not c'  -> fmap not <$> decode sol c'
          Var l   -> decode sol l

    where
      andMaybeBools :: [Maybe Bool] -> Maybe Bool
      andMaybeBools mbs
        | any not knowns = Just False  -- One is known to be false.
        | null unknowns  = Just True   -- All are known to be true.
        | otherwise      = Nothing     -- Unknown.
        where
          (unknowns, knowns) = partitionMaybes mbs

      orMaybeBools :: [Maybe Bool] -> Maybe Bool
      orMaybeBools mbs
        | or knowns     = Just True   -- One is known to be true.
        | null unknowns = Just False  -- All are known to be false.
        | otherwise     = Nothing     -- Unknown.
        where
          (unknowns, knowns) = partitionMaybes mbs

      partitionMaybes :: [Maybe a] -> ([()], [a])
      partitionMaybes = foldr (maybe nothing just) ([],[])
        where
          nothing ~(ns, js) = (():ns, js)
          just a  ~(ns, js) = (ns,    a:js)

instance Boolean Bit where
  -- improve the stablemap this way
  bool t | t = true
         | otherwise = false
  true  = Bit (Var (lit True))
  false = Bit (Var (lit False))
  Bit (And as) && Bit (And bs) = and (as ++ bs)
  Bit (And as) && b            = and (as ++ [b])
  a            && Bit (And bs) = and (a : bs)
  a            && b            = and [a,b]
  Bit (Or as) || Bit (Or bs) = or (as ++ bs)
  Bit (Or as) || b           = or (as ++ [b])
  a           || Bit (Or bs) = or (a : bs)
  a           || b           = or [a,b]
  not (Bit (Not c)) = c
  not (Bit (Var b)) = Bit (Var (negateLit b))
  not c     = Bit (Not c)
  a `xor` b = Bit (Xor a b)
  and xs  = Bit (And xs)
  or xs   = Bit (Or xs)
  choose s = fmap (\(a,b) -> Bit (Mux a b s))

class Equatable t where
  (===) :: t -> t -> Bit
  (/==) :: t -> t -> Bit

  a === b = not (a /== b)
  a /== b = not (a === b)

instance Equatable Bit where
  (/==) = xor
instance (Equatable a, Equatable b) => Equatable (a,b) where
  (a,b) === (a',b') = a === a' && b === b'
instance (Equatable a, Equatable b, Equatable c) => Equatable (a,b,c) where
  (a,b,c) === (a',b',c') = a === a' && b === b' && c === c'
instance (Equatable a, Equatable b, Equatable c, Equatable d) => Equatable (a,b,c,d) where
  (a,b,c,d) === (a',b',c',d') = a === a' && b === b' && c === c' && d === d'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e) => Equatable (a,b,c,d,e) where
  (a,b,c,d,e) === (a',b',c',d',e') = a === a' && b === b' && c === c' && d === d' && e === e'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f) => Equatable (a,b,c,d,e,f) where
  (a,b,c,d,e,f) === (a',b',c',d',e',f') = a === a' && b === b' && c === c' && d === d' && e === e' && f === f'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g) => Equatable (a,b,c,d,e,f,g) where
  (a,b,c,d,e,f,g) === (a',b',c',d',e',f',g') = a === a' && b === b' && c === c' && d === d' && e === e' && f === f' && g === g'
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g, Equatable h) => Equatable (a,b,c,d,e,f,g,h) where
  (a,b,c,d,e,f,g,h) === (a',b',c',d',e',f',g',h') = a === a' && b === b' && c === c' && d === d' && e === e' && f === f' && g === g' && h === h'

instance (a ~ Bit) => Equatable [a] where
  as === bs = bool (length as == length bs) && and (zipWith (===) as bs)

instance Variable Bit where
  exists = Bit . Var <$> exists
  forall = Bit . Var <$> forall

assert :: MonadSAT m => Bit -> m ()
assert b = do
  l <- runBit b
  assertFormula (formulaLiteral l)

runBit :: MonadSAT m => Bit -> m Literal
runBit (Bit (Not c)) = negateLiteral <$> runBit c
runBit (Bit (Var (Lit l))) = return l
runBit b@(Bit c) = generateLiteral b $ \out ->
  assertFormula =<< case c of
    And bs    -> formulaAnd out <$> traverse runBit bs
    Or  bs    -> formulaOr  out <$> traverse runBit bs
    Xor x y   -> formulaXor out <$> runBit x <*> runBit y
    Mux x y p -> formulaMux out <$> runBit x <*> runBit y <*> runBit p
    Var (Bool False) -> return $ formulaLiteral (negateLiteral out)
    Var (Bool True)  -> return $ formulaLiteral out

    -- Already handled above but GHC doesn't realize it.
    Not _       -> error "Unreachable"
    Var (Lit _) -> error "Unreachable"
