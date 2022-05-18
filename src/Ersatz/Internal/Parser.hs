--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A trivial, inefficient parser with no support for error reporting.
--------------------------------------------------------------------
module Ersatz.Internal.Parser
  ( Parser
  , runParser
  , sepBy, sepBy1
  , token, string
  , integer, natural
  , eof
  , satisfy
  ) where

import Control.Applicative
import Control.Monad (guard)
import Control.Monad.Trans.State
import Data.Char (isDigit)

type Parser t a = StateT [t] [] a

runParser :: Parser t a -> [t] -> [a]
runParser = evalStateT

sepBy :: Parser t a -> Parser t sep -> Parser t [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser t a -> Parser t sep -> Parser t [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

token :: Eq t => t -> Parser t t
token t = satisfy (== t)

string :: Eq t => [t] -> Parser t [t]
string = traverse token

integer :: (Num i, Read i) => Parser Char i
integer = negation <*> natural

negation :: Num n => Parser Char (n -> n)
negation  =  negate <$ token '-'
         <|> pure id

natural :: Read i => Parser Char i
natural = read <$> some (satisfy isDigit)

eof :: Parser t ()
eof = do
  [] <- get
  return ()

satisfy :: (t -> Bool) -> Parser t t
satisfy f = do
  (t:ts) <- get
  guard (f t)
  t <$ put ts
