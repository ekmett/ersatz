-- | A parser for a subset of the regular expression syntax.

module RegexpGrid.Regexp (Regexp (..), parseRegexp) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Data.Char
import Text.Parsec

data Regexp = Nil
            | AnyCharacter Regexp
            | Character Char Regexp
            | Accept [Char] Regexp
            | Reject [Char] Regexp
            | Choice [Regexp] Regexp
            | Group Regexp Regexp
            | Repeat Integer (Maybe Integer) Regexp Regexp
            | Backreference Integer Regexp
  deriving Show

type REParser = Parsec String REState (Regexp -> Regexp)
type REState  = Integer

parseRegexp :: SourceName -> String -> Either ParseError Regexp
parseRegexp = runParser (regexp <* eof) 0

regexp :: Parsec String REState Regexp
regexp = go <$> (items `sepBy` char '|')
  where
    go []   = Nil
    go [re] = re
    go res  = Choice res Nil

items :: Parsec String REState Regexp
items = go <$> many (nonModifier >>= modifier)
  where
    go = ($ Nil) . foldr (.) id

nonModifier :: REParser
nonModifier  =  AnyCharacter <$ char '.'
            <|> group
            <|> characterClass
            <|> backreference
            <|> Character <$> nonSpecialChar

group :: REParser
group = Group <$> between (char '(') (char ')') go
  where
    go = modifyState (+1) *> regexp

backreference :: REParser
backreference = do
  _ <- char '\\'
  n <- fromIntegral . digitToInt <$> digit
  numGroups <- getState
  when (n == 0 || n > numGroups) $ invalid n
  return (Backreference n)
  where
    invalid n = fail ("Invalid backreference: " ++ show n)

characterClass :: REParser
characterClass = between (char '[') (char ']') go
  where
    go  =  Reject <$> (char '^' *> many1 nonSpecialChar)
       <|> Accept <$> many1 nonSpecialChar

nonSpecialChar :: Parsec String u Char
nonSpecialChar = noneOf "\\.[](){}|^$?*+" <?> "nonspecial"

modifier :: (Regexp -> Regexp) -> REParser
modifier re  =  Repeat 0 (Just 1) re' <$ char '?'
            <|> Repeat 0 Nothing  re' <$ char '*'
            <|> Repeat 1 Nothing  re' <$ char '+'
            <|> pure re
  where
    re' :: Regexp
    re' = re Nil
