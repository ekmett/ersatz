--------------------------------------------------------------------
-- |
-- Copyright :  © Edward Kmett 2010-2014, Johan Kiviniemi 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- <http://lonsing.github.io/depqbf/ DepQBF> is a solver capable of
-- solving quantified boolean formulae ('QBF').
--------------------------------------------------------------------
module Ersatz.Solver.DepQBF
  ( depqbf
  , depqbfPath
  , depqbfPathArgs
  ) where

import Control.Monad.IO.Class
import Data.Version (Version, makeVersion, parseVersion)
import Ersatz.Problem ( QSAT, writeQdimacs' )
import Ersatz.Solution
import Ersatz.Solver.Common
import qualified Data.IntMap as I
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import qualified Text.ParserCombinators.ReadP as P

-- | This is a 'Solver' for 'QSAT' problems that runs the @depqbf@ solver using
-- the current @PATH@, it tries to run an executable named @depqbf@.
depqbf :: MonadIO m => Solver QSAT m
depqbf = depqbfPath "depqbf"

parseLiteral :: String -> (Int, Bool)
parseLiteral ('-':xs) = (read xs, False)
parseLiteral xs = (read xs, True)

-- Parse the QDIMACS output format, which is described in
-- http://www.qbflib.org/qdimacs.html#output
parseOutput :: String -> [(Int, Bool)]
parseOutput out =
  case filter (not . comment) $ lines out of
    (_preamble:certLines) -> map parseCertLine certLines
    [] -> error "QDIMACS output without preamble"
  where
    comment [] = True
    comment ('c' : _) = True
    comment _ = False

    parseCertLine :: String -> (Int, Bool)
    parseCertLine certLine =
      case words certLine of
        (_v:lit:_) -> parseLiteral lit
        _ -> error $ "Malformed QDIMACS certificate line: " ++ certLine

-- | This is a 'Solver' for 'QSAT' problems that lets you specify the path to the @depqbf@ executable.
-- This passes different arguments to @depqbf@ depending on its version:
--
-- * If using version 6.03 or later, this passes @[\"--qdo\", \"--no-dynamic-nenofex\"]@.
--
-- * Otherwise, this passes @[\"--qdo\"]@.
depqbfPath :: MonadIO m => FilePath -> Solver QSAT m
depqbfPath path problem = do
  ver <- liftIO $ depqbfVersion path
  let args | ver >= makeVersion [6,03]
           = [ "--qdo", "--no-dynamic-nenofex" ]
           | otherwise
           = [ "--qdo" ]
  depqbfPathArgs path args problem

-- | This is a 'Solver' for 'QSAT' problems that lets you specify the path to the @depqbf@ executable
-- as well as a list of command line arguments. They will appear after the problem file name.
depqbfPathArgs :: MonadIO m => FilePath -> [String] -> Solver QSAT m
depqbfPathArgs path args problem = liftIO $
  withTempFiles ".cnf" "" $ \problemPath _ -> do
    writeQdimacs' problemPath problem

    (exit, out, _err) <-
      readProcessWithExitCode path (problemPath : args) []

    let result = resultOf exit

    return $ (,) result $
      case result of
        Satisfied ->
          I.fromList $ parseOutput out
        _ ->
          I.empty

-- | Query @depqbf@'s 'Version' by invoking @depqbf --version@ and parsing the
-- output. This assumes that the output can be parsed as a valid 'Version' and
-- that @depqbf@ versions increase in a way that is compatible with the
-- 'Ord Version' instance (see 'depqbfPath', which compares 'Version's using
-- ('>=')).
depqbfVersion :: FilePath -> IO Version
depqbfVersion path = do
  (exit, out, err) <-
    readProcessWithExitCode path ["--version"] []

  let parseError reason =
        fail $ unlines
          [ "Could not query depqbf version (" ++ reason ++ ")"
          , "Standard output:"
          , out
          , ""
          , "Standard error:"
          , err
          ]

  case exit of
    ExitSuccess -> do
      -- Should be something like "DepQBF 6.03"
      verStrLine <-
        case lines err of
          line:_ -> pure line
          [] -> parseError "no lines of standard error"
      -- Should be something like "6.03"
      verStr <-
        case words verStrLine of
          _depQBF:ver:_ -> pure ver
          _ -> parseError $ "unexpected version number " ++ verStrLine
      -- Convert the string to a full Version
      case readEitherP parseVersion verStr of
        Left reason -> parseError reason
        Right v -> pure v
    ExitFailure i ->
      parseError $ "exit code " ++ show i ++ ")"

-- | Like @readEither@ from "Text.Read", but accepting an arbitrary 'P.ReadP'
-- argument instead of requiring a 'Read' constraint.
readEitherP :: P.ReadP a -> String -> Either String a
readEitherP rp s =
  case [ x | (x,"") <- P.readP_to_S read' s ] of
    [x] -> Right x
    []  -> Left "no parse"
    _   -> Left "ambiguous parse"
 where
  read' = do
    x <- rp
    P.skipSpaces
    pure x
