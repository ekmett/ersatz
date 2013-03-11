{-# OPTIONS_GHC -fglasgow-exts #-}

-- | This file cooperates with @benchmark.sh@.  This script grovels over the
-- result files produced by @benchmark.sh@ given on the command-line and outputs
-- a PNG graph comparison of the runtime data.
--
-- Example usage:
--
-- runghc GraphResult.hs /path/to/benchmark/date-1 ... /path/to/benchmark/date-n
module Main where

import Control.Monad
import Data.List( foldl', intercalate, transpose, genericLength )
import Graphics.Rendering.Chart
import System.Console.GetOpt
import System.Environment( getArgs )
import System.Exit( exitWith, ExitCode(..) )
import System.FilePath.Posix( pathSeparator )
import Text.Regex

type RegexGroup = ([String], String)

-- | Assume the input contains @n>0@ records delimited at the start by
-- whatever matches regexp.  Each element @(xs, s)@ of @groups rx f s@ is such
-- that (1) @xs@ is a list of submatches of @rx@ and (2) @s@ is the string
-- from (and not including) the last match, up to and not including the next
-- match (if any).
--
-- If the regex fails to match at all (i.e. @n=0@), the empty list is
-- returned.
groups :: Regex -> String -> [RegexGroup]
groups markerRx s = snd $ groups' markerRx s
  where
    -- Returns the text before the match, if any match, in its first position.
    groups' markerRx s =
        case matchRegexAll markerRx s of
          Nothing -> (s, [])
          Just (beforeMatch, _matched, afterMatch, submatches) ->
              let (beforeNext, retList) = groups' markerRx afterMatch
              in ( beforeMatch, (submatches, beforeNext) : retList )

------------------------------------------------------------------------------
-- Plots

plotColumnPoints col s = defaultPlotPoints
  { plot_points_style = s
  , plot_points_values = zipWith Point [1..] col }

plotLines col s = defaultPlotLines
  { plot_lines_style = s
  , plot_lines_values = [zipWith Point [1..] col] }

verticalLines numTests s = defaultPlotLines
  { plot_lines_style = s
  , plot_lines_values = map (\p -> [Point (p_x p) 1, p]) $
                        zipWith Point [1..numTests] (repeat 210) }

manyTickAxis = defaultAxis

-- input matrix a list of rows of data; first row has test label
myLayout header yLabel names matrix = defaultLayout1
    { layout1_title = header ++ intercalate " vs. " names
    , layout1_plots =
        -- Vertical lines.
        ("", HA_Bottom, VA_Right,
         toPlot (verticalLines (genericLength matrix) (gridLineStyle dimGray))):
        -- Show each column of data, not including the label column.
        concat
        [ [ ("", HA_Bottom, VA_Right, toPlot (plotLines col (lineStyle color)))
          , (yLabel ++ ": " ++ name ++ " (" ++ show i ++ ")", HA_Bottom, VA_Left,
             toPlot (plotColumnPoints col (pointStyle color))) ]
        | i     <- [1..length names]
        | name  <- names
        | col   <- pointsRows
        | color <- map toColor colors ] }
  where
    pointStyle color = exes 7 2 color
    gridLineStyle color = dashedLine 0.3 [4.0] (toColor color)
    lineStyle color  = solidLine 0.4 color --dashedLine 0.4 [7.0] color

    black = Color 0 0 0
    red   = Color 1 0 0
    green = Color 0 1 0
    blue  = Color 0 0 1
    dimGray = IntColor 105 105 105
    colors = [blue, red, green]

    -- remove label, convert to doubles, and turn into list of columns
    pointsRows = transpose . map (map read) . map tail $ matrix :: [[Double]]

data IntColor = IntColor Int Int Int
class ToColor a where
    toColor :: a -> Color
instance ToColor Color where
    toColor = id
instance ToColor IntColor where
    toColor (IntColor ri gi bi) = Color (r/255) (g/255) (b/255)
      where r = fromIntegral ri
            g = fromIntegral gi
            b = fromIntegral bi
instance Show Color where
    show (Color r g b) = "Color " ++ intercalate " " [show r, show g, show b]

savePNG filename header yLabel names matrix =
    renderableToPNGFile (toRenderable (myLayout header yLabel names matrix))
                        1024 768 filename

------------------------------------------------------------------------------
-- Statistics

-- | @deltas xs ys@ produces a list @zs@ such that @zs!!i == (xs!!i - ys!!i)@
-- for all @i@ for which indexing @xs@ and @ys@ is correct.
--
-- @deltas xs (repeat 0) == xs@
deltas :: (Num a) => [a] -> [a] -> [a]
{-# INLINE deltas #-}
deltas = zipWith (-)

deltasF :: (Floating r) => [r] -> [r] -> [r]
{-# INLINE deltasF #-}
deltasF = zipWith (-)

------------------------------------------------------------------------------
-- Main

-- group is filename at start of each run
satrunRx = mkRegex $
  "Solving ([-_./a-zA-Z0-9]+[.]cnf)"-- ./bench/bf/bf0432-007.cnf

-- group is user time
userTimeRx = mkRegex "([[:digit:]]+[.][[:digit:]]+) user"

findUserTime s =
    head `fmap` matchRegex userTimeRx s




main = do
  (opts, dirs) <- getArgs >>= parseOptions
  when (null dirs) $ do putStrLn "Nothing to do."
                        putStrLn (usageInfo usageHeader validOptions)
                        exitWith ExitSuccess
  groupList <- forM dirs (\dir -> groups satrunRx `liftM`
                                  readFile (dir ++ [pathSeparator] ++ "result.1"))
               :: IO [[RegexGroup]]
  titles <- forM dirs (\dir -> readFile (dir ++ [pathSeparator] ++ "info"))
  let benchFiles = map (head . fst) $ head groupList
      showTime maybeTime = case maybeTime of
                             Nothing   -> optionMax opts
                             Just time -> time
      labelRow = replicate (length dirs + 1) "IGNOREDLABEL"
      dataMatrix =
          (labelRow :)
          . zipWith (:) benchFiles
          . transpose
          . reverse $ 
          foldl' (\matrix grouping ->
                      (map showTime . map findUserTime . map snd $ grouping)
                      : matrix)
                 [] groupList
          :: [[String]]
  putStrLn $ "Saving '" ++ optionOutput opts ++ "'..."
  savePNG (optionOutput opts) (optionHeader opts) (optionYLabel opts) titles
          (tail dataMatrix)
--   forM_ dataMatrix $ putStrLn . intercalate " " 



validOptions :: [OptDescr (RunOptions -> RunOptions)]
validOptions = 
    [ Option [] ["header"] (ReqArg (\t opts -> opts{ optionHeader = t }) "STRING")
      "Header for the graph."
    , Option ['o'] ["output"]
      (ReqArg (\t opts -> opts{ optionOutput = t }) "FILENAME")
      "Output filename, a PNG."
    , Option [] ["ylabel"] (ReqArg (\t opts -> opts{ optionYLabel = t }) "STRING")
      "Label of the graphed value."
    , Option [] ["max"]
      (ReqArg (\t opts -> opts{ optionMax = t }) "NUMBER")
      "Height of y axis on graph." ]

data RunOptions = RunOptions
    { optionHeader :: String
    , optionOutput :: String
    , optionMax :: String
    , optionYLabel :: String }
defaultRunOptions = RunOptions
                    { optionHeader = "funsat comparison: "
                    , optionOutput = "result.png"
                    , optionMax = "300.0"
                    , optionYLabel = "y" }

-- Parse options, possibly exiting if there is nothing to do, or an error.
parseOptions :: [String] -> IO (RunOptions, [FilePath])
parseOptions args = do
    let (runOptTranss, filepaths, errors) = getOpt RequireOrder validOptions args
    when (not (null errors)) $ do { mapM_ putStr errors ;
                                    putStrLn (usageInfo usageHeader validOptions) ;
                                    exitWith (ExitFailure 1) }
    return (foldr id defaultRunOptions runOptTranss, filepaths)

usageHeader =
    "Usage: GraphResult [options] <results-dir-1> <results-dir-2> ... <results-dir-n>\nOptions:\n"


