{-
  trex

  Simple transformation and extraction for data files.
-}

{-# LANGUAGE QuasiQuotes #-}

import System.Environment
import Quote
import Fields
import First
import Shape
import Select
import Transform

main :: IO ()
main = do
    args <- getArgs
    case args of
      []    -> showUsage
      (c:a) -> case c of
                 "help"     -> showUsage
                 "-h"       -> showUsage
                 "--help"   -> showUsage
                 "fields"   -> fields a
                 "first"    -> first a
                 "leadzero" -> leadZero a
                 "select"   -> select a
                 "shape"    -> shape a
                 "unquote"  -> unQuote a
                 _          -> showUsage

showUsage :: IO ()
showUsage = putStrLn usage

usage = [quote|
trex - Simple transformation and extraction for CSV files.

USAGE

trex <command> [options] [file ...]

COMMANDS

  fields           Lists the fields in the header, if any.
    -t, --types    Adds the field types according to the first record.
      
  first            Prints the first record.
    -H, --header   Skips the header line.
    -t, --types    Adds the field types.

  leadzero         Adds a leading zero if missing from floating-point values.
    -H, --header   Skips the header line.

  select           Copies selected fields into a new file with .out as extension.
    -H, --header   Skips the header line.
    -f, --fields   Specifies field numbers (1-based) for extraction.
                   -fx,y,z --fields=x,y,z

  shape            Prints the number of columns and rows.

  unquote          Removes double (or single) quotes from fields.
    -H, --header   Skips the header line.
|]
