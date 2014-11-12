{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module : Transform
--
-- Implements the transform commands.
--

module Transform (
    unQuote,
    leadZero
    ) where

import           Data.Char (ord)
import           Data.Csv (encode)
import           Data.Csv.Streaming (decode, HasHeader(..))
import           Data.Word (Word8)
import           Data.Vector (Vector, (!))
import           Util
import qualified Data.Vector          as V
import qualified Data.Text.Lazy       as LT
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable        as F

----
---- Functions for setting up a transform command.
----

-- | The type of a field-transform function.
type Transfn = LB.ByteString -> LB.ByteString

process :: Transfn -> Bool -> LB.ByteString -> LB.ByteString
process f header contents =
    let recLst = F.toList $ decode NoHeader contents in
      encode $ [trans f rec | rec <- if header then tail recLst else recLst]
    where trans f r = map f (V.toList r)

processFile :: Transfn -> Bool -> String -> IO ()
processFile f header path = do
    contents <- LB.readFile path
    LB.writeFile out $ process f header contents
    putStrLn path
  where out = path ++ ".out"

parseCmd :: [String] -> (Bool, [String])
parseCmd xs = (header, files)
    where header = "-H" `elem` xs || "--header" `elem` xs
          files  = dropWhile isOption xs

-- | Process one or more files, applying a field-transform function.
transform :: Transfn -> [String] -> IO ()
transform f args =
    let (header, files) = parseCmd args in
      if null files
        then LB.interact $ process f header
        else mapM_ (processFile f header) files

----
---- Transform commands.
----

single :: Word8
single = fromIntegral (ord '\'')

double :: Word8
double = fromIntegral (ord '"')

dot :: Word8
dot = fromIntegral (ord '.')

zero :: Word8
zero = fromIntegral (ord '0')

neg :: Word8
neg = fromIntegral (ord '-')

quoted :: LB.ByteString -> Bool
quoted s | s == LB.empty = False
         | otherwise     = let h = LB.head s in
                             h == single || h == double

unQuote_ :: Transfn
unQuote_ s | quoted s  = LB.tail $ LB.init s
           | otherwise = s

unQuote :: [String] -> IO ()
unQuote args = transform unQuote_ args

leadZero_ :: Transfn
leadZero_ s | s == LB.empty = s
            | first == dot  = LB.cons zero s
            | first == neg  = LB.cons neg (leadZero_ $ LB.tail s)
            | otherwise     = s
    where first = LB.head s

leadZero :: [String] -> IO ()
leadZero args = transform leadZero_ args
