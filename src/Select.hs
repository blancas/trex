{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module : Select
--
-- Implements the select command.
--

module Select (select) where

import           Control.Monad (mplus)
import           Data.Csv (encode)
import           Data.Csv.Streaming (decode, HasHeader(..))
import           Data.List (foldl')
import           Data.Vector (Vector, (!))
import           Util                 as U
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable        as F

type Fields = [Int]

extract :: Fields -> Vector LB.ByteString -> [LB.ByteString]
extract fs rec = map (\n -> rec ! (n-1)) fs

process :: Bool -> Fields -> LB.ByteString -> LB.ByteString
process header fields contents =
    let recLst = F.toList $ decode NoHeader contents in
      encode $ [extract fields rec | rec <- if header then tail recLst else recLst]
 
processFile :: Bool ->Fields -> String -> IO ()
processFile header fields path = do
    contents <- LB.readFile path
    LB.writeFile out $ process header fields contents
    putStrLn out
  where out = path ++ ".out"

scanFields :: Maybe String -> Fields
scanFields opt =
    case opt of
      Just lst -> read $ "[" ++ lst ++ "]"
      Nothing  -> []

parseCmd :: [String] -> (Bool, Fields, [String])
parseCmd xs = (header, fields, files)
    where header = "-H" `elem` xs || "--header" `elem` xs
          fields = scanFields $ U.getOption xs "-f" `mplus` U.getOption xs "--fields="
          files  = dropWhile U.isOption xs

-- | Copies CSV files with only the listed fields.
select :: [String] -> IO ()
select args =
    let (header, fields, files) = parseCmd args in
      if null files
        then LB.interact $ process header fields
        else mapM_ (processFile header fields) files
