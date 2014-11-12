{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module : Shape
--
-- Implements the shape command.
--

module Shape (shape) where

import           Data.Csv (encode)
import           Data.Csv.Streaming (decode, HasHeader(..))
import           Data.List (foldl')
import qualified Data.Vector          as V
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable        as F

-- Shape is a list of columns and the number of rows.
type Shape = ([Int], Int)

count :: V.Vector LB.ByteString -> Int
count = length . V.toList

printShape :: Shape -> IO ()
printShape (lst, n) = do
    putStrLn $ "Columns: " ++ (show $ reverse lst)
    putStrLn $ "By Rows: " ++ show n

getShape :: LB.ByteString -> Shape
getShape s = let counts = [count r | r <- F.toList $ decode NoHeader s] in
               foldl' f ([], 0) counts
    where f (lst, !n) c | c `elem` lst = (lst, n+1)
                        | otherwise    = (c:lst, n+1)

processFile :: String -> IO ()
processFile path = do
    contents <- LB.readFile path
    putStrLn path
    printShape $ getShape contents

interactive :: IO ()
interactive = do
    contents <- LB.getContents
    printShape $ getShape contents

-- | Shows the shape of CSV files as number of rows and columns.
shape :: [String] -> IO ()
shape files =
    if null files
      then interactive
      else mapM_ processFile files
