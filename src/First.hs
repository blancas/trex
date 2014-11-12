{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module : First
--
-- Implements the first command.
--

module First (first) where

import           Data.Csv
import           Data.Vector (Vector, (!), toList)
import           System.IO 
import           Text.Printf (printf)
import           Util
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC

parse :: LB.ByteString -> [LB.ByteString]
parse s =
    case decode NoHeader s :: Either String (Vector (Vector LB.ByteString)) of
      Right v -> toList (v ! 0)
      Left _  -> [LB.empty]

guessType :: LB.ByteString -> LB.ByteString
guessType s | isInt   s = "Integer"
            | isFloat s = "Floating point"              
            | isBool  s = "Boolean"
            | isDate  s = "Date"
            | isAlpha s = "Alphanumeric"
            | otherwise = "String"

getTypes :: BS.ByteString -> [LB.ByteString]
getTypes record = map guessType $ parse (LB.fromChunks [record])

getFields :: BS.ByteString -> Bool -> [(Int, LB.ByteString)]
getFields record types = zip fnums values
    where fields = parse $ LB.fromChunks [record]
          values  = if types then map append tnames else fields
          tnames = zip fields $ getTypes record
          append = (\(x, y) -> x `LB.append` ", " `LB.append` y)
          fnums  = [1..length values]

printFields :: [(Int, LB.ByteString)] -> IO ()
printFields field = mapM_ display field
   where display (a,s) = printf "%3d) " a >> LC.putStrLn s

processFile :: Bool -> Bool -> String -> IO ()
processFile header types path = do
    handle <- openFile path ReadMode
    _      <- if header then BS.hGetLine handle else return ""
    record <- BS.hGetLine handle
    hClose handle
    putStrLn path
    printFields $ getFields record types

processStdin :: Bool -> Bool -> IO ()
processStdin header types = do
    _      <- if header then BS.getLine else return ""
    record <- BS.getLine
    printFields $ getFields record types

parseCmd :: [String] -> (Bool, Bool, [String])
parseCmd xs = (header, types, files)
    where header = "-H" `elem` xs || "--header" `elem` xs
          types  = "-t" `elem` xs || "--types"  `elem` xs
          files  = dropWhile isOption xs

-- | Lists the values of the first record.
first :: [String] -> IO ()
first args =
    let (header, types, files) = parseCmd args in
      if null files
        then processStdin header types
        else mapM_ (processFile header types) files
