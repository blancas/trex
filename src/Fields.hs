{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module : Fields
--
-- Implements the fields command.
--

module Fields (fields) where

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

getTypes :: BS.ByteString -> [LB.ByteString]
getTypes record = map guessType $ parse (LB.fromChunks [record])

getFields :: BS.ByteString -> BS.ByteString -> [(Int, LB.ByteString)]
getFields header record = zip fnums names
    where fnames = parse $ LB.fromChunks [header]
          names  = if BS.null record then fnames else map append tnames
          tnames = zip fnames $ getTypes record
          append = (\(x, y) -> x `LB.append` ", " `LB.append` y)
          fnums  = [1..length names]

printFields :: [(Int, LB.ByteString)] -> IO ()
printFields field = mapM_ display field
   where display (a,s) = printf "%3d) " a >> LC.putStrLn s

processFile :: Bool -> String -> IO ()
processFile types path = do
    handle <- openFile path ReadMode
    header <- BS.hGetLine handle
    record <- if types then BS.hGetLine handle else return ""
    hClose handle
    putStrLn path
    printFields $ getFields header record

interactive :: Bool -> IO ()
interactive types = do
    header <- BS.getLine
    record <- if types then BS.getLine else return ""
    printFields $ getFields header record

parseCmd :: [String] -> (Bool, [String])
parseCmd []       = (False, [])
parseCmd a@(x:xs) = if x == "-t" || x == "--types" then (True, xs) else (False, a)


-- | Lists the field names from the first line.
fields :: [String] -> IO ()
fields args =
    let (types, files) = parseCmd args in
      if null files
        then interactive types
        else mapM_ (processFile types) files
