{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module : Util
--
-- Provides utility functions.
--

module Util (
    isOption,
    getOption,
    toText,
    fromText,
    toString,
    fromString,
    toString_,
    fromString_,
    isInt,
    isFloat,
    isBool,
    isDate,
    isAlpha
    ) where

import           Data.List (find, isPrefixOf, stripPrefix)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Text.Regex.Base.RegexLike (defaultCompOpt, defaultExecOpt)
import           Text.Regex.TDFA.ByteString.Lazy (Regex, compile, execute)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Lazy        as LT

isOption :: String -> Bool
isOption = (== '-') . head

getOption :: [String] -> String -> Maybe String
getOption args opt =  case find (isPrefixOf opt) args of
                        Just arg -> stripPrefix opt arg
                        Nothing  -> Nothing

----
---- Conversion between string types, lazy and strict.
---- Functions are named with relation to ByteString.
----

-- | Converts a lazy ByteString into a lazy Text.
toText :: LB.ByteString -> LT.Text
toText = decodeUtf8

-- | Converts a lazy Text into a lazy ByteString.
fromText :: LT.Text -> LB.ByteString
fromText = encodeUtf8

-- | Converts a lazy ByteString into a String. 
toString :: LB.ByteString -> String
toString = LT.unpack . toText

-- | Converts a String into a lazy ByteString.
fromString :: String -> LB.ByteString
fromString = fromText . LT.pack

-- | Converts a ByteString into a String.
toString_ :: B.ByteString -> String
toString_ = C.unpack

-- | Converts a String into a ByteString.
fromString_ :: String -> B.ByteString
fromString_ = C.pack

----
---- These functions attempt to detect the of a text value
---- using the regex-tdfa library.
----

getRegex :: LB.ByteString -> Regex
getRegex s = case compile defaultCompOpt defaultExecOpt s of
                   Right regex -> regex
                   Left msg    -> error msg

runRegex :: Regex -> LB.ByteString -> Bool
runRegex r s = case execute r s of
                 Right Nothing  -> False
                 Right (Just _) -> True
                 Left msg       -> error msg

intRegex   = getRegex "^-?[0-9]+$"
floatRegex = getRegex "^-?[0-9]*\\.[0-9]+([eE][\\+\\-]?[0-9]+)?$"
boolRegex  = getRegex "^(true|false)$"
dateRegex  = getRegex "[0-9]+[/\\-]+[0-9]+[/\\-]+[0-9]+"
alphaRegex = getRegex "[a-zA-Z]+"

-- | Tells if s encodes an integer.
isInt :: LB.ByteString -> Bool
isInt = runRegex intRegex

-- | Tells if s encodes a floating-point number.
isFloat :: LB.ByteString -> Bool
isFloat = runRegex floatRegex

-- | Tells if s encodes a boolean.
isBool :: LB.ByteString -> Bool
isBool = runRegex boolRegex . lower
    where lower = fromText . LT.toLower . toText

-- | Tells if s encodes a date.
isDate :: LB.ByteString -> Bool
isDate = runRegex dateRegex

-- | Tells if s encodes an alphanumeric value.
isAlpha  :: LB.ByteString -> Bool
isAlpha = runRegex alphaRegex
