{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Bytestring.Parser.Fast where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.AffineSpace ((.-^))
import Control.Applicative
import Data.Word
import Data.Thyme
import Control.Lens

newtype Parser a = Parser { runParser :: forall r. BS.ByteString -> r -> (BS.ByteString -> a -> r) -> r }
                   deriving Functor

instance Applicative Parser where
    pure a = Parser $ \b _ s -> s b a
    {-# INLINE pure #-}
    Parser pf <*> Parser px = Parser $ \input failure success ->
        let succ' input' f = px input' failure (\i a -> success i (f a))
        in  pf input failure succ'
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = Parser (\_ failure _ -> failure)
    {-# INLINE empty #-}
    Parser a <|> Parser b = Parser $ \input failure success -> a input (b input failure success) success
    {-# INLINE (<|>) #-}

instance Monad Parser where
    fail _ = Parser $ \_ failure _ -> failure
    m >>= k = Parser $ \input failure success ->
        let succ' input' a = runParser (k a) input' failure success
        in  runParser m input failure succ'
    {-# INLINE (>>=) #-}

getInt :: BS.ByteString -> Int
getInt = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0
{-# INLINE getInt #-}

getOctal :: BS.ByteString -> Int
getOctal = BS.foldl' (\acc n -> acc * 8 + fromIntegral (n - 0x30)) 0
{-# INLINE getOctal #-}

isDigit :: Word8 -> Bool
isDigit !x = x >= 0x30 && x <= 0x39

isUpper :: Word8 -> Bool
isUpper !x = x >= 0x41 && x <= 0x5a

decimal :: Parser Int
decimal = getInt <$> takeWhile1 isDigit
{-# INLINE decimal #-}

anyChar :: Parser Char
anyChar = Parser $ \input failure success -> if BS.null input then failure else success (BS8.tail input) (BS8.head input)

char :: Char -> Parser ()
char c = Parser $ \input failure success -> if BS.null input then failure else if BS8.head input == c then success (BS.tail input) () else failure
{-# INLINE char #-}

scientific :: Parser Double
scientific = finalize . BS.foldl' step (0,0) <$> takeWhile1 (\n -> isDigit n || n == 0x2e)
    where
        finalize :: (Int, Double) -> Double
        finalize (!n,!x) = if x == 0
                             then fromIntegral n
                             else fromIntegral n / x
        step (!n,!x) !v = if v == 0x2e
                              then (n,1)
                              else (n * 10 + fromIntegral (v - 0x30), x * 10)
{-# INLINE scientific #-}

takeWhile1 :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile1 prd = Parser $ \s failure success -> case BS.span prd s of
                                                    (a,b) -> if BS.null a then failure else success b a
{-# INLINE takeWhile1 #-}

parseOnly :: Parser a -> BS.ByteString -> Maybe a
parseOnly (Parser p) s = p s Nothing $ \b a -> if BS.null b
                                                   then Just a
                                                   else Nothing


parseYMD :: Parser Day
parseYMD = do
    !y <- decimal <* char '-'
    !m <- decimal <* char '-'
    !d <- decimal
    return $! YearMonthDay y m d ^. from gregorian

parseDTime :: Parser DiffTime
parseDTime = do
    !h  <- decimal <* char ':'
    !mi <- decimal <* char ':'
    !s  <- scientific
    return $! fromSeconds $ fromIntegral (h * 3600 + mi * 60 :: Int) + s

timestamp :: Parser UTCTime
timestamp = do
    !day <- parseYMD <* char '+'
    !difftime <- parseDTime <* char '+'
    let !tm = UTCTime day difftime ^. from utcTime
    !tz <- takeWhile1 isUpper
    return $! case tz of
                  "CEST" -> tm .-^ fromSeconds (7200 :: Int)
                  "CET" -> tm .-^ fromSeconds (3600 :: Int)
                  _ -> tm

parseTimestamp :: BS.ByteString -> Maybe UTCTime
parseTimestamp txt | "%++" `BS.isPrefixOf` txt = Just $ view (from utcTime) $ UTCTime (YearMonthDay 2016 03 12 ^. from gregorian) (fromSeconds (0 :: Int))
                   | otherwise = parseOnly timestamp txt

pFold :: Parser a -> Fold BS.ByteString a
pFold p = to (parseOnly p) . _Just


