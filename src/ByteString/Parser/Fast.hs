{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module ByteString.Parser.Fast where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.AffineSpace ((.-^), (.+^))
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
{-# INLINE isDigit #-}

isHexa :: Word8 -> Bool
isHexa !x =  (x >= 0x30 && x <= 0x39)
          || (x >= 0x41 && x <= 0x46)
          || (x >= 0x61 && x <= 0x66)
{-# INLINE isHexa #-}

hexToNum :: Num n => Word8 -> n
hexToNum x | x >= 0x30 && x <= 0x39 = fromIntegral x - 0x30
           | x >= 0x41 && x <= 0x46 = fromIntegral x - 0x37
           | x >= 0x61 && x <= 0x66 = fromIntegral x - 0x57
           | otherwise              = 0
{-# INLINABLE hexToNum #-}

isUpper :: Word8 -> Bool
isUpper !x = x >= 0x41 && x <= 0x5a
{-# INLINE isUpper #-}

isLower :: Word8 -> Bool
isLower !x = x >= 0x61 && x <= 0x7a
{-# INLINE isLower #-}

decimal :: Parser Int
decimal = getInt <$> takeWhile1 isDigit
{-# INLINE decimal #-}

num :: Num n => Parser n
num = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0 <$> takeWhile1 isDigit
{-# INLINABLE num #-}

hnum :: Num n => Parser n
hnum = BS.foldl' (\acc n -> acc * 16 + hexToNum n) 0 <$> takeWhile1 isHexa
{-# INLINABLE hnum #-}

onum :: Num n => Parser n
onum = BS.foldl' (\acc n -> acc * 8 + fromIntegral (n - 0x30)) 0 <$> takeWhile1 isHexa
{-# INLINABLE onum #-}

takeN :: Int -> Parser BS.ByteString
takeN n = Parser $ \input failure success -> if BS.length input < n
                                                 then failure
                                                 else let (a,rest) = BS.splitAt n input
                                                      in  success rest a

anyChar :: Parser Char
anyChar = Parser $ \input failure success -> if BS.null input then failure else success (BS8.tail input) (BS8.head input)

char :: Char -> Parser ()
char c = Parser $ \input failure success -> if BS.null input then failure else if BS8.head input == c then success (BS.tail input) () else failure
{-# INLINE char #-}

string :: BS.ByteString -> Parser ()
string s = Parser $ \input failure success -> if s `BS.isPrefixOf` input
                                                  then success (BS.drop (BS.length s) input) ()
                                                  else failure
quotedString :: Parser BS.ByteString
quotedString = char '"' *> go <* char '"'
    where
        go = BS.concat <$> many (normal <|> escaped)
        normal = charTakeWhile1 (\x -> x /= '"' && x /= '\\')
        escaped = do
            char '\\'
            c <- anyChar
            return $ case c of
                         'r' -> BS8.singleton '\r'
                         'n' -> BS8.singleton '\n'
                         't' -> BS8.singleton '\t'
                         'a' -> BS8.singleton '\a'
                         'b' -> BS8.singleton '\b'
                         '"' -> BS8.singleton '"'
                         _   -> BS8.pack ['\\',c]

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

charTakeWhile1 :: (Char -> Bool) -> Parser BS.ByteString
charTakeWhile1 prd = Parser $ \s failure success -> case BS8.span prd s of
                                                    (a,b) -> if BS.null a then failure else success b a
{-# INLINE charTakeWhile1 #-}

takeWhile1 :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile1 prd = Parser $ \s failure success -> case BS.span prd s of
                                                    (a,b) -> if BS.null a then failure else success b a
{-# INLINE takeWhile1 #-}

charTakeWhile :: (Char -> Bool) -> Parser BS.ByteString
charTakeWhile prd = Parser $ \s _ success -> case BS8.span prd s of
                                             (a,b) -> success b a
{-# INLINE charTakeWhile #-}

takeWhile :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile prd = Parser $ \s _ success -> case BS.span prd s of
                                             (a,b) -> success b a
{-# INLINE takeWhile #-}

dropWhile :: (Word8 -> Bool) -> Parser ()
dropWhile prd = Parser $ \s _ success -> success (BS.dropWhile prd s) ()
{-# INLINE dropWhile #-}

parseOnly :: Parser a -> BS.ByteString -> Maybe a
parseOnly (Parser p) s = p s Nothing $ \b a -> if BS.null b
                                                   then Just a
                                                   else Nothing

remaining :: Parser BS.ByteString
remaining = Parser $ \input _ success -> success BS.empty input

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

rfc3339 :: Parser UTCTime
rfc3339 = do
    !day <- parseYMD <* char 'T'
    !difftime <- parseDTime
    !o <- anyChar
    let !tm = UTCTime day difftime ^. from utcTime
        suboffset = (tm .-^)
        addoffset = (tm .+^)
        getOffset = do
            h <- decimal
            char ':'
            m <- decimal
            return (fromSeconds (h*3600 + m*60))
    case o of
        'Z' -> return tm
        '+' -> suboffset <$> getOffset
        '-' -> addoffset <$> getOffset
        _ -> empty

parseTimestamp :: BS.ByteString -> Maybe UTCTime
parseTimestamp txt | "%++" `BS.isPrefixOf` txt = Just $ view (from utcTime) $ UTCTime (YearMonthDay 2016 03 12 ^. from gregorian) (fromSeconds (0 :: Int))
                   | otherwise = parseOnly timestamp txt

pFold :: Parser a -> Fold BS.ByteString a
pFold p = to (parseOnly p) . _Just


