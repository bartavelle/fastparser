-- | A fast parser combinators module.
-- 
-- This module is extremely bare-bones, and provides only very limited
-- functionality.
--
-- Sample usage:
-- 
-- > module Syslog where
-- > 
-- > import ByteString.Parser.Fast
-- > import qualified Data.ByteString as BS
-- > import Data.Thyme.Clock
-- > import Control.Applicative
-- > 
-- > data SyslogMsg
-- >     = SyslogMsg
-- >     { _syslogPrio    :: {-# UNPACK #-} !Int
-- >     , _syslogTS      :: {-# UNPACK #-} !UTCTime
-- >     , _syslogHost    :: !BS.ByteString
-- >     , _syslogProgram :: !BS.ByteString
-- >     , _syslogPID     :: !(Maybe Int)
-- >     , _syslogData    :: !BS.ByteString
-- >     } deriving (Show, Eq)
-- > 
-- > 
-- > syslogMsg :: Parser SyslogMsg
-- > syslogMsg = do
-- >     char '<'
-- >     prio <- decimal
-- >     char '>'
-- >     ts <- rfc3339
-- >     char ' '
-- >     host <- charTakeWhile1 (/= ' ')
-- >     char ' '
-- >     program <- charTakeWhile1 (\x -> x /= ':' && x /= '[')
-- >     pid' <- optional (char '[' *> decimal <* char ']')
-- >     char ':'
-- >     dt <- remaining
-- >     return (SyslogMsg prio ts host program pid' dt)
-- > 
-- > test :: BS.ByteString -> Either ParseError SyslogMsg
-- > test = parseOnly syslogMsg
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module ByteString.Parser.Fast
  (
  Parser, ParserM(..), parseOnly,
  -- * Error handling
  ParseError(..), ErrorItem(..), ueof, ufail, parseError,
  -- * Parsing numerical values
  decimal, num, hnum, onum, frac, scientific,
  -- * Parsing characters
  anyChar, char, string, quotedString,
  -- * Various combinators
  takeN, remaining, charTakeWhile, charTakeWhile1, ByteString.Parser.Fast.takeWhile, takeWhile1, skipWhile,
  -- * Parsing time-related values
  parseYMD, parseDTime, timestamp, rfc3339,
  -- * Interfacing with other libraries
  wlex, pFold,
  -- * Hacks and bits
  isLower, getOctal, getInt
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Set (Set)
import qualified Data.Set as S
import Data.AffineSpace ((.-^), (.+^))
import Control.Applicative
import Control.Monad
import Data.Word
import Data.Thyme
import Data.Thyme.Time.Core
import Lens.Micro
import Data.Monoid
import qualified Data.ByteString.Lex.Fractional as L
import Control.Monad.Codensity (Codensity, lowerCodensity)
import Control.Monad.Trans.Class (lift)
import Prelude

-- | A parser, church encoded. The arguments to the wrapped function are:
--
--  * Input "ByteString".
--  * A function that handles parse errors.
--  * A function that handles success, taking as argument the remaining
--  input and the parser result.
newtype ParserM a
  = Parser {
           runParser :: forall r. BS.ByteString
                     -> (ParseError -> r)
                     -> (BS.ByteString -> a -> r)
                     -> r
  } deriving Functor

type Parser = Codensity ParserM

instance Applicative ParserM where
    pure a = Parser $ \b _ s -> s b a
    {-# INLINE pure #-}
    Parser pf <*> Parser px = Parser $ \input failure success ->
        let succ' input' f = px input' failure (\i a -> success i (f a))
        in  pf input failure succ'
    {-# INLINE (<*>) #-}

instance Alternative ParserM where
    empty = Parser (\_ failure _ -> failure mempty)
    {-# INLINE empty #-}
    Parser a <|> Parser b = Parser $ \input failure success -> a input (\rr ->  b input (failure . mappend rr) success) success
    {-# INLINE (<|>) #-}

instance Monad ParserM where
    fail s = Parser $ \_ failure _ -> failure (ufail s)
    m >>= k = Parser $ \input failure success ->
        let succ' input' a = runParser (k a) input' failure success
        in  runParser m input failure succ'
    {-# INLINE (>>=) #-}

instance MonadPlus ParserM

data ErrorItem
    = Tokens BS.ByteString
    | Label String
    deriving (Show, Eq, Ord)

data ParseError
  = ParseError
  { errorUnexpected :: !(Set ErrorItem)
  , errorExpected   :: !(Set ErrorItem)
  } deriving (Show, Eq)

instance Monoid ParseError where
  mempty = ParseError mempty mempty
  mappend (ParseError u1 e1) (ParseError u2 e2) = ParseError (mappend u1 u2) (mappend e1 e2)

-- | An error representing the unexpected end of input.
ueof :: ParseError
ueof = ParseError (S.singleton (Label "end of input")) mempty

-- | A generic error.
ufail :: String -- ^ The expected label.
      -> ParseError
ufail s = ParseError (S.singleton (Label s)) mempty

-- | Creates a generic parse error.
parseError :: BS8.ByteString -- ^ Unexpected content
           -> BS8.ByteString -- ^ Expected content
           -> ParseError
parseError un ex = ParseError (S.singleton (Tokens un)) (S.singleton (Tokens ex))

-- | Creates a parser from the supplied function.
-- The first argument to the supplied function is the remaining input, and
-- it should return `Nothing` when parsing failes, or `Just` the result
-- along with the non-consumed input.
--
-- It works well with the
-- [bytestring-lexing](https://hackage.haskell.org/package/bytestring-lexing) library.
wlex :: (BS.ByteString -> Maybe (a, BS.ByteString)) -> Parser a
wlex p = lift $ Parser $ \i failure success -> case p i of
                                            Nothing -> failure mempty
                                            Just (a, i') -> success i' a
{-# INLINABLE wlex #-}

-- | Parses bytestrings as if they were representing a decimal number in ASCII.
getInt :: BS.ByteString -> Int
getInt = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0
{-# INLINE getInt #-}

-- | Parses bytestrings as if they were representing an octal number in
-- ASCII.
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

-- | Returns true when the character represents an ASCII lowercase letter.
isLower :: Word8 -> Bool
isLower !x = x >= 0x61 && x <= 0x7a
{-# INLINE isLower #-}

-- | parses a decimal integer.
decimal :: Parser Int
decimal = getInt <$> takeWhile1 isDigit
{-# INLINE decimal #-}

-- | Parses any positive decimal 'Num'.
num :: Num n => Parser n
num = BS.foldl' (\acc n -> acc * 10 + fromIntegral (n - 0x30)) 0 <$> takeWhile1 isDigit
{-# INLINABLE num #-}

-- | Parses any positive hexadecimal 'Num'.
hnum :: Num n => Parser n
hnum = BS.foldl' (\acc n -> acc * 16 + hexToNum n) 0 <$> takeWhile1 isHexa
{-# INLINABLE hnum #-}

-- | Parses any positives octal 'Num'.
onum :: Num n => Parser n
onum = BS.foldl' (\acc n -> acc * 8 + fromIntegral (n - 0x30)) 0 <$> takeWhile1 isHexa
{-# INLINABLE onum #-}

-- | Parses 'Fractional' numbers.
frac :: Fractional a => Parser a
frac = wlex (L.readSigned L.readDecimal)
{-# INLINABLE frac #-}

-- | Consumes n bytes of input
takeN :: Int -> Parser BS.ByteString
takeN n = lift $ Parser $ \input failure success
    -> if BS.length input < n
         then failure ueof
         else let (a,rest) = BS.splitAt n input
              in  success rest a

-- | Parses any character.
anyChar :: Parser Char
anyChar = lift $ Parser $ \input failure success -> if BS.null input then failure ueof else success (BS8.tail input) (BS8.head input)

-- | Parses a specific character.
char :: Char -> Parser ()
char c = lift $ Parser $ \input failure success -> if BS.null input then failure ueof else if BS8.head input == c then success (BS.tail input) () else failure (parseError (BS8.take 1 input) (BS8.singleton c))
{-# INLINE char #-}

-- | Parses the supplied string.
string :: BS.ByteString -> Parser ()
string s = lift $ Parser $ \input failure success
    -> if s `BS.isPrefixOf` input
          then success (BS.drop (BS.length s) input) ()
          else failure (parseError (BS.take (BS.length s) input) s)

-- | Parses strings between double quotes. This functions handles the
-- following escape sequences: \\r, \\n, \\t, \\a, \\b, \\", \\\\.
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

-- | A fast parser for numbers of the form 5.123. Contrary to what its name
-- implies, it parses to 'Double'.
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
charTakeWhile1 prd = lift $ Parser $ \s failure success ->
    case BS8.span prd s of
      (a,b) -> if BS.null a then failure (ParseError (S.singleton (Tokens (BS.take 1 s))) mempty) else success b a
{-# INLINE charTakeWhile1 #-}

takeWhile1 :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile1 prd = lift $ Parser $ \s failure success ->
    case BS.span prd s of
      (a,b) -> if BS.null a then failure (ParseError (S.singleton (Tokens (BS.take 1 s))) mempty) else success b a
{-# INLINE takeWhile1 #-}

-- | Consumes the input as long as the predicate remains true.
charTakeWhile :: (Char -> Bool) -> Parser BS.ByteString
charTakeWhile prd = lift $ Parser $ \s _ success ->
    case BS8.span prd s of
      (a,b) -> success b a
{-# INLINE charTakeWhile #-}

-- | Consumes the input as long as the predicate remains true.
takeWhile :: (Word8 -> Bool) -> Parser BS.ByteString
takeWhile prd = lift $ Parser $ \s _ success ->
  case BS.span prd s of
    (a,b) -> success b a
{-# INLINE takeWhile #-}

-- | Discards the input as long as the predicate remains true.
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile prd = lift $ Parser $ \s _ success -> success (BS.dropWhile prd s) ()
{-# INLINE skipWhile #-}

-- | Runs the parser. Will return a parse error if the parser fails
-- or if the input is not completely consumed.
parseOnly :: Parser a -> BS.ByteString -> Either ParseError a
parseOnly prs s = case lowerCodensity prs of
                  Parser p -> p s Left $ \b a -> if BS.null b
                                              then Right a
                                              else Left (ParseError (S.singleton (Tokens (BS.take 1 b))) mempty)

-- | Parses the remaining input.
remaining :: Parser BS.ByteString
remaining = lift $ Parser $ \input _ success -> success BS.empty input

-- | Parses days, with format YYYY-MM-DD
parseYMD :: Parser Day
parseYMD = do
    !y <- decimal <* char '-'
    !m <- decimal <* char '-'
    !d <- decimal
    return $! fromGregorian y m d

-- | Parses a difftime, with format HH:MM:SS
parseDTime :: Parser DiffTime
parseDTime = do
    !h  <- decimal <* char ':'
    !mi <- decimal <* char ':'
    !s  <- scientific
    return $! fromSeconds $ fromIntegral (h * 3600 + mi * 60 :: Int) + s

-- | Parses a whole timestamp, with format YYYY-MM-DD+HH:MM:SS+CEST.
-- This is very much *not* robust, as it only handles CET and CEST.
timestamp :: Parser UTCTime
timestamp = do
    !day <- parseYMD <* char '+'
    !difftime <- parseDTime <* char '+'
    let !tm = mkUTCTime day difftime
    !tz <- takeWhile1 isUpper
    return $! case tz of
                  "CEST" -> tm .-^ fromSeconds (7200 :: Int)
                  "CET" -> tm .-^ fromSeconds (3600 :: Int)
                  _ -> tm

-- | Parses RFC3339 compatible timestamps to UTCTime.
rfc3339 :: Parser UTCTime
rfc3339 = do
    !day <- parseYMD <* char 'T'
    !difftime <- parseDTime
    !o <- anyChar
    let !tm = mkUTCTime day difftime
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

-- | Turns any parser into a 'SimpleFold'.
pFold :: Parser a -> SimpleFold BS.ByteString a
pFold p = to (parseOnly p) . _Right
