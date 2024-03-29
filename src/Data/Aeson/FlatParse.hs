
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP          #-}

module Data.Aeson.FlatParse (json) where

import Control.Applicative (liftA2)
import Data.Aeson.KeyMap (fromMapText)
import Data.Aeson.Types (Value(..), Key, Object)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function (fix)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum(..))
import Data.Word (Word8)
import FlatParse.Basic hiding (string)


import Data.Vector (Vector)
import Data.Scientific ( scientific, Scientific )
import Data.Char (chr)
import qualified Data.Vector as Vector (empty, fromList, fromListN, reverse)

import Debug.Trace

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB

-- From aeson package
#if MIN_VERSION_text(2,0,0)
-- import Data.Aeson.Internal.ByteString
import GHC.ForeignPtr (ForeignPtr(ForeignPtr))
import Data.ByteString.Internal (ByteString (..))

import Data.Text.Array                (Array (..))
import Data.Text.Internal qualified as T (Text (..))

import Data.ByteString.Short.Internal qualified as SBS

#else
import Data.Text.Encoding qualified as TE

#endif

-- | The input is assumed to contain only 7bit ASCII characters (i.e. @< 0x80@).
--   We use TE.decodeLatin1 here because TE.decodeASCII is currently (text-1.2.4.0)
--   deprecated and equal to TE.decodeUtf8, which is slower than TE.decodeLatin1.
unsafeDecodeASCII :: ByteString -> T.Text

#if MIN_VERSION_text(2,0,0)
unsafeDecodeASCII bs = withBS bs $ \_fp len -> if len == 0 then T.empty else
  let !(SBS.SBS arr) = SBS.toShort bs in T.Text (ByteArray arr) 0 len

withBS :: ByteString -> (ForeignPtr Word8 -> Int -> r) -> r
#if MIN_VERSION_bytestring(0,11,0)
withBS (BS !sfp !slen)       kont = kont sfp slen
#else
withBS (PS !sfp !soff !slen) kont = kont (plusForeignPtr sfp soff) slen
#endif
{-# INLINE withBS #-}

#else
unsafeDecodeASCII = TE.decodeLatin1
#endif


-- punctuation
pattern W8_BACKSLASH    :: Word8
pattern W8_SLASH        :: Word8
pattern W8_DOUBLE_QUOTE :: Word8

pattern W8_BACKSLASH    = 92
pattern W8_SLASH        = 47
pattern W8_DOUBLE_QUOTE = 34

-- digits
pattern W8_0 :: Word8
pattern W8_9 :: Word8

pattern W8_0 = 48
pattern W8_9 = 57

-- lower case
pattern W8_a :: Word8
pattern W8_f :: Word8
pattern W8_n :: Word8
pattern W8_t :: Word8
pattern W8_r :: Word8
pattern W8_b :: Word8
pattern W8_u :: Word8

pattern W8_a = 97
pattern W8_f = 102
pattern W8_n = 110
pattern W8_t = 116
pattern W8_r = 114
pattern W8_b = 98
pattern W8_u = 117

-- upper case
pattern W8_A :: Word8
pattern W8_F :: Word8

pattern W8_A = 65
pattern W8_F = 70

skipSpace :: Parser e ()
skipSpace = many_ $ satisfyASCII_ $ \w -> w == ' ' || w == '\n' || w == '\r' || w == '\t'
{-# INLINE skipSpace #-}

data Err
  = UnexpectedNumber Word8
  | NumberPartMustBe0OrDigits Word8
  | ExponentOutOfBounds Integer
  | ExpectedNumber Word8
  | ExpectedHexDigit Word8
  | UnexpectedEscapeCharacter Word8
  | Other String
  deriving Show

peekWord8 :: Parser e Word8
peekWord8 = lookahead anyWord8

json ::  Parser Err Value
json = do
  skipSpace
  fix $ \value_ -> do
      $(switch [| case _ of
        "{"     -> skipSpace  *> object' value_
        "["     -> skipSpace  *> array' value_
        "false" -> Bool False <$ skipSpace
        "true"  -> Bool True  <$ skipSpace
        "null"  -> Null       <$ skipSpace
        "\""    -> string'
        "-"     -> number' (-1) 0
        "1"     -> number' 1 1
        "2"     -> number' 1 2
        "3"     -> number' 1 3
        "4"     -> number' 1 4
        "5"     -> number' 1 5
        "6"     -> number' 1 6
        "7"     -> number' 1 7
        "8"     -> number' 1 8
        "9"     -> number' 1 9
        "0"     -> number' 0 0
        |]
        )


value :: Parser Err Value
value = skipSpace *> fix (\val -> atoms <|> object val <|> array val <|> string <|> number)

atoms :: Parser e Value
atoms =
    $(switch [| case _ of
      "false" -> pure $ Bool False
      "true"  -> pure $ Bool True
      "null"  -> pure Null
      |]
    ) <* skipSpace

number :: Parser Err Value
number = do
  sign     <- ((-1::Int) <$ $(char '-')) <|> pure 1
  whole    <- readIntegerZ
  mDecimal <- optional (parsedLength readDecimal)
  let (mantissa,shift) = case mDecimal of
        Nothing          -> (whole                   , 0      )
        Just (dec,bytes) -> (whole*10^(bytes-1) + dec, bytes-1)
  exp      <- readExponent <|> pure 0
  let !res = scientific (if sign < 0 then -mantissa else mantissa) (exp - shift)
  skipSpace
  pure $ Number res
{-# INLINE number #-}


number' :: Int -> Integer -> Parser Err Value
number' 0 _ = do
  setBack# 1
  number
number' sign start = do
  whole    <- readIntegerZ' start
  mDecimal <- optional (parsedLength readDecimal)
  let (mantissa,shift) = case mDecimal of
        Nothing          -> (whole                   , 0      )
        Just (dec,bytes) -> (whole*10^(bytes-1) + dec, bytes-1)
  exp      <- readExponent <|> pure 0
  let !res = scientific (if sign < 0 then -mantissa else mantissa) (exp - shift)
  skipSpace
  pure $ Number res
{-# INLINE number' #-}


readDecimal :: Parser Err Integer
readDecimal = $(char '.') *> readInteger
{-# INLINE readDecimal #-}

readExponent :: Parser Err Int
readExponent = do
  $(char 'e') <|> $(char 'E')
  sign <- ((-1) <$ $(char '-')) <|> (1 <$ $(char '+')) <|> pure 1
  i <- fromIntegral <$> readInteger
  if abs i > -(fromIntegral (minBound @Int))
    then err $ ExponentOutOfBounds i
    else pure $! fromIntegral (if sign < 0 then -i else i)
{-# INLINE readExponent #-}

readIntegerZ :: Parser Err Integer
readIntegerZ = do
  w <- peekWord8
  case w of
    W8_0 -> do
      anyWord8
      peekWord8 >>= \case
          d | W8_0 <= d && d <= W8_9 -> err $ NumberPartMustBe0OrDigits d
          _ -> pure 0
    d | W8_0 < d && d <= W8_9 -> readInteger
      | otherwise -> err $ ExpectedNumber d
{-# INLINE readIntegerZ #-}

readIntegerZ' :: Integer -> Parser Err Integer
readIntegerZ' n = do
  w <- peekWord8
  case w of
    d | W8_0 <= d && d <= W8_9 -> anyWord8 *> readIntegerZ' (n*10 + fromIntegral (d - W8_0))
      | otherwise -> pure n
{-# INLINE readIntegerZ' #-}


parsedLength :: Parser e a -> Parser e (a,Int)
parsedLength p = withSpan p $ \a (Span (Pos start) (Pos end)) ->
  pure (a,start - end) -- position is from end of bytestring
{-# INLINE parsedLength #-}


string :: Parser Err Value
string = String <$> text
{-# INLINE string #-}

string' :: Parser Err Value
string' = String <$> text'
{-# INLINE string' #-}

text :: Parser Err Text
text = do
  $(char '"')
  text'
{-# INLINE text #-}

text' :: Parser Err Text
text' = do
  (builder, Sum totalLen) <- chainl (<>) (pure mempty) (asciiSpan <|> nonAsciiChars <|> escapedChars)
  $(char '"')
  let !res = TL.toStrict $ TB.toLazyTextWith totalLen builder
  skipSpace
  pure res
{-# INLINE text' #-}

asciiSpan :: Parser e (Builder, Sum Int)
asciiSpan = do
  bytes <- byteStringOf (some_ (satisfyASCII (\c -> c >= ' ' && c < '\128' && c /= '"' && c /= '\\')))
  pure (TB.fromText $ unsafeDecodeASCII bytes, Sum $ BS.length bytes)
{-# INLINE asciiSpan #-}

nonAsciiChars :: Parser e (Builder, Sum Int)
nonAsciiChars = do
  bytes <- byteStringOf (some_ nonAsciiChar)
  pure (TB.fromText $ decodeUtf8 bytes, Sum $ BS.length bytes)
{-# INLINE nonAsciiChars #-}

nonAsciiChar :: Parser e ()
nonAsciiChar = fusedSatisfy_ (\c -> c >= '\128') (const True) (const True) (const True)
{-# INLINE nonAsciiChar #-}

escapedChars :: Parser Err (Builder, Sum Int)
escapedChars = do
  $(char '\\')
  anyWord8 >>= \case
    W8_DOUBLE_QUOTE -> pure (TB.singleton '"',  1)
    W8_BACKSLASH    -> pure (TB.singleton '\\', 1)
    W8_SLASH        -> pure (TB.singleton '/',  1)
    W8_b            -> pure (TB.singleton '\b', 1)
    W8_f            -> pure (TB.singleton '\f', 1)
    W8_n            -> pure (TB.singleton '\n', 1)
    W8_r            -> pure (TB.singleton '\r', 1)
    W8_t            -> pure (TB.singleton '\t', 1)
    W8_u            -> do
      a <- hexDigit
      b <- hexDigit
      c <- hexDigit
      d <- hexDigit
      pure (TB.singleton $ chr $ a*4096 + b*256 + c*16 + d, 4) -- hack, could be smaller than this
    d -> err $ UnexpectedEscapeCharacter d
{-# INLINE escapedChars #-}

hexDigit :: Parser Err Int
hexDigit = anyWord8 >>= \case
  d | W8_0 <= d && d <= W8_9 -> pure $! fromIntegral $ d - W8_0
    | W8_a <= d && d <= W8_f -> pure $! fromIntegral $ d - W8_a+10
    | W8_A <= d && d <= W8_F -> pure $! fromIntegral $ d - W8_A+10
    | otherwise              -> err $ ExpectedHexDigit d
{-# INLINE hexDigit #-}

object :: Parser Err Value -> Parser Err Value
object val = do
  $(char '{')
  skipSpace
  object' val

object' :: Parser Err Value -> Parser Err Value
object' val = do
  let pair = liftA2 (,) (text <* $(char ':') <* skipSpace) val
  pairs <- liftA2 (:) pair (many ($(char ',') *> skipSpace *> pair)) <|> pure []
  $(char '}')
  skipSpace
  pure $! Object $! fromMapText $ Map.fromList pairs
{-# INLINE object' #-}

array :: Parser e Value -> Parser e Value
array val = do
  $(char '[')
  skipSpace
  array' val

array' :: Parser e Value -> Parser e Value
array' val = do
  (vals, len) <- ((\v (vs,len) -> (v:reverse vs, len)) <$> val <*> go [] 1) <|> pure ([],0)
  $(char ']')
  skipSpace
  pure $! Array $! Vector.fromListN len vals
  where
    go !vs !n =
        ($(char ',') *> skipSpace *> val >>= \v -> go (v:vs) (n+1))
        <|> pure (vs, n)
{-# INLINE array' #-}
