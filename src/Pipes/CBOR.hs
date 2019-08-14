{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | [pipes](https://hackage.haskell.org/package/pipes) utilities for encoding
-- and decoding values as byte streams in CBOR format using the
-- [cborg](https://hackage.haskell.org/package/cborg) library.

module Pipes.CBOR (
  -- * Encoding
    serialise
  , encode
  -- * Decoding
  , deserialise
  , decode
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.ST (stToIO)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read as CBOR
  (IDecode(..), ByteOffset, DeserialiseFailure, deserialiseIncremental)
import qualified Codec.CBOR.Write as CBOR (toLazyByteString)
import qualified Codec.Serialise as Ser
import qualified Data.ByteString as B
import qualified Pipes as P
import qualified Pipes.ByteString as Pb
import qualified Pipes.Parse as Pp

--------------------------------------------------------------------------------

-- | Renders @x@ to a byte stream using its 'Ser.Serialise' instance.
serialise
 :: (Ser.Serialise x, Monad m)
 => x -- ^
 -> P.Producer' B.ByteString m ()
serialise = encode . Ser.encode
{-# INLINE serialise #-}

-- | Renders an 'CBOR.Encoding' to a byte stream.
encode
  :: Monad m
  => CBOR.Encoding -- ^
  -> P.Producer' B.ByteString m ()
encode = Pb.fromLazy . CBOR.toLazyByteString
{-# INLINE encode #-}

--------------------------------------------------------------------------------

-- | Parses @x@ from a byte stream using its 'Ser.Serialise' instance.
--
-- Also returns the number of bytes consumed in order to to decode the value.
--
-- Implementation note: No, ideally this function shouldn't run in 'IO'. But
-- unfortunately, the underlying 'CBOR.deserialiseIncremental' and its use of
-- 'Control.Monad.ST.ST', which becomes both covariant and contravariant in
-- 'Pp.Parser', make removing the 'IO' tricky. The only 'IO' this function
-- performs is 'stToIO'.
deserialise
  :: (MonadIO m, Ser.Serialise a)
  => Pp.Parser B.ByteString m (Either CBOR.DeserialiseFailure (CBOR.ByteOffset, a)) -- ^
deserialise = decode Ser.decode
{-# INLINE deserialise #-}

-- | Parses @x“ from a byte stream using the given 'CBOR.Decoder'.
--
-- Also returns the number of bytes consumed in order to to decode the value.
--
-- Implementation note: No, ideally this function shouldn't run in 'IO'. But
-- unfortunately, the underlying 'CBOR.deserialiseIncremental' and its use of
-- 'Control.Monad.ST.ST', which becomes both covariant and contravariant in
-- 'Pp.Parser', make removing the 'IO' tricky. The only 'IO' this function
-- performs is 'stToIO'.
decode
  :: MonadIO m
  => (forall s. CBOR.Decoder s x)
  -> Pp.Parser B.ByteString m (Either CBOR.DeserialiseFailure (CBOR.ByteOffset, x)) -- ^
decode dec = S.StateT (go id (CBOR.deserialiseIncremental dec))
  where
    go diffP m = \p0 -> do
      idec <- liftIO (stToIO m)
      case idec of
         CBOR.Fail _ _ err -> pure (Left err, diffP p0)
         CBOR.Done bs off a -> pure (Right (off, a), P.yield bs >> p0)
         CBOR.Partial k -> do
            x <- nextSkipEmpty p0
            case x of
               Left e -> go diffP (k Nothing) (pure e)
               Right (bs, p1) -> go (diffP . (P.yield bs >>)) (k (Just bs)) p1
{-# INLINABLE decode #-}

--------------------------------------------------------------------------------

-- | Like 'P.next', except it skips leading 'B.null' chunks.
nextSkipEmpty
  :: Monad m
  => P.Producer B.ByteString m r
  -> m (Either r (B.ByteString, P.Producer B.ByteString m r))
nextSkipEmpty = go
  where
    go p0 = do
      x <- P.next p0
      case x of
         Left _ -> pure x
         Right (a, p1)
          | B.null a -> go p1
          | otherwise -> pure x
{-# INLINABLE nextSkipEmpty #-}

