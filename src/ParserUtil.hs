module ParserUtil where

import qualified Data.ByteString as B
import Data.Binary.Get
import GHC.Word
import Control.Applicative ((<|>))

-- | Parse Bitcoin's VarInt.
parseVI :: Get Int
parseVI = 
  cast (be 0xFF >> getWord64le) <|>
  cast (be 0xFE >> getWord32le) <|>
  cast (be 0xFD >> getWord16le) <|>
  cast getWord8

-- | Match a specific byte.
be :: Word8 -> Get Word8
be x = do
  w <- getWord8
  if w == x
    then return w
    else fail "mismatch at be"

-- | Cast to Int
cast :: Integral a => Get a -> Get Int
cast = fmap fromIntegral

-- | if given bool is true, do given action and return Just. if false, return Nothing.
opt :: Monad m => Bool -> m a -> m (Maybe a)
opt b a = if b then Just <$> a else return Nothing