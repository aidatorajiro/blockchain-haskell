module ParseTransaction where

import Data.Binary.Get
import Data.Char
import GHC.Word
import Control.Applicative ((<|>), Alternative, optional)
import Control.Monad (replicateM, when, unless, guard)

import Transaction

-- Parse a raw transaction.
-- First argument represents wheather use Segwit-style transaction format (BIP 144) or not.
parseTransaction :: Bool -> Get Transaction
parseTransaction isSegwit = do
  version <- cast getWord32le

  -- If segwit
  marker <- opt isSegwit (cast getWord8)
  flag   <- opt isSegwit (cast getWord8)

  -- Run parseTxin <numTxins> times
  numTxins <- parseVI
  txins <- replicateM numTxins parseTxin

  -- Run parseTxout <numTxouts> times
  numTxouts <- parseVI
  txouts <- replicateM numTxouts parseTxout

  -- Parse witness if segwit.
  witness <- opt isSegwit (parseWitness numTxins)

  locktime <- cast getWord32le

  flip unless (fail "parse failed") =<< isEmpty

  return $ Transaction isSegwit version marker flag txins txouts witness locktime

parseTxin :: Get Txin
parseTxin = do
  hash   <- getByteString 32
  index  <- cast getWord32le
  script <- getByteString =<< parseVI
  seqno  <- cast getWord32le
  
  return $ Txin hash index script seqno

parseTxout :: Get Txout
parseTxout = do
  amount <- cast getWord64le
  script <- getByteString =<< parseVI

  return $ Txout amount script

-- get a witness whose number of fields is given int
parseWitness :: Int -> Get Witness
parseWitness numFields =
  fmap Witness $
    replicateM numFields $ do
      numItems <- parseVI
      replicateM numItems $ 
        getByteString =<< parseVI

-- Parse Bitcoin's VarInt.
parseVI :: Get Int
parseVI = 
  cast (be 0xFF >> getWord64le) <|>
  cast (be 0xFE >> getWord32le) <|>
  cast (be 0xFD >> getWord16le) <|>
  cast getWord8

-- Match a specific byte.
be :: Word8 -> Get Word8
be x = do
  w <- getWord8
  if w == x
    then return w
    else fail "mismatch at be"

-- Cast to Int
cast :: Integral a => Get a -> Get Int
cast = fmap fromIntegral

-- if given bool is true, do given action and return Just. if false, return Nothing.
opt :: Monad m => Bool -> m a -> m (Maybe a)
opt b a = if b then Just <$> a else return Nothing