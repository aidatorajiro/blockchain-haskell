module Transaction where

import qualified Data.ByteString as B

data Txin = Txin {
  txinHash       :: B.ByteString,  -- hash of the to-be-used transaction
  txinIndex      :: Int,           -- index of the to-be-used transaction
  txinScript     :: B.ByteString,  -- unlock script
  txinSequenceNo :: Int            -- sequence no
} deriving Eq

data Txout = Txout {
  txoutAmount :: Int,              -- amount of btc to use
  txoutScript :: B.ByteString      -- lock script
} deriving Eq

-- A witness field is an array of stack items.
-- A witness consists of an array of witness fields, each of them associated with a txin.
-- Since a witness is not a script, serialization of stack items will be done out of script layer.
-- This is why Witness has a two dimentional array of ByteString.
data Witness = Witness {
  witnessFields :: [[B.ByteString]]
} deriving Eq

data Transaction = Transaction {
  transactionIsSegwit :: Bool,          -- whether segwit or not
  transactionVersion  :: Int,           -- version of the transaction
  transactionMarker   :: Maybe Int,     -- transaction marker (if segwit)
  transactionFlag     :: Maybe Int,     -- transactio flag (if segwit)
  transactionTxins    :: [Txin],        -- list of unlock scripts
  transactionTxouts   :: [Txout],       -- list of lock scripts
  transactionWitness  :: Maybe Witness, -- transaction witness (if segwit)
  transactionLocktime :: Int            -- locktime of the transaction
} deriving Eq