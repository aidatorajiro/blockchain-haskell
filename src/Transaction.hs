module Transaction (
  Txin(..),
  Txout(..),
  Witness(..),
  Transaction(..),
  parseTransaction,
  parseTxin,
  parseTxout, 
  parseWitness,
  parseTransactionFromHexString
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16.Lazy as Base16
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Base16
import Data.List
import Data.Maybe (fromJust)
import Data.Binary.Get
import Data.Char
import GHC.Word
import Control.Applicative ((<|>), Alternative, optional)
import Control.Monad (replicateM, when, unless, guard)
import ParserUtil

data Txin = Txin {
  txinHash       :: B.ByteString,  -- | hash of the to-be-used transaction
  txinIndex      :: Int,           -- | index of the to-be-used transaction
  txinScript     :: B.ByteString,  -- | unlock script
  txinSequenceNo :: Int            -- | sequence no
} deriving Eq

data Txout = Txout {
  txoutAmount :: Int,              -- | amount of btc to use
  txoutScript :: B.ByteString      -- | lock script
} deriving Eq

-- | A witness field is an array of stack items.
-- | A witness consists of an array of witness fields, and each of them is associated with a txin.
-- | Since a witness is not a script, serialization of stack items will be done out of script layer.
-- | This is why Witness has a two dimentional array of ByteString.
data Witness = Witness {
  witnessFields :: [[B.ByteString]]
} deriving Eq

data Transaction = Transaction {
  transactionIsSegwit :: Bool,          -- | whether segwit or not
  transactionVersion  :: Int,           -- | version of the transaction
  transactionMarker   :: Maybe Int,     -- | transaction marker (if segwit)
  transactionFlag     :: Maybe Int,     -- | transactio flag (if segwit)
  transactionTxins    :: [Txin],        -- | list of unlock scripts
  transactionTxouts   :: [Txout],       -- | list of lock scripts
  transactionWitness  :: Maybe Witness, -- | transaction witness (if segwit)
  transactionLocktime :: Int            -- | locktime of the transaction
} deriving Eq

listLines :: (a -> [String]) -> [a] -> [String]
listLines f l =
  concatMap
    (\(i, x) ->
      (show i ++ ": ") : (indent 2) (f x)
    ) (zip (take (length l) [0..]) l)

txinLines :: Txin -> [String]
txinLines (Txin h i s n) = [
  "Txin [",
  "  hash: "        ++ (show . encode) h,
  "  index: "       ++ show i,
  "  script: "      ++ (show . encode) s,
  "  sequence_no: " ++ show n,
  "]"]

txoutLines :: Txout -> [String]
txoutLines (Txout a s) = [
  "Txout [",
  "  amount: " ++ show a,
  "  script: " ++ (show . encode) s,
  "]"]

witnessLines :: Witness -> [String]
witnessLines (Witness f) =
  "Witness [" :
  "  fields:" : (indent 4) (listFields f) ++ [
  "]"]

transactionLines :: Transaction -> [String]
transactionLines (Transaction True v m f i o w l) = [
  "Transaction [",
  "  version: " ++ show v,
  "  marker: " ++ (show . fromJust) m,
  "  flag: " ++ (show . fromJust) f ] ++
  "  txins: " : (indent 4 . listLines txinLines) i ++
  "  txouts: " : (indent 4 . listLines txoutLines) o ++
  "  witness: " : (indent 4 . witnessLines) (fromJust w) ++ [
  "  locktime:  " ++ show l,
  "]" ]

transactionLines (Transaction False v _ _ i o _ l) = [
  "Transaction [",
  "  version: "  ++ show v ] ++
  "  txins: "  : (indent 4 . listLines txinLines) i ++
  "  txouts: " : (indent 4 . listLines txoutLines) o  ++ [
  "  locktime: " ++ show l,
  "]"]

listFields :: [[B.ByteString]] -> [String]
listFields x = ["["] ++ map (("  "++) . unwords . map (show . encode)) x ++ ["]"]

indent :: Int -> [String] -> [String]
indent n = map (replicate n ' ' ++)

join :: [String] -> String
join = intercalate "\n"

instance Show Txin where
  show x = join (txinLines x)

instance Show Txout where
  show x = join (txoutLines x)

instance Show Witness where
  show x = join (witnessLines x)

instance Show Transaction where
  show x = join (transactionLines x)

-- | Parse a raw transaction.
-- | First argument represents wheather use Segwit-style transaction format (BIP 144) or not.
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

parseTransactionFromHexString :: [Char] -> Transaction
parseTransactionFromHexString hex = runGet (parseTransaction True <|> parseTransaction False) (fst $ Base16.decode $ pack hex)

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

-- | get a witness whose number of fields is given int
parseWitness :: Int -> Get Witness
parseWitness numFields =
  fmap Witness $
    replicateM numFields $ do
      numItems <- parseVI
      replicateM numItems $ 
        getByteString =<< parseVI
