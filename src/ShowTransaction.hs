module ShowTransaction where

import qualified Data.ByteString as B
import Data.ByteString.Base16
import Data.List
import Data.Maybe (fromJust)

import Transaction
import ParseTransaction

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