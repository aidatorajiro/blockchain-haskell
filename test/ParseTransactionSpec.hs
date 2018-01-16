module ParseTransactionSpec (test) where

import Data.ByteString.Base16.Lazy (encode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Binary.Get
import Control.Applicative ((<|>))

import Transaction
import ParseTransaction
import ShowTransaction

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

parsetx hex = runGet (parseTransaction True <|> parseTransaction False) (fst $ decode $ pack hex)

test :: IO ()
test = hspec $ do
  describe "ParseTransaction.parseTransaction"