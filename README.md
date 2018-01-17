# BlockchainHaskell

A blockchain library for haskell.

## Reference

### Parse a raw bitcoin transaction
Use a haskell library [binary](https://hackage.haskell.org/package/binary) to parse.

```haskell
ParseTransaction :: bool -> Get Transaction
```

The input bool tells whether the given transaction is segwit style or not.

An example:

```haskell
import Data.ByteString.Base16.Lazy (encode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Binary.Get
import Control.Applicative ((<|>))

import Transaction
import ParseTransaction
import ShowTransaction

parsetx hex = runGet (parseTransaction True <|> parseTransaction False) (fst $ decode $ pack hex)

main :: IO ()
main = print $ parsetx "01000000000101....."
```

## Features

### implemented
- Parse a Bitcoin transaction

### not implemented
- Simulate a Bitcoin system monadically
- Bitcoin daemon
