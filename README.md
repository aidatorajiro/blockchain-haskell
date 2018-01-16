# BlockchainHaskell

A blockchain library for haskell.

## Usage

### Parse a raw bitcoin transaction
Use a haskell library [binary](https://hackage.haskell.org/package/binary) to parse.

```haskell
ParseTransaction :: bool -> Get Transaction
```

The input bool tells whether the given transaction is segwit style or not.
