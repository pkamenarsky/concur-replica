# concur-replica

[![CircleCI](https://circleci.com/gh/pkamenarsky/concur-replica.svg?style=svg)](https://circleci.com/gh/pkamenarsky/concur-replica)

`concur-replica` is a [Concur](https://github.com/ajnsit/concur) backend for [Replica](https://github.com/pkamenarsky/replica). As such, you should make yourself familiar with both the [Concur docs](https://github.com/ajnsit/concur-documentation/blob/master/README.md) and [Replica docs](https://github.com/pkamenarsky/replica/blob/master/README.md) first.

## Obligatory counter example

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Core (Widget, orr)
import Concur.Replica

import Prelude hiding (div)

counter :: Int -> Widget HTML a
counter x = do
  click <- div []
    [ Left  <$> div [ onClick ] [ text "-" ]
    , text $ T.pack $ show x
    , Right <$> div [ onClick ] [ text "+" ]
    ]

  case click of
    Left _  -> counter (x - 1)
    Right _ -> counter (x + 1)

main :: IO ()
main = runDefault 8080 "Counter" (counter 0)
```

## Running the examples

The examples are mostly taken from `concur-react`, but there are some new ones as well under `examples/Misc`.

```
stack build
stack exec concur-replica-(calc|hilo|menu|misc|multi-entry)
```

Point your browser at `http://localhost:8080` and behold the godly combination of **Concur**rent, **Replica**ted remote VDOM goodness 🤘.
