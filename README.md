# concur-replica

[![CircleCI](https://circleci.com/gh/pkamenarsky/concur-replica.svg?style=svg)](https://circleci.com/gh/pkamenarsky/concur-replica)

`concur-replica` is a [Concur](https://github.com/ajnsit/concur) backend for [Replica](https://github.com/pkamenarsky/replica). As such, you might want to make yourself familiar with both projects first.

## Obligatory counter example

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Concur.Core
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

The examples are mostly taken from `concur-react`, but there are a couple of new ones ones under `examples/Misc` as well.

```
stack build
stack exec concur-replica-(calc|hilo|menu|misc|multi-entry)
```

Point your browser to `http://localhost:8080` and behold the remote VDOM goodness.
