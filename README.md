# concur-replica

[![CircleCI](https://circleci.com/gh/pkamenarsky/concur-replica.svg?style=svg)](https://circleci.com/gh/pkamenarsky/concur-replica)

Build interactive web UIs using nothing but Haskell.

No user-written JS. No transpiler.

## How?!

[Replica](https://github.com/pkamenarsky/replica): a remote virtual DOM library. Clients run a fixed JavaScript snippet which opens a connection to your Haskell server. The client uses it to notify the server of browser events, and the server uses it to send DOM updates to be applied by the client.

[Concur](https://github.com/ajnsit/concur): a UI framework that *reifies time* and as such generalizes the Elm architecture. It's based around `Widget`s, sections of the UI that may return a value.

## Upsides and downsides

This strategy is well understood due to its use by [Phoenix LiveView](https://hexdocs.pm/phoenix_live_view) (among other projects).

There are two situations where you should rule `concur-replica` out completely:

+ Realtime client-side feedback is required (this is rarely the case)

+ It needs to work offline

The strategy also has two softer costs:

+ Events and updates cost bandwidth (e.g. listening to mouse position, or doing an animation in Haskell instead of CSS)

+ It puts a heavier load on your server than a traditional stateless webapp

Additionally, `concur-replica` is alpha software so it currently has additional deficiencies. Check the issue tracker for these, and they'll be fixed as it matures.

Despite these caveats however, `concur-replica` has *incredible* benefits. No JS! No REST API layer -- you can call whatever Haskell functions you want directly from your UI code. `concur` is an awesome way of making UIs. And you have the Haskell classics: STM is absolutely unfair for building collaborative apps, and UIs are so annoying to test that the type system really shines.

## Obligatory counter example

```hs
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

## Projects using concur-replica

* [Unison Code Explorer](http://unison.readvar.com) - [Github repo](https://github.com/seagreen/unison-code-explorer)

## Running the examples

```
stack build
stack exec concur-replica-(calc|hilo|menu|misc|multi-entry|select|routing|website|chat)
```

Point your browser to `http://localhost:8080` and behold the remote VDOM goodness.
