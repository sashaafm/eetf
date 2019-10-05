# Erlang's External Term Format in Elm

This package helps you convert between Elm values and Erlang's External Term
Format (ETF) values. In essence, this library allows to communicate back and
forth between your Elm application and you BEAM language (Erlang, Elixir, LFE,
etc) programs using your desired transport mechanism (HTTP, Websocket or
others).

## Usage

This set of encoders and decoders was written with
[`elm/json`][https://package.elm-lang.org/packages/elm/json/latest/] in mind and
as such it adheres to the same APIs as said package. Anyone experienced using
the official JSON parsing library shall find this one very familiar.

### Example

The example provided in `elm/json` is shown here using the `eetf` package
instead:

```elm
module Cause exposing (Cause, encode, decoder)

import Eetf.Decode as D
import Eetf.Encode as E


-- CAUSE OF DEATH

type alias Cause =
  { name : String
  , percent : Float
  , per100k : Float
  }


-- ENCODE

encode : Cause -> E.Value
encode cause =
  E.object
    [ ("name", E.string cause.name)
    , ("percent", E.float cause.percent)
    , ("per100k", E.float cause.per100k)
    ]


-- DECODER

decoder : D.Decoder Cause
decoder =
  D.map3 Cause
    (D.field "name" D.string)
    (D.field "percent" D.float)
    (D.field "per100k" D.float)
```

### Tuples

JSON lacks support for tuples as presented by Elm. Therefore, the official
`elm/json` package lacks direct parsers to decode or encode Elm tuples into JSON
tuples (which do not exist) or vice-versa.

Erlang has tuples similarly to Elm. The only difference being that in Erlang
tuples can have up to 255 elements while in Elm the maximum number is 3.
However, the fact that tuples are an implemented data structure in Erlang
allowed the introduction of the `tuple`, `tuple2` and `tuple3` function for both
encoding and decoding in this package.

### Caveats

Unfortunately, due to Elm's strict type system, supporting dictionaries (maps in
Erlang and Elixir) with keys of any type belonging to Elm's special `comparable`
type was discarded in favor of only supporting `String` keys. It may be possible
to support keys belonging to the `comparable` type, however, the API would
become cumbersome and user friendliness would degrade a lot.

`String` keys were chosen as the way to go since they are the most common
occurrence in the Erlang-world use cases. For example, maps in Erlang (or
Elixir) are usually employed to represent data payloads (e.g., a JSON body
delivered via HTTP) or database rows (where the keys represent the column names
either as Erlang strings or atoms).
