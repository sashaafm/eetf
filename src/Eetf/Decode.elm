module Eetf.Decode exposing
  ( Decoder
  , Value
  , Error(..)
  , andThen
  , at
  , array
  , bool
  , decodeValue
  , decodeBytes
  , dict
  , fail
  , field
  , float
  , index
  , int
  , keyValuePairs
  , list
  , map
  , map2
  , map3
  , map4
  , map5
  , map6
  , map7
  , map8
  , maybe
  , oneOf
  , string
  , succeed
  , tuple
  , tuple2
  , tuple3
  )

{-| Turn Erlang External Term Format (ETF) values into Elm values. Check out the
official [documentation][docs] to better understand how this library and the
Erlang side work.

[docs]: http://erlang.org/doc/apps/erts/erl_ext_dist.html

# Primitives
@docs Decoder, string, bool, int, float

# Data structures
@docs list, array, dict, keyValuePairs, tuple, tuple2, tuple3

# Object Primitives
@docs field, at, index

# Incosistent Structure
@docs maybe, oneOf

# Run Decoders
@docs decodeBytes, decodeValue, Value, Error

# Mapping

@docs map, map2, map3, map4, map5, map6, map7, map8

# Fancy Decoding
@docs succeed, fail, andThen
-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode
import Dict exposing (Dict)
import Eetf exposing (Term(..))

{-| A value that knows how to decode ETF values.

This whole API is similar to `elm/json`. Maybe check the section about decoders
in `guide.elm-lang.org` to better undestand how to use this module.
-}
type Decoder a = Decoder (Value -> Result Error a)

{-| Represents an External Term Format value.
-}
type alias Value = Term

{-| A structured error describing exactly how the decoder failed. This is meant
to help you understand and inform of the problem occurred during the decoding
process.
-}
type Error
  = WrongType String
  | InvalidTupleSize String
  | MapFieldNotFound String
  | IndexOutOfBounds String
  | NoMatchingDecoder String
  | UnableToDecode String
  | InvalidTerm String

{-| Run a `Decoder` on some Erlang Term `Value`.
-}
decodeValue : Decoder a -> Value -> Result Error a
decodeValue (Decoder decoder) value = decoder value

{-| Parse the given `Bytes` sequence into an Erlang Term `Value` and then run
the `Decoder` on it. This will fail if the `Bytes` sequence is not a valid
Erlang Term.
-}
decodeBytes : Decoder a -> Bytes -> Result Error a
decodeBytes decoder bin =
  case Eetf.fromBytes bin of
    Ok val ->
      decodeValue decoder val
    Err error ->
      Err (InvalidTerm "Binary sequence does not represent an Erlang term.")

-- Primitives

{-| Decode an Erlang string into an Elm `String`.

Any Erlang atom or binary will be decoded successfully.
-}
string : Decoder String
string =
  Decoder <| \term ->
    case term of
      Atom val ->
        Ok val
      Binary bin ->
        case bytesToString bin of
          Just str -> Ok str
          Nothing -> Err (WrongType "Not a string.")
      _ ->
        Err (WrongType "Not a string.")

{-| Decode an Erlang integer into an Elm `Int`.
-}
int : Decoder Int
int =
  Decoder <| \term ->
    case term of
      Integer i -> Ok i
      _ -> Err (WrongType "Not an integer.")

{-| Decode an Erlang float into an Elm `Float`.
-}
float : Decoder Float
float =
  Decoder <| \term ->
    case term of
      FloatingPoint f -> Ok f
      _ -> Err (WrongType "Not a float.")

{-| Decode an Erlang boolean into an Elm `Bool`.

Either the Erlang atom `true` or `false` will be decoded sucessfully.
-}
bool : Decoder Bool
bool =
  Decoder <| \term ->
    case term of
      Atom "true" -> Ok True
      Atom "false" -> Ok False
      _ -> Err (WrongType "Not a boolean.")

{-| Decode an Erlang tuple of size 1 into an Elm `Tuple`.
-}
tuple : Decoder a -> Decoder (a)
tuple decoder =
  Decoder <| \term ->
    case term of
      Tuple [val] -> decodeValue decoder val
      Tuple _ -> Err (InvalidTupleSize "Tuple size must be 1.")
      _ -> Err (WrongType "Not a tuple.")

{-| Decode an Erlang tuple of size 2 into an Elm `Tuple`.
-}
tuple2 : Decoder a -> Decoder b -> Decoder (a, b)
tuple2 decoderA decoderB =
  Decoder <| \term ->
    case term of
      Tuple [valA, valB] ->
        case (decodeValue decoderA valA, decodeValue decoderB valB) of
          (Ok resA, Ok resB) -> Ok (resA, resB)
          (Err err, _) -> Err err
          (_, Err err) -> Err err
      Tuple _ ->
        Err (InvalidTupleSize "Tuple size must be 2.")
      _ ->
        Err (WrongType "Not a tuple.")

{-| Decode an Erlang tuple of size 3 into an Elm `Tuple`.
-}
tuple3 : Decoder a -> Decoder b -> Decoder c -> Decoder (a, b, c)
tuple3 decoderA decoderB decoderC =
  Decoder <| \term ->
    case term of
      Tuple [valA, valB, valC] ->
        case (decodeValue decoderA valA, decodeValue decoderB valB, decodeValue decoderC valC) of
          (Ok resA, Ok resB, Ok resC) -> Ok (resA, resB, resC)
          (Err err, _, _) -> Err err
          (_, Err err, _) -> Err err
          (_, _, Err err) -> Err err
      Tuple _ ->
        Err (InvalidTupleSize "Tuple size must be 3.")
      _ ->
        Err (WrongType "Not a tuple.")

{-| Decode an Erlang list into an Elm `List`.
-}
list : Decoder a -> Decoder (List a)
list decoder =
  Decoder <| \term ->
    case term of
      ProperList list_ ->
        foldResult (List.map (decodeValue decoder) list_)
      ImproperList list_ tail_ ->
        foldResult (List.map (decodeValue decoder) (list_ ++ [tail_]))
      Binary bin ->
        decodeBytes (list decoder) bin
      _ -> Err (WrongType "Not a list.")

{-| Decode an Erlang list into an Elm `Array`.
-}
array : Decoder a -> Decoder (Array a)
array decoder =
  decoder
  |> list
  |> andThen (\list_ ->
    succeed (Array.fromList list_)
  )

{-| Decode an Erlang map into an Elm `Dict`.

The map's keys must be decoded into an Elm `comparable` due to `Dict`'s
limitations regarding the possible type for its keys.
-}
dict : Decoder (comparable, a) -> Decoder (Dict comparable a)
dict decoder =
  decoder
  |> list
  |> andThen (\list_ ->
    succeed (Dict.fromList list_)
  )

{-| Decode an Erlang key-value pair list (usually called a `proplist` in Erlang,
or a `Keyword` list in Elixir) into an Elm `List` of pairs.
-}
keyValuePairs : Decoder (a, b) -> Decoder (List (a, b))
keyValuePairs decoder =
  Decoder <| \term ->
    case term of
      ProperList list_ ->
        foldResult (List.map (decodeValue decoder) list_)
      ImproperList list_ tail_ ->
        foldResult (List.map (decodeValue decoder) (list_ ++ [tail_]))
      _ ->
        Err (WrongType "Not a list.")

{-| Decode an Erlang key-value pair list (usually called a `proplist` in Erlang,
or a `Keyword` list in Elixir) into an Elm `List` of pairs.
-}
field : String -> Decoder a -> Decoder a
field field_ decoder =
  Decoder <| (\term ->
    fetchNestedFromMap [field_] decoder term
  )

{-| Decode a nested Erlang map, requiring certain fields.
-}
at : List String -> Decoder a -> Decoder a
at fields decoder =
  Decoder <| (\term ->
    fetchNestedFromMap fields decoder term
  )

{-| Decode an Erlang list, requiring a particular index.
-}
index : Int -> Decoder a -> Decoder a
index index_ decoder =
  let
      fetchIndexFromList = \l ->
        case Array.get index_ (Array.fromList l) of
          Just val -> decodeValue decoder val
          Nothing ->
            Err
              (IndexOutOfBounds
                ("Index " ++ String.fromInt index_ ++ " is out of bounds.")
              )
  in
  Decoder <| \term ->
    case term of
      ProperList list_ -> fetchIndexFromList list_
      ImproperList list_ _ -> fetchIndexFromList list_
      _ -> Err (WrongType "Not a list.")

-- Special

{-| Ignore the Erlang Term and produce a certain Elm value.
-}
succeed : a -> Decoder a
succeed val = Decoder <| \_ -> Ok val

{-| A decoder that always fails. This can be useful when using `andThen` to
decode custom types.
-}
fail : String -> Decoder a
fail error = Decoder <| \_ -> Err (UnableToDecode error)

{-| Create decoders that depend on previous results.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen next decoder =
  Decoder <| (\term ->
    case decodeValue decoder term of
      Ok val -> decodeValue (next val) term
      Err err -> Err err
  )

{-| Helpful for dealing with optional fields.
-}
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
  Decoder <| (\term ->
    case decodeValue decoder term of
      Ok val -> Ok (Just val)
      Err _ -> Ok Nothing
  )

{-| Try a bunch of different decoders. This can be useful if the Erlang Term may
come in a couple of different formats.

A common use-case may be to handle Erlang's dynamic nature more easily. For
example, certain function may have different return types (`ok` vs. `{error,
Reason}`).
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
  Decoder <| (\term ->
    let decodeFunc = \decode -> decodeValue decode term in
    decoders
    |> List.map decodeFunc
    |> findSuccessful
  )

-- Mappers

{-| Transform a decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map func (Decoder decoder) =
  Decoder <| \term ->
    case decoder term of
      Ok decodedTerm -> Ok (func decodedTerm)
      Err err -> Err err

{-| Transform two decoders and then combine the result. This is useful to decode
an Erlang map into an Elm `Record`.
-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 func (Decoder decoderA) (Decoder decoderB) =
  Decoder <| \term ->
    case decoderA term of
      Err errA -> Err errA
      Ok decodedTermA ->
        case decoderB term of
          Err errB -> Err errB
          Ok decodedTermB -> Ok (func decodedTermA decodedTermB)

{-| Transform three decoders and then combine the result. This is useful to
decode an Erlang map into an Elm `Record`.
-}
map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 func (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) =
  Decoder <| \term ->
    case decoderA term of
      Err errA -> Err errA
      Ok decodedTermA ->
        case decoderB term of
          Err errB -> Err errB
          Ok decodedTermB ->
            case decoderC term of
              Err errC -> Err errC
              Ok decodedTermC -> Ok (func decodedTermA decodedTermB decodedTermC)

{-| Transform four decoders and then combine the result. This is useful to
decode an Erlang map into an Elm `Record`.
-}
map4 :
  (a -> b -> c -> d -> e)
  -> Decoder a
  -> Decoder b
  -> Decoder c
  -> Decoder d
  -> Decoder e
map4 func (Decoder decoderA) (Decoder decoderB) (Decoder decoderC) (Decoder decoderD) =
  Decoder <| \term ->
    case decoderA term of
      Err errA -> Err errA
      Ok decodedTermA ->
        case decoderB term of
          Err errB -> Err errB
          Ok decodedTermB ->
            case decoderC term of
              Err errC -> Err errC
              Ok decodedTermC ->
                case decoderD term of
                  Err errD ->
                    Err errD
                  Ok decodedTermD ->
                    Ok (func decodedTermA decodedTermB decodedTermC decodedTermD)

{-| Transform five decoders and then combine the result. This is useful to
decode an Erlang map into an Elm `Record`.
-}
map5 :
  (a -> b -> c -> d -> e -> f)
  -> Decoder a
  -> Decoder b
  -> Decoder c
  -> Decoder d
  -> Decoder e
  -> Decoder f
map5
  func
  (Decoder decoderA)
  (Decoder decoderB)
  (Decoder decoderC)
  (Decoder decoderD)
  (Decoder decoderE) =
  Decoder <| \term ->
    case decoderA term of
      Err errA -> Err errA
      Ok decodedTermA ->
        case decoderB term of
          Err errB -> Err errB
          Ok decodedTermB ->
            case decoderC term of
              Err errC -> Err errC
              Ok decodedTermC ->
                case decoderD term of
                  Err errD ->
                    Err errD
                  Ok decodedTermD ->
                    case decoderE term of
                      Err errE ->
                        Err errE
                      Ok decodedTermE ->
                        Ok (
                          func
                            decodedTermA
                            decodedTermB
                            decodedTermC
                            decodedTermD
                            decodedTermE
                        )

{-| Transform six decoders and then combine the result. This is useful to
decode an Erlang map into an Elm `Record`.
-}
map6 :
  (a -> b -> c -> d -> e -> f -> g)
  -> Decoder a
  -> Decoder b
  -> Decoder c
  -> Decoder d
  -> Decoder e
  -> Decoder f
  -> Decoder g
map6
  func
  (Decoder decoderA)
  (Decoder decoderB)
  (Decoder decoderC)
  (Decoder decoderD)
  (Decoder decoderE)
  (Decoder decoderF) =
  Decoder <| \term ->
    case decoderA term of
      Err errA -> Err errA
      Ok decodedTermA ->
        case decoderB term of
          Err errB -> Err errB
          Ok decodedTermB ->
            case decoderC term of
              Err errC -> Err errC
              Ok decodedTermC ->
                case decoderD term of
                  Err errD ->
                    Err errD
                  Ok decodedTermD ->
                    case decoderE term of
                      Err errE ->
                        Err errE
                      Ok decodedTermE ->
                        case decoderF term of
                          Err errF ->
                            Err errF
                          Ok decodedTermF ->
                            Ok (
                              func
                                decodedTermA
                                decodedTermB
                                decodedTermC
                                decodedTermD
                                decodedTermE
                                decodedTermF
                            )

{-| Transform seven decoders and then combine the result. This is useful to
decode an Erlang map into an Elm `Record`.
-}
map7 :
  (a -> b -> c -> d -> e -> f -> g -> h)
  -> Decoder a
  -> Decoder b
  -> Decoder c
  -> Decoder d
  -> Decoder e
  -> Decoder f
  -> Decoder g
  -> Decoder h
map7
  func
  (Decoder decoderA)
  (Decoder decoderB)
  (Decoder decoderC)
  (Decoder decoderD)
  (Decoder decoderE)
  (Decoder decoderF)
  (Decoder decoderH) =
  Decoder <| \term ->
    case decoderA term of
      Err errA -> Err errA
      Ok decodedTermA ->
        case decoderB term of
          Err errB -> Err errB
          Ok decodedTermB ->
            case decoderC term of
              Err errC -> Err errC
              Ok decodedTermC ->
                case decoderD term of
                  Err errD ->
                    Err errD
                  Ok decodedTermD ->
                    case decoderE term of
                      Err errE ->
                        Err errE
                      Ok decodedTermE ->
                        case decoderF term of
                          Err errF ->
                            Err errF
                          Ok decodedTermF ->
                            case decoderH term of
                              Err errH ->
                                Err errH
                              Ok decodedTermH ->
                                Ok (
                                  func
                                    decodedTermA
                                    decodedTermB
                                    decodedTermC
                                    decodedTermD
                                    decodedTermE
                                    decodedTermF
                                    decodedTermH
                                )

{-| Transform eight decoders and then combine the result. This is useful to
decode an Erlang map into an Elm `Record`.
-}
map8 :
  (a -> b -> c -> d -> e -> f -> g -> h -> i)
  -> Decoder a
  -> Decoder b
  -> Decoder c
  -> Decoder d
  -> Decoder e
  -> Decoder f
  -> Decoder g
  -> Decoder h
  -> Decoder i
map8
  func
  (Decoder decoderA)
  (Decoder decoderB)
  (Decoder decoderC)
  (Decoder decoderD)
  (Decoder decoderE)
  (Decoder decoderF)
  (Decoder decoderH)
  (Decoder decoderI) =
  Decoder <| \term ->
    case decoderA term of
      Err errA -> Err errA
      Ok decodedTermA ->
        case decoderB term of
          Err errB -> Err errB
          Ok decodedTermB ->
            case decoderC term of
              Err errC -> Err errC
              Ok decodedTermC ->
                case decoderD term of
                  Err errD ->
                    Err errD
                  Ok decodedTermD ->
                    case decoderE term of
                      Err errE ->
                        Err errE
                      Ok decodedTermE ->
                        case decoderF term of
                          Err errF ->
                            Err errF
                          Ok decodedTermF ->
                            case decoderH term of
                              Err errH ->
                                Err errH
                              Ok decodedTermH ->
                                case decoderI term of
                                  Err errI ->
                                    Err errI
                                  Ok decodedTermI ->
                                    Ok (
                                      func
                                        decodedTermA
                                        decodedTermB
                                        decodedTermC
                                        decodedTermD
                                        decodedTermE
                                        decodedTermF
                                        decodedTermH
                                        decodedTermI
                                    )

-- Private

bytesToString : Bytes.Bytes -> Maybe String
bytesToString bin =
  Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bin)) bin

foldResult : List (Result Error a) -> Result Error (List a)
foldResult = List.foldl doFoldResult  (Ok []) >> Result.map List.reverse

doFoldResult : Result Error a -> Result Error (List a) -> Result Error (List a)
doFoldResult result accumulatedResult =
  case accumulatedResult of
    Err _ -> accumulatedResult
    Ok accumulatedValues ->
      case result of
        Ok v -> Ok (v :: accumulatedValues)
        Err err -> Err err

fetchNestedFromMap : List String -> Decoder a -> Eetf.Term -> Result Error a
fetchNestedFromMap fields decoder term =
  case fields of
    [] ->
      decodeValue decoder term
    field_ :: rem ->
      case term of
        Map pairs ->
          case Dict.get field_ pairs of
            Just newTerm ->
              fetchNestedFromMap rem decoder newTerm
            Nothing ->
              Err
                (MapFieldNotFound
                  ("Field '" ++ field_ ++ "' not found in map.")
                )
        _ ->
          Err (WrongType "Not a map.")

findSuccessful : List (Result Error a) -> Result Error a
findSuccessful results =
  List.foldl
    keepOkOrError
    (Err
      (NoMatchingDecoder
        "No given decoder was able to properly decode the binary sequence."
      )
    )
    results

keepOkOrError : Result Error a -> Result Error a -> Result Error a
keepOkOrError result acc =
  case result of
    Ok val -> Ok val
    Err err -> acc
