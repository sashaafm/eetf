module Eetf.Encode exposing
  ( Value
  , array
  , bool
  , encode
  , dict
  , float
  , int
  , list
  , set
  , string
  , tuple
  , tuple2
  , tuple3
  )

{-| Turns Elm values into Erlang External Term Format (ETF) values. Check out
the official [documentation][docs] to better understand how this library and the
Erlang side work.

[docs]: http://erlang.org/doc/apps/erts/erl_ext_dist.html

# Encoding
@docs encode, Value

# Primitives
@docs string, int, float, bool

# Data Structures
@docs list, array, set, dict, tuple, tuple2, tuple3
-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Encode as E exposing (..)
import Dict exposing (Dict)
import Eetf
import Set exposing (Set)

{-| Represents an External Term Format value.
-}
type alias Value = Eetf.Term

{-| Convert a `Value` into a `Bytes` sequence.
-}
encode : Value -> Bytes
encode val = val |> valueToEncoder |> appendVersion |> E.encode

{-| Turn a `Bool` into an Erlang `boolean` in External Term Format.
-}
bool : Bool -> Value
bool b = if b then Eetf.Atom "true" else Eetf.Atom "false"

{-| Turn an `Int` into an Erlang `integer` in External Term Format.
-}
int : Int -> Value
int i = Eetf.Integer i

{-| Turn a `Float` into an Erlang `float` in External Term Format.
-}
float : Float -> Value
float f = Eetf.FloatingPoint f

{-| Turn a `String` into an Erlang `binary` (UTF-8 encoded byte sequence) in
External Term Format.
-}
string : String -> Value
string str = str |> E.string |> E.encode |> Eetf.Binary

{-| Turn a `List` into an Erlang `list` in External Term Format.
-}
list : (a -> Value) -> List a -> Value
list func list_ = Eetf.ProperList (List.map func list_)

{-| Turn an `Array` into an Erlang `list` in External Term Format.
-}
array : (a -> Value) -> Array a -> Value
array func array_ = list func (Array.toList array_)

{-| Turn a `Set` into an Erlang `list` in External Term Format.
-}
set : (a -> Value) -> Set a -> Value
set func set_ = list func (Set.toList set_)

{-| Turn a `Dict` into an Erlang `map` in External Term Format.
-}
dict : (k -> String) -> (v -> Value) -> Dict k v -> Value
dict keyFunc valueFunc dict_ =
  dict_
  |> Dict.toList
  |> List.map (\(k, v) -> Tuple.pair (keyFunc k) (valueFunc v))
  |> Dict.fromList
  |> Eetf.Map

{-| Turn a `Tuple` of arity 1 into an Erlang `tuple` of arity 1 in External Term
Format.
-}
tuple : (e -> Value) -> (e) -> Value
tuple func tuple_ = Eetf.Tuple [ func tuple_ ]

{-| Turn a `Tuple` of arity 2 into an Erlang `tuple` of arity 2 in External Term
Format.
-}
tuple2 : (e1 -> Value) -> (e2 -> Value) -> (e1, e2) -> Value
tuple2 func1 func2 (elem1, elem2) = Eetf.Tuple [ func1 elem1, func2 elem2 ]

{-| Turn a `Tuple` of arity 3 into an Erlang `tuple` of arity 3 in External Term
Format.
-}
tuple3 : (e1 -> Value) -> (e2 -> Value) -> (e3 -> Value) -> (e1, e2, e3) -> Value
tuple3 func1 func2 func3 (e1, e2, e3) =
  Eetf.Tuple [ func1 e1, func2 e2, func3 e3 ]

-- Private

valueToEncoder : Value -> Encoder
valueToEncoder val =
  case val of
    Eetf.Integer i ->
      if isRepresentableInUInt8 i then
        smallIntegerExt i
      else if isRepresentableInInt32 i then
        integerExt i
      else if isRepresentableInSmallBig i then
        -- Elm does not deal with such large integers since it's bound to
        -- Javascript integers. This is kept for completeness of the
        -- implementation. In reality this code can never be executed.
        smallBigExt i
      else
        largeBigExt i
    Eetf.FloatingPoint f ->
      newFloatExt f
    Eetf.Atom atom ->
      let len = getStringWidth atom in
      if len < 256 then
        smallAtomUtf8Ext len atom
      else
        atomUtf8Ext len atom
    Eetf.Binary bin ->
      binaryExt bin
    Eetf.ProperList list_ ->
      listExt list_
    Eetf.ImproperList elements tail ->
      listExt (elements ++ [tail])
    Eetf.Nil ->
      nilExt
    Eetf.Map pairs ->
      mapExt pairs
    Eetf.Tuple elements ->
      let arity = List.length elements in
      if arity < 256 then
        smallTupleExt arity elements
      else
        largeTupleExt arity elements
    Eetf.Pid properties ->
      pidExt properties
    Eetf.Port properties ->
      portExt properties
    Eetf.Reference properties ->
      referenceExt properties
    Eetf.NewReference properties ->
      newReferenceExt properties
    Eetf.Export mfa ->
      exportExt mfa

-- Encoders

appendVersion : Encoder -> Encoder
appendVersion encoder = sequence (version :: [encoder])

version : Encoder
version = unsignedInt8 131

-- [ 88 :: 1B, Node :: N, ID :: 4B, Serial :: 4B, Creation :: 4B ]
newPidExt : Eetf.PidProperties -> Encoder
newPidExt {node, id, serial, creation} =
  sequence
    [ newPidExtTag
    , valueToEncoder (string node)
    , unsignedInt32 Bytes.BE id
    , unsignedInt32 Bytes.BE serial
    , unsignedInt32 Bytes.BE creation
    ]

-- [ 89 :: 1B, Node :: N, ID :: 4B, Creation :: 4B ]
newPortExt : Eetf.PortProperties -> Encoder
newPortExt {node, id, creation} =
  sequence
    [ newPortExtTag
    , valueToEncoder (string node)
    , unsignedInt32 Bytes.BE id
    , unsignedInt32 Bytes.BE creation
    ]

-- [ 97 :: 1B, Int :: 1B ]
smallIntegerExt : Int -> Encoder
smallIntegerExt i = sequence [ smallIntegerExtTag, unsignedInt8 i ]

-- [ 98 :: 1B, Int :: 4B ]
integerExt : Int -> Encoder
integerExt i = sequence [ integerExtTag, signedInt32 Bytes.BE i ]

-- [ 70 :: 1B, Float :: 8B ]
newFloatExt : Float -> Encoder
newFloatExt f = sequence [ newFloatExtTag, float64 Bytes.BE f ]

-- [ 101 :: 1B, Node : N, ID :: 4B, Creation :: 1B ]
referenceExt : Eetf.ReferenceProperties -> Encoder
referenceExt {node, id, creation} =
  sequence
    [ referenceExtTag
    , valueToEncoder (string node)
    , unsignedInt32 Bytes.BE id
    , unsignedInt8 creation
    ]

-- [ 102 :: 1B, Node :: N, ID :: 4B, Creation :: 1B ]
portExt : Eetf.PortProperties -> Encoder
portExt {node, id, creation} =
  sequence
    [ portExtTag
    , valueToEncoder (string node)
    , unsignedInt32 Bytes.BE id
    , unsignedInt8 creation
    ]

-- [ 103 :: 1B, Node :: N, ID :: 4B, Serial :: 4B, Creation :: 1B ]
pidExt : Eetf.PidProperties -> Encoder
pidExt {node, id, serial, creation} =
  sequence
    [ pidExtTag
    , valueToEncoder (string node)
    , unsignedInt32 Bytes.BE id
    , unsignedInt32 Bytes.BE serial
    , unsignedInt8 creation
    ]

-- [ 104 :: 1B, Arity :: 1B, Elements :: Arity ]
smallTupleExt : Int -> List Value -> Encoder
smallTupleExt arity elements =
  sequence
    [ smallTupleExtTag
    , unsignedInt8 arity
    , sequence (List.map valueToEncoder elements)
    ]

-- [ 105 :: 1B, Arity :: 4B, Elements :: Arity ]
largeTupleExt : Int -> List Value -> Encoder
largeTupleExt arity elements =
  sequence
    [ largeTupleExtTag
    , unsignedInt32 Bytes.BE arity
    , sequence (List.map valueToEncoder elements)
    ]

-- [ 106 :: 1B ]
nilExt : Encoder
nilExt = nilExtTag

-- [ 108 :: 1B, Len :: 4B, Elements, Tail ]
listExt : List Value -> Encoder
listExt list_ =
  sequence
    [ listExtTag
    , unsignedInt32 Bytes.BE (List.length list_)
    , sequence (List.map valueToEncoder list_)
    , nilExt
    ]

-- [ 109 :: 1B, Len :: 4B, Data :: Len ]
binaryExt : Bytes -> Encoder
binaryExt bin =
  sequence
    [ binaryExtTag
    , unsignedInt32 Bytes.BE (Bytes.width bin)
    , bytes bin
    ]

-- [ 110 :: 1B, N :: 1B, Sign :: 1B, N :: Integer ]
smallBigExt : Int -> Encoder
smallBigExt i =
  let
      isNegative = i < 0
      originalInt = if isNegative then (negate i) - 1 else i
      sequence_ = buildIntegerSequence isNegative [] originalInt
      bytesSequence = sequence_ |> List.map unsignedInt8 |> sequence
  in
  sequence
    [ smallBigExtTag
    , unsignedInt8 (List.length sequence_)
    , bigExtSign i
    , bytesSequence
    ]

-- [ 111 :: 1B, N :: 4B, Sign :: 1B, N :: Integer ]
largeBigExt : Int -> Encoder
largeBigExt i =
  let
      isNegative = i < 0
      originalInt = if isNegative then (negate i) - 1 else i
      sequence_ = buildIntegerSequence isNegative [] originalInt
      bytesSequence = sequence_ |> List.map unsignedInt8 |> sequence
  in
  sequence
    [ largeBigExtTag
    , unsignedInt32 Bytes.BE (List.length sequence_)
    , bigExtSign i
    , bytesSequence
    ]

-- [ 113 :: 1B, Module :: N1, Function :: N2, Arity :: N3 ]
exportExt : Eetf.Mfa -> Encoder
exportExt {module_, function, arity} =
  sequence
    [ exportExtTag
    , valueToEncoder (string module_)
    , valueToEncoder (string function)
    , smallIntegerExt arity
    ]

-- [ 114 :: 1B, Len :: 2B, Node :: N, Creation :: 1B, ID :: N ]
newReferenceExt : Eetf.NewReferenceProperties -> Encoder
newReferenceExt {node, creation, ids} =
  let
      idsSequence =
        ids
        |> buildIdsSequence []
        |> List.map (unsignedInt32 Bytes.BE)
        |> sequence
  in
  sequence
    [ newReferenceExtTag
    , unsignedInt16 Bytes.BE (List.length ids)
    , valueToEncoder (string node)
    , unsignedInt8 creation
    , idsSequence
    ]

-- [ 116 : 1B, Arity :: 4B, Pairs :: Arity ]
mapExt : Dict String Value -> Encoder
mapExt dict_ =
  let
      pairs =
        dict_
        |> Dict.toList
        |> List.map pairEncoder
        |> List.concat
  in
  sequence
    [ mapExtTag
    , unsignedInt32 Bytes.BE (Dict.size dict_)
    , sequence pairs
    ]

pairEncoder : (String, Value) -> List Encoder
pairEncoder (key, value) = [ valueToEncoder (string key), valueToEncoder value ]

-- [ 118 :: 1B, Len :: 2B, AtomName :: Len ]
atomUtf8Ext : Int -> String -> Encoder
atomUtf8Ext len atom =
  sequence
    [ atomUtf8ExtTag
    , unsignedInt16 Bytes.BE len
    , E.string atom
    ]

-- [ 119 :: 1B, Len :: 1B, AtomName :: Len ]
smallAtomUtf8Ext : Int -> String -> Encoder
smallAtomUtf8Ext len atom =
  sequence
    [ smallAtomUtf8ExtTag
    , unsignedInt8 len
    , E.string atom
    ]


-- Tags

newPidExtTag : Encoder
newPidExtTag = unsignedInt8 88

newPortExtTag : Encoder
newPortExtTag = unsignedInt8 89

smallIntegerExtTag : Encoder
smallIntegerExtTag = unsignedInt8 97

integerExtTag : Encoder
integerExtTag = unsignedInt8 98

newFloatExtTag : Encoder
newFloatExtTag = unsignedInt8 70

referenceExtTag : Encoder
referenceExtTag = unsignedInt8 101

portExtTag : Encoder
portExtTag = unsignedInt8 102

pidExtTag : Encoder
pidExtTag = unsignedInt8 103

smallTupleExtTag : Encoder
smallTupleExtTag = unsignedInt8 104

largeTupleExtTag : Encoder
largeTupleExtTag = unsignedInt8 105

nilExtTag : Encoder
nilExtTag = unsignedInt8 106

listExtTag : Encoder
listExtTag = unsignedInt8 108

binaryExtTag : Encoder
binaryExtTag = unsignedInt8 109

smallBigExtTag : Encoder
smallBigExtTag = unsignedInt8 110

largeBigExtTag : Encoder
largeBigExtTag = unsignedInt8 111

exportExtTag : Encoder
exportExtTag = unsignedInt8 113

newReferenceExtTag : Encoder
newReferenceExtTag = unsignedInt8 114

mapExtTag : Encoder
mapExtTag = unsignedInt8 116

atomUtf8ExtTag : Encoder
atomUtf8ExtTag = unsignedInt8 118

smallAtomUtf8ExtTag : Encoder
smallAtomUtf8ExtTag = unsignedInt8 119

-- Helpers

uInt8Max : Int
uInt8Max = 255

uInt8Min : Int
uInt8Min = 0

int32Max : Int
int32Max = 2147483647

int32Min : Int
int32Min = -2147483648

smallBigMin : Int
smallBigMin = negate smallBigMax

smallBigMax : Int
smallBigMax =
  126238304966058622268417487065116999845484776053576109500509161826268184136202698801551568013761380717534054534851164138648904527931605160527688095259563605939964364716019515983399209962459578542172100149937763938581219604072733422507180056009672540900709554109516816573779593326332288314873251559077853068444977864803391962580800682760017849589281937637993445539366428356761821065267423102149447628375691862210717202025241630303118559188678304314076943801692528246980959705901641444238894928620825482303431806955690226308773426829503900930529395181208739591967195841536053143145775307050594328881077553168201547775

isRepresentableInUInt8 : Int -> Bool
isRepresentableInUInt8 i = i >= uInt8Min && i <= uInt8Max

isRepresentableInInt32 : Int -> Bool
isRepresentableInInt32 i = i >= int32Min && i <= int32Max

isRepresentableInSmallBig : Int -> Bool
isRepresentableInSmallBig i = i >= smallBigMin && i <= smallBigMax

bigExtSign : Int -> Encoder
bigExtSign i = if i >= 0 then unsignedInt8 0 else unsignedInt8 1

buildIdsSequence : List Int -> List Int -> List Int
buildIdsSequence seq ids =
  case ids of
    [] -> List.reverse seq
    id :: rem -> buildIdsSequence (id :: seq) rem

buildIntegerSequence : Bool -> List Int -> Int -> List Int
buildIntegerSequence isNegative seq i =
  if i /= 0 then
    let
        rem = if isNegative then 255 - (modBy 256 i) else (modBy 256 i)
        newSeq = rem :: seq
        newI = floor (toFloat i / 256)
    in
    buildIntegerSequence isNegative newSeq newI
  else
    List.reverse seq
