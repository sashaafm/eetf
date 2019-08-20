module Eetf exposing
  ( Term
  , decode
  )

import Bytes
import Bytes.Decode as D

type Term
  = Integer Int
  | FloatingPoint Float
  | Atom String
  | Pid (Id, Serial, Creation)
  | Tuple Elements
  | Map Pairs
  | Nil
  | CharList String -- String represented by list of bytes (ints in 0-255 range)
  | ProperList Elements -- TODO: FINISH
  -- | ImproperList Elements Tail -- TODO: Implement
  | Binary Bytes.Bytes

type Version = Version Int
type Sign = Positive | Negative
type alias Id = Int
type alias Serial = Int
type alias Creation = Int
type alias Node = String
type alias Arity = Int
type alias Tail = Term
type alias Elements = List Term
type alias Pairs = List (Term, Term)
type Tag
  = SmallIntegerExt Int
  | IntegerExt Int
  | NewFloatExt Float
  | AtomExt String -- (DEPRECATED) Needs latin-1 string decoder instead of UTF-8
  | SmallAtomExt String -- (DEPRECATED) Needs latin-1 string decoder instead of UTF-8
  | AtomUtf8Ext String
  | SmallAtomUtf8Ext String
  | PidExt Node Id Serial Creation
  | NewPidExt Node Id Serial Creation
  | SmallTupleExt Elements -- Unlike Erlang it's hard or impossible to have a generic tuple type
  | LargeTupleExt Elements
  | MapExt Pairs
  | NilExt
  | StringExt String
  | ListExt Elements -- Unlike Erlang lists cannot have mixed types
  | BinaryExt Bytes.Bytes
  | SmallBigExt Int
  | LargeBigExt Int

decode : D.Decoder Term
decode = version |> D.andThen (\_ -> tag) |> D.andThen extractTerm

version : D.Decoder Version
version = D.map Version D.unsignedInt8

tag : D.Decoder Tag
tag = D.unsignedInt8 |> D.andThen pickTag

pickTag : Int -> D.Decoder Tag
pickTag tag_ =
  case tag_ of
    97 -> D.map SmallIntegerExt D.unsignedInt8
    98 -> D.map IntegerExt (D.signedInt32 Bytes.BE)
    70 -> D.map NewFloatExt (D.float64 Bytes.BE)
    100 -> D.map AtomExt atomUtf8Ext
    104 -> D.map SmallTupleExt (tupleExt D.unsignedInt8)
    105 -> D.map LargeTupleExt (tupleExt (D.unsignedInt32 Bytes.BE))
    106 -> D.succeed NilExt
    107 -> D.map StringExt stringExt
    108 -> D.map ListExt listExt
    109 -> D.map BinaryExt binaryExt
    110 -> D.map SmallBigExt (bigExt D.unsignedInt8)
    111 -> D.map LargeBigExt (bigExt (D.unsignedInt32 Bytes.BE))
    115 -> D.map SmallAtomExt smallAtomUtf8Ext
    116 -> D.map MapExt mapExt
    118 -> D.map AtomUtf8Ext atomUtf8Ext
    119 -> D.map SmallAtomUtf8Ext smallAtomUtf8Ext
    _ -> D.fail

extractTerm : Tag -> D.Decoder Term
extractTerm tag_ =
  case tag_ of
    SmallIntegerExt integer -> D.succeed (Integer integer)
    IntegerExt integer -> D.succeed (Integer integer)
    NewFloatExt float -> D.succeed (FloatingPoint float)
    AtomExt atom -> D.succeed (Atom atom)
    SmallTupleExt elements -> D.succeed (Tuple elements)
    LargeTupleExt elements -> D.succeed (Tuple elements)
    MapExt pairs -> D.succeed (Map pairs)
    NilExt -> D.succeed Nil
    StringExt string -> D.succeed (CharList string)
    ListExt elements -> D.succeed (ProperList elements)
    BinaryExt binary -> D.succeed (Binary binary)
    SmallBigExt bigInteger -> D.succeed (Integer bigInteger)
    LargeBigExt bigInteger -> D.succeed (Integer bigInteger)
    SmallAtomExt atom -> D.succeed (Atom atom)
    AtomUtf8Ext atom -> D.succeed (Atom atom)
    SmallAtomUtf8Ext atom -> D.succeed (Atom atom)
    PidExt _ id serial creation -> D.succeed (Pid (id, serial, creation))
    NewPidExt _ id serial creation -> D.succeed (Pid (id, serial, creation))

stringExt : D.Decoder String
stringExt = D.unsignedInt16 (Bytes.BE) |> D.andThen D.string

binaryExt : D.Decoder Bytes.Bytes
binaryExt = D.unsignedInt32 (Bytes.BE) |> D.andThen D.bytes

listExt : D.Decoder Elements
listExt =
  Bytes.BE
  |> D.unsignedInt32
  |> D.andThen (\len -> D.loop (len, []) (listStep decode))

listStep :
  D.Decoder Term
  -> (Int, Elements)
  -> D.Decoder (D.Step (Int, Elements) (Elements))
listStep decoder (len, list) =
  let _ = Debug.log "LENGTH OF LIST" len in
  if len <= 0 then
    let _ = Debug.log "GOING TO FINISH" True in
    D.succeed (D.Done list)
  else
    let _ = Debug.log "GOING TO CONTINUE" True in
    D.map (\elem ->
      let _ = Debug.log "ELEMENT" elem in
      D.Loop (len - 1, elem :: list)
    ) decoder

tupleExt : D.Decoder Int -> D.Decoder Elements
tupleExt lengthDecoder =
  lengthDecoder
  |> D.andThen (\len -> D.loop (len, []) (listStep decode))

mapExt : D.Decoder Pairs
mapExt =
  Bytes.BE
  |> D.unsignedInt32
  |> D.andThen (\arity -> D.loop (arity, []) (mapStep decode))

mapStep :
  D.Decoder Term
  -> (Int, Pairs)
  -> D.Decoder (D.Step (Int, Pairs) (Pairs))
mapStep decoder (arity, pairs) =
  if arity <= 0 then
    D.succeed (D.Done pairs)
  else
    D.map2
      (\key value -> D.Loop (arity - 1, (key, value) :: pairs))
      decoder
      decoder

bigExt : D.Decoder Int -> D.Decoder Int
bigExt lengthDecoder =
  lengthDecoder -- Docs don't mention size if is signed or unsigned int
  |> D.andThen (\len ->
    D.signedInt8
    |> D.andThen (\sign ->
        D.loop ((len, (toSign sign), 0), 0) (bigStep D.unsignedInt8)
    )
  )

bigStep :
  D.Decoder Int
  -> ((Int, Sign, Int), Int)
  -> D.Decoder (D.Step ((Int, Sign, Int), Int) Int)
bigStep decoder ((len, sign, currentIteration), result) =
  if currentIteration == (len) then
    D.succeed (D.Done (negateBasedOnSign sign result))
  else
    D.map (\digit ->
      let newResult = digit * 256^currentIteration + result in
      D.Loop ((len, sign, currentIteration + 1), newResult)
    )
    decoder

negateBasedOnSign : Sign -> Int -> Int
negateBasedOnSign sign integer =
  case sign of
    Positive -> integer
    Negative -> negate integer

toSign : Int -> Sign
toSign int = if int == 0 then Positive else Negative

atomUtf8Ext : D.Decoder String
atomUtf8Ext = D.unsignedInt16 (Bytes.BE) |> D.andThen extractAtom

smallAtomUtf8Ext : D.Decoder String
smallAtomUtf8Ext = D.unsignedInt8 |> D.andThen extractAtom

extractAtom : Int -> D.Decoder String
extractAtom len = D.string len

