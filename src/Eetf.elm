module Eetf exposing
  ( Term(..)
  , Mfa
  , Pairs
  , PidProperties
  , PortProperties
  , ReferenceProperties
  , NewReferenceProperties
  , decode
  , fromBytes
  )

import Bytes
import Bytes.Decode exposing (..)
import Dict exposing (Dict)

type Term
  = Integer Int
  | FloatingPoint Float
  | Atom String
  | Pid PidProperties
  | Port PortProperties
  | Reference ReferenceProperties
  | NewReference NewReferenceProperties
  | Tuple Elements
  | Map Pairs
  | Nil
  | ProperList Elements
  | ImproperList Elements Tail
  | Binary Bytes.Bytes
  | Export Mfa

type Version = Version Int
type Sign = Positive | Negative
type alias Id = Int
type alias Serial = Int
type alias Creation = Int
type alias Node = String
type alias Tail = Term
type alias Elements = List Term
type alias Module = String
type alias Function = String
type alias Arity = Int
type alias Index = Int
type alias OldIndex = Index
type alias Uniq = Bytes.Bytes
type alias OldUniq = Int
type alias FreeVars = List Term
type alias Pairs = Dict String Term
type alias Mfa =
  { module_ : Module
  , function : Function
  , arity : Arity
  }
type alias ReferenceProperties =
  { node:  Node
  , id : Id
  , creation : Creation
  }
type alias NewReferenceProperties =
  { node:  Node
  , creation : Creation
  , ids : List Id
  }
type alias PortProperties =
  { node:  Node
  , id : Id
  , creation : Creation
  }
type alias PidProperties =
  { node : Node
  , id : Id
  , serial : Serial
  , creation : Creation
  }
type alias NewFunProperties =
  { module_ : Module
  , arity : Arity
  , pid : PidProperties
  , freeVars : FreeVars
  , index : Index
  , uniq : Uniq
  , oldIndex : OldIndex
  , oldUniq : OldUniq
  }
type Tag
  = SmallIntegerExt Int -- DONE
  | IntegerExt Int -- DONE
  | FloatExt Float -- DONE
  | PortExt PortProperties -- DONE
  | NewPortExt PortProperties -- DONE
  | PidExt PidProperties -- DONE
  | NewPidExt PidProperties -- DONE
  | SmallTupleExt Elements -- DONE (Unlike Erlang it's hard or impossible to have a generic tuple type)
  | LargeTupleExt Elements -- DONE
  | MapExt Pairs -- DONE
  | NilExt -- DONE
  | StringExt Elements -- DONE
  | ListExt Elements -- DONE (Unlike Erlang lists cannot have mixed types)
  | BinaryExt Bytes.Bytes -- DONE
  | SmallBigExt Int -- DONE
  | LargeBigExt Int -- DONE
  | ReferenceExt ReferenceProperties -- DONE
  | NewReferenceExt NewReferenceProperties -- DONE
  | NewerReferenceExt NewReferenceProperties -- DONE
  | FunExt Creation  Module Index Uniq FreeVars -- TODO: Implement
  | NewFunExt NewFunProperties -- TODO: Implement
  | ExportExt Mfa -- DONE
  | BitBinaryExt Bytes.Bytes -- DONE
  | NewFloatExt Float -- DONE
  | AtomUtf8Ext String -- DONE
  | SmallAtomUtf8Ext String -- DONE
  | AtomExt String -- DONE
  | SmallAtomExt String -- DONE

fromBytes : Bytes.Bytes -> Result String Term
fromBytes bin =
  case Bytes.Decode.decode decode bin of
    Just val -> Ok val
    Nothing -> Err "Invalid Erlang term."

decode : Decoder Term
decode = version |> andThen (\_ -> tag) |> andThen extractTerm

version : Decoder Version
version = map Version unsignedInt8

tag : Decoder Tag
tag = unsignedInt8 |> andThen pickTag

pickTag : Int -> Decoder Tag
pickTag tag_ =
  let _ = Debug.log "TAG" tag_ in
  case tag_ of
    77 -> map BitBinaryExt bitBinaryExt
    88 -> map NewPidExt newPidExt
    89 -> map NewPortExt newPortExt
    90 -> map NewerReferenceExt newerReferenceExt
    97 -> map SmallIntegerExt smallIntegerExt
    98 -> map IntegerExt integerExt
    70 -> map NewFloatExt newFloatExt
    100 -> map AtomExt atomExt
    101 -> map ReferenceExt referenceExt
    102 -> map PortExt portExt
    103 -> map PidExt pidExt
    104 -> map SmallTupleExt smallTupleExt
    105 -> map LargeTupleExt largeTupleExt
    106 -> succeed NilExt
    107 -> map StringExt stringExt
    108 -> map ListExt listExt
    109 -> map BinaryExt binaryExt
    110 -> map SmallBigExt smallBigExt
    111 -> map LargeBigExt largeBigExt
    112 -> map NewFunExt newFunExt
    113 -> map ExportExt exportExt
    114 -> map NewReferenceExt newReferenceExt
    115 -> map SmallAtomExt smallAtomExt
    116 -> map MapExt mapExt
    118 -> map AtomUtf8Ext atomUtf8Ext
    119 -> map SmallAtomUtf8Ext smallAtomUtf8Ext
    _ -> fail

extractTerm : Tag -> Decoder Term
extractTerm tag_ =
  let _ = Debug.log "DECODED TAG" tag_ in
  case tag_ of
    SmallIntegerExt integer -> succeed (Integer integer)
    IntegerExt integer -> succeed (Integer integer)
    PortExt portProperties -> succeed (Port portProperties)
    NewPortExt portProperties -> succeed (Port portProperties)
    PidExt pidProperties -> succeed (Pid pidProperties)
    NewPidExt pidProperties -> succeed (Pid pidProperties)
    SmallTupleExt elements -> succeed (Tuple elements)
    LargeTupleExt elements -> succeed (Tuple elements)
    MapExt pairs -> succeed (Map pairs)
    NilExt -> succeed Nil
    StringExt integers -> succeed (ProperList integers)
    ListExt elements -> intoListTerm elements
    BinaryExt binary -> succeed (Binary binary)
    SmallBigExt bigInteger -> succeed (Integer bigInteger)
    LargeBigExt bigInteger -> succeed (Integer bigInteger)
    NewReferenceExt properties -> succeed (NewReference properties)
    NewerReferenceExt properties -> succeed (NewReference properties)
    ExportExt mfa -> succeed (Export mfa)
    BitBinaryExt bitstring -> succeed (Binary bitstring)
    NewFloatExt floatingPoint -> succeed (FloatingPoint floatingPoint)
    AtomUtf8Ext atom -> succeed (Atom atom)
    SmallAtomUtf8Ext atom -> succeed (Atom atom)
    AtomExt atom -> succeed (Atom atom)
    SmallAtomExt atom -> succeed (Atom atom)
    _ -> fail

extractString : Tag -> Decoder String
extractString tag_ =
  case tag_ of
    AtomUtf8Ext atom -> succeed atom
    SmallAtomUtf8Ext atom -> succeed atom
    AtomExt atom -> succeed atom
    SmallAtomExt atom -> succeed atom
    BinaryExt bin ->
      case Bytes.Decode.decode (string (Bytes.width bin)) bin of
        Just str -> succeed str
        Nothing -> fail
    _ -> fail


extractAtom : Tag -> Decoder String
extractAtom tag_ =
  case tag_ of
    AtomUtf8Ext atom -> succeed atom
    SmallAtomUtf8Ext atom -> succeed atom
    AtomExt atom -> succeed atom
    SmallAtomExt atom -> succeed atom
    _ -> fail

extractInt : Tag -> Decoder Int
extractInt tag_ =
  case tag_ of
    SmallIntegerExt int_ -> succeed int_
    IntegerExt int_ -> succeed int_
    _ -> fail

intoListTerm : Elements -> Decoder Term
intoListTerm elements =
  case last elements of
    Just t ->
      case t of
        Nil ->
          let
              len = List.length elements
              finalList = List.take (len - 1) elements
          in
          succeed (ProperList finalList)
        _ ->
          succeed (ImproperList elements t)
    Nothing ->
      succeed (ProperList elements)

smallIntegerExt : Decoder Int
smallIntegerExt = unsignedInt8

integerExt : Decoder Int
integerExt = signedInt32 Bytes.BE

newFloatExt : Decoder Float
newFloatExt = float64 Bytes.BE

stringExt : Decoder Elements
stringExt =
  Bytes.BE
  |> unsignedInt16
  |> andThen (\len -> loop (len, []) (stringStep ()))

type alias StringStepState = (Int, Elements)

stringStep : () -> StringStepState -> Decoder (Step StringStepState Elements)
stringStep _ (len, list) =
  if len == 0 then
    succeed (Done (List.reverse list))
  else
    map
      (\elem -> Loop (len - 1, (Integer elem) :: list))
      (unsignedInt8)

binaryExt : Decoder Bytes.Bytes
binaryExt =
  let
      len = unsignedInt32 Bytes.BE
      data = \len_ -> bytes len_
  in
  len |> andThen data

bitBinaryExt : Decoder Bytes.Bytes
bitBinaryExt =
  let
      len = unsignedInt32 Bytes.BE
      trailingBits = unsignedInt8
      data = \len_ -> bytes len_
  in
  len |> andThen (\len_ -> trailingBits |> andThen (\_ -> (data len_)))

node : Decoder Node
node = tag |> andThen extractAtom

id : Decoder Id
id = unsignedInt32 Bytes.BE

serial : Decoder Serial
serial = unsignedInt32 Bytes.BE

module_ : Decoder Module
module_ = tag |> andThen extractAtom

function : Decoder Function
function = tag |> andThen extractAtom

arity : Decoder Arity
arity = unsignedInt8

functionArity : Decoder Arity
functionArity = tag |> andThen extractInt

index : Decoder Index
index = unsignedInt32 (Bytes.BE)

oldIndex : Decoder OldIndex
oldIndex = tag |> andThen extractInt

uniq : Decoder Uniq
uniq = bytes 16

oldUniq : Decoder OldUniq
oldUniq = tag |> andThen extractInt

numFree : Decoder Int
numFree = unsignedInt32 (Bytes.BE)

smallCreation : Decoder Creation
smallCreation = unsignedInt8

bigCreation : Decoder Creation
bigCreation = unsignedInt32 Bytes.BE

pidExt : Decoder PidProperties
pidExt = map4 PidProperties node id serial smallCreation

newPidExt : Decoder PidProperties
newPidExt = map4 PidProperties node id serial bigCreation

portExt : Decoder PortProperties
portExt = map3 PortProperties node id smallCreation

newPortExt : Decoder PortProperties
newPortExt = map3 PortProperties node id bigCreation

referenceExt : Decoder ReferenceProperties
referenceExt = map3 ReferenceProperties node id smallCreation

newReferenceExt : Decoder NewReferenceProperties
newReferenceExt =
  let len = unsignedInt16 Bytes.BE in
  len |> andThen (\len_ ->
    map3 NewReferenceProperties node smallCreation (ids len_)
  )

newerReferenceExt : Decoder NewReferenceProperties
newerReferenceExt =
  let len = unsignedInt16 Bytes.BE in
  len |> andThen (\len_ ->
    map3 NewReferenceProperties node bigCreation (ids len_)
  )

ids : Int -> Decoder (List Id)
ids len = loop (len, []) (idStep id)

type alias IdsStepState = (Int, List Id)
type alias IdsStepDecoder =
  (IdsStepState -> Decoder (Step IdsStepState (List Id)))

idStep : Decoder Id -> IdsStepDecoder
idStep decoder (len_, ids_) =
  if len_ <= 0 then
    succeed (Done (List.reverse ids_))
  else
    map (\id_ -> Loop (len_ - 1, id_ :: ids_)) decoder

newFunExt : Decoder NewFunProperties
newFunExt =
  let size = unsignedInt32 (Bytes.BE) in
  size |> andThen (\size_ ->
    let _ = Debug.log "SIZE" size_ in
    arity |> andThen (\arity_ ->
      let _ = Debug.log "ARITY" arity_ in
      uniq |> andThen (\uniq_ ->
        let _ = Debug.log "UNIQ" uniq_ in
        index |> andThen (\index_ ->
          let _ = Debug.log "INDEX" index_ in
          numFree |> andThen (\numFree_ ->
            let _ = Debug.log "NUM FREE" numFree_ in
            module_ |> andThen (\mod_ ->
              let _ = Debug.log "MODULE" mod_ in
              oldIndex |> andThen (\oldIndex_ ->
                let _ = Debug.log "OLD INDEX" oldIndex_ in
                oldUniq |> andThen (\oldUniq_ ->
                  let _ = Debug.log "OLD UNIQ" oldUniq_ in
                  pidExt |> andThen (\pid_ ->
                    let _ = Debug.log "PID" pid_ in
                    freeVars (numFree_) |> andThen (\freeVars_ ->
                      let _ = Debug.log "FREE VARS" freeVars_ in
                      succeed (NewFunProperties
                        mod_
                        arity_
                        pid_
                        freeVars_
                        index_
                        uniq_
                        oldIndex_
                        oldUniq_
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

type alias FreeVarsStepState = (Int, FreeVars)
type alias FreeVarsStepDecoder =
  (FreeVarsStepState -> Decoder (Step FreeVarsStepState FreeVars))

freeVars : Int -> Decoder FreeVars
freeVars count = loop (count, []) (freeVarsStep decode)

freeVarsStep : Decoder Term -> FreeVarsStepDecoder
freeVarsStep decoder (count, list) =
  let _ = Debug.log "STEP" True in
  if count <= 0 then
    succeed (Done (List.reverse list))
  else
    map (\freeVar -> Loop (count - 1, freeVar :: list)) decoder

exportExt : Decoder Mfa
exportExt = map3 Mfa module_ function functionArity

listExt : Decoder Elements
listExt =
  Bytes.BE
  |> unsignedInt32
  |> andThen (\len -> loop (len, []) (listStep decode))

type alias ListStepState = (Int, Elements)

listStep :
  Decoder Term -> ListStepState -> Decoder (Step ListStepState (Elements))
listStep decoder (len, list) =
  if len < 0 then
    succeed (Done (List.reverse list))
  else if len == 0 then
    map
      (\tail -> Loop (len - 1, tail :: list))
      (tag |> andThen (\tag_ -> extractTerm tag_))
  else
    map
      (\elem -> Loop (len - 1, elem :: list))
      (tag |> andThen (\tag_ -> extractTerm tag_))

smallTupleExt : Decoder Elements
smallTupleExt =
  unsignedInt8 |> andThen (\len -> loop (len, []) (tupleStep decode))

largeTupleExt : Decoder Elements
largeTupleExt =
  Bytes.BE
  |> unsignedInt32
  |> andThen (\len -> loop (len, []) (tupleStep decode))

type alias TupleStepState = (Int, Elements)

tupleStep :
  Decoder Term -> TupleStepState -> Decoder (Step TupleStepState (Elements))
tupleStep decoder (len, list) =
  if len == 0 then
    succeed (Done (List.reverse list))
  else
    map
      (\elem -> Loop (len - 1, elem :: list))
      (tag |> andThen (\tag_ -> extractTerm tag_))

mapExt : Decoder Pairs
mapExt =
  Bytes.BE
  |> unsignedInt32
  |> andThen (\arity_ -> loop (arity_, Dict.empty) (mapStep decode))

type alias MapStepState = (Int, Pairs)

mapStep :
  Decoder Term -> MapStepState -> Decoder (Step MapStepState (Pairs))
mapStep decoder (arity_, pairs) =
  if arity_ <= 0 then
    succeed (Done pairs)
  else
    map2
      (\key value -> Loop (arity_ - 1, Dict.insert key value pairs))
      (tag |> andThen (\tag_ -> extractString tag_))
      (tag |> andThen (\tag_ -> extractTerm tag_))

smallBigExt : Decoder Int
smallBigExt = bigExt unsignedInt8

largeBigExt : Decoder Int
largeBigExt = bigExt (unsignedInt32 Bytes.BE)

bigExt : Decoder Int -> Decoder Int
bigExt len =
  len
  |> bigExtLenAndSign
  |> andThen (\(len_, sign_) ->
    let state = (len_, sign_, 0) in
    loop (state, 0) (bigStep unsignedInt8)
  )

bigExtLenAndSign : Decoder Int -> Decoder (Int, Sign)
bigExtLenAndSign len =
  map2 (\len_ sign_ -> Tuple.pair len_ (toSign sign_)) len signedInt8

bigStep :
  Decoder Int
  -> ((Int, Sign, Int), Int)
  -> Decoder (Step ((Int, Sign, Int), Int) Int)
bigStep decoder ((len, sign, currentIteration), result) =
  if currentIteration == (len) then
    succeed (Done (negateBasedOnSign sign result))
  else
    map (\digit ->
      let newResult = digit * 256^currentIteration + result in
      Loop ((len, sign, currentIteration + 1), newResult)
    )
    decoder

negateBasedOnSign : Sign -> Int -> Int
negateBasedOnSign sign integer =
  case sign of
    Positive -> integer
    Negative -> negate integer

toSign : Int -> Sign
toSign int_ = if int_ == 0 then Positive else Negative

atomUtf8Ext : Decoder String
atomUtf8Ext = Bytes.BE |> unsignedInt16 |> andThen intoString

smallAtomUtf8Ext : Decoder String
smallAtomUtf8Ext = unsignedInt8 |> andThen intoString

atomExt : Decoder String
atomExt  = Bytes.BE |> unsignedInt16 |> andThen intoString

smallAtomExt : Decoder String
smallAtomExt = unsignedInt8 |> andThen intoString

intoString : Int -> Decoder String
intoString len = string len

last : List a -> Maybe a
last list_ =
  case list_ of
    [] -> Nothing
    [e] -> Just e
    _ :: rem -> last rem
