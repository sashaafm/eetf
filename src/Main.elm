module Main exposing (main)

import Browser
import Bytes exposing (Bytes)
import Bytes.Decode
import Dict exposing (Dict)
import Eetf
import Eetf.Decode exposing (Decoder)
import Eetf.Encode
import Eetf.Http
import Html
import Html.Attributes
import Html.Events
import Http
import Platform

type alias Flags = ()
type TermType
  = ErlSmallInteger
  | ErlInteger
  | ErlFloat
  | ErlString
  | ErlBitBinary
  | ErlBinary
  | ErlAtom
  | ErlAtomUtf8
  | ErlSmallAtomUtf8
  | ErlSmallTuple
  | ErlLargeTuple
  | ErlNil
  | ErlList
  | ErlPid
  | ErlPort
  | ErlMap
  | ErlSmallBigNum
  | ErlLargeBigNum
  | ErlExport
  | ErlNewReference
  | ErlNewFun
type Payload a
  = NotRequested
  | Loading
  | Failure
  | Received a
type Role = Visitor | Owner | Admin
type alias Company =
  { name : String
  , sharePrice : Float
  }
type alias House =
  { value : Int
  , region : String
  }
type alias Person =
  { name : String
  , role : Role
  , nicknames : List String
  , houses : List House
  , posts : List Int
  , crimes : List (String, Int)
  , father : String
  , company : Company
  }
type alias Model =
  { term : Payload Eetf.Term
  , number : Payload Int
  , float : Payload Float
  , string : Payload String
  , keyValuePairs : Payload (List (Int, Float))
  , listInt : Payload (List Int)
  , role : Payload Role
  , person : Payload Person
  , toPost : String
  }

type Msg
  = GotTerm (Result Http.Error Eetf.Term)
  | GotNumber (Result Http.Error Int)
  | GotFloat (Result Http.Error Float)
  | GotString (Result Http.Error String)
  | GotListInt (Result Http.Error (List Int))
  | GotKeyValuePairs (Result Http.Error (List (Int, Float)))
  | GotRole (Result Http.Error Role)
  | GotPerson (Result Http.Error Person)
  | RequestTerm TermType
  | RequestNumber
  | RequestFloat
  | RequestKeyValuePairs
  | RequestListInt
  | RequestString
  | RequestRole
  | RequestPerson
  | UpdatePostTerm String
  | PostInt
  | PostFloat
  | PostBool
  | PostString
  | PostList
  | PostDict
  | PostTuple
  | GotSentTermResponse (Result Http.Error String)

getTerm : TermType -> Cmd Msg
getTerm termType =
  Http.get
    { url = "http://localhost:4001/term?type=" ++ (termTypeToString termType)
    , expect = Http.expectBytes GotTerm Eetf.decode
    }

getNumber : Cmd Msg
getNumber =
  Http.get
    { url = "http://localhost:4001/number"
    , expect = Eetf.Http.expectTerm GotNumber Eetf.Decode.int
    }

getFloat : Cmd Msg
getFloat =
  Http.get
    { url = "http://localhost:4001/float"
    , expect = Eetf.Http.expectTerm GotFloat Eetf.Decode.float
    }

getString : Cmd Msg
getString =
  Http.get
    { url = "http://localhost:4001/string"
    , expect = Http.expectString GotString
    }

getListInt : Cmd Msg
getListInt =
  Http.get
    { url = "http://localhost:4001/list_int"
    , expect =
        Eetf.Http.expectTerm GotListInt (Eetf.Decode.list Eetf.Decode.int)
    }

getKeyValuePairs : Cmd Msg
getKeyValuePairs =
  Http.get
    { url = "http://localhost:4001/key_value_pairs"
    , expect =
        Eetf.Http.expectTerm
          GotKeyValuePairs
          (Eetf.Decode.keyValuePairs
            (Eetf.Decode.tuple2
              Eetf.Decode.int
              Eetf.Decode.float
            )
          )
    }

getRole : Cmd Msg
getRole =
  Http.get
    { url = "http://localhost:4001/role"
    , expect = Eetf.Http.expectTerm GotRole roleDecoder
    }

getPerson : Cmd Msg
getPerson =
  Http.get
    { url = "http://localhost:4001/person"
    , expect = Eetf.Http.expectTerm GotPerson personDecoder
    }

postInt : Int -> Cmd Msg
postInt int =
  Http.request
    { method = "POST"
    , url = "http://localhost:4001/term"
    , headers = []
    , body = Http.bytesBody
              "application/octet-stream"
              (Eetf.Encode.encode (Eetf.Encode.int int))
    , expect = Http.expectString GotSentTermResponse
    , timeout = Nothing
    , tracker = Nothing
    }

postFloat : Float -> Cmd Msg
postFloat float =
  Http.request
    { method = "POST"
    , url = "http://localhost:4001/term"
    , headers = []
    , body = Http.bytesBody
              "application/octet-stream"
              (Eetf.Encode.encode (Eetf.Encode.float float))
    , expect = Http.expectString GotSentTermResponse
    , timeout = Nothing
    , tracker = Nothing
    }

postBool : Bool -> Cmd Msg
postBool bool =
  Http.request
    { method = "POST"
    , url = "http://localhost:4001/term"
    , headers = []
    , body = Http.bytesBody
              "application/octet-stream"
              (Eetf.Encode.encode (Eetf.Encode.bool bool))
    , expect = Http.expectString GotSentTermResponse
    , timeout = Nothing
    , tracker = Nothing
    }

postString : String -> Cmd Msg
postString string =
  Http.request
    { method = "POST"
    , url = "http://localhost:4001/term"
    , headers = []
    , body = Http.bytesBody
              "application/octet-stream"
              (Eetf.Encode.encode (Eetf.Encode.string string))
    , expect = Http.expectString GotSentTermResponse
    , timeout = Nothing
    , tracker = Nothing
    }

postList : List String -> Cmd Msg
postList list =
  Http.request
    { method = "POST"
    , url = "http://localhost:4001/term"
    , headers = []
    , body = Http.bytesBody
              "application/octet-stream"
              (Eetf.Encode.encode (Eetf.Encode.list Eetf.Encode.string list))
    , expect = Http.expectString GotSentTermResponse
    , timeout = Nothing
    , tracker = Nothing
    }

postDict : Dict String (Dict String Int) -> Cmd Msg
postDict dict =
  Http.request
    { method = "POST"
    , url = "http://localhost:4001/term"
    , headers = []
    , body = Http.bytesBody
              "application/octet-stream"
              (Eetf.Encode.encode
                (Eetf.Encode.dict
                  identity
                  (Eetf.Encode.dict identity Eetf.Encode.int)
                  dict
                )
              )
    , expect = Http.expectString GotSentTermResponse
    , timeout = Nothing
    , tracker = Nothing
    }

postTuple : (List Float, Dict String (Dict String Float), Int) -> Cmd Msg
postTuple tuple =
  Http.request
    { method = "POST"
    , url = "http://localhost:4001/term"
    , headers = []
    , body = Http.bytesBody
              "application/octet-stream"
              ( Eetf.Encode.encode
                ( Eetf.Encode.tuple3
                    ( Eetf.Encode.list Eetf.Encode.float )
                    ( Eetf.Encode.dict
                        identity
                        (Eetf.Encode.dict identity Eetf.Encode.float)
                    )
                    Eetf.Encode.int
                    tuple
                )
              )
    , expect = Http.expectString GotSentTermResponse
    , timeout = Nothing
    , tracker = Nothing
    }

roleDecoder : Decoder Role
roleDecoder =
  Eetf.Decode.string
  |> Eetf.Decode.andThen (\str ->
    case str of
      "owner" -> Eetf.Decode.succeed Owner
      "visitor" -> Eetf.Decode.succeed Visitor
      "admin" -> Eetf.Decode.succeed Admin
      _ -> Eetf.Decode.fail "Unknown role."
  )

houseDecoder : Decoder House
houseDecoder =
  Eetf.Decode.map2
    House
    (Eetf.Decode.field "value" Eetf.Decode.int)
    (Eetf.Decode.field "region" Eetf.Decode.string)

personDecoder : Decoder Person
personDecoder =
  Eetf.Decode.map8
    Person
    (Eetf.Decode.field "name" Eetf.Decode.string)
    (Eetf.Decode.field "role" roleDecoder)
    (Eetf.Decode.field "nicknames" (Eetf.Decode.list Eetf.Decode.string))
    (Eetf.Decode.field "houses" (Eetf.Decode.list houseDecoder))
    (Eetf.Decode.field "posts" (Eetf.Decode.list Eetf.Decode.int))
    (Eetf.Decode.field "crimes"
      (Eetf.Decode.keyValuePairs
        (Eetf.Decode.tuple2 Eetf.Decode.string Eetf.Decode.int)
      )
    )
    (Eetf.Decode.at ["parents", "father"] Eetf.Decode.string)
    (Eetf.Decode.field "company" companyDecoder)

companyDecoder : Decoder Company
companyDecoder =
  Eetf.Decode.map2
    Company
    (Eetf.Decode.field "name" Eetf.Decode.string)
    (Eetf.Decode.field "share_price" Eetf.Decode.float)

termTypeToString : TermType -> String
termTypeToString termType =
  case termType of
    ErlSmallInteger -> "small_integer"
    ErlInteger -> "integer"
    ErlFloat -> "float"
    ErlString -> "string"
    ErlBitBinary -> "bitstring"
    ErlBinary -> "binary"
    ErlAtomUtf8 -> "atom_utf8"
    ErlAtom -> "atom"
    ErlSmallAtomUtf8 -> "small_atom_utf8"
    ErlSmallTuple -> "small_tuple"
    ErlLargeTuple -> "large_tuple"
    ErlNil -> "nil"
    ErlList -> "list"
    ErlPid -> "pid"
    ErlPort -> "port"
    ErlMap -> "map"
    ErlSmallBigNum -> "small_big_num"
    ErlLargeBigNum -> "large_big_num"
    ErlExport -> "export"
    ErlNewReference -> "new_reference"
    ErlNewFun -> "new_fun"

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Flags -> (Model, Cmd Msg)
init _ =
  ({ term = NotRequested
   , number = NotRequested
   , float = NotRequested
   , string = NotRequested
   , listInt = NotRequested
   , keyValuePairs = NotRequested
   , role = NotRequested
   , person = NotRequested
   , toPost = ""
   }
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotTerm result ->
      let _ = Debug.log "RESULT" result in
      case result of
        Ok term ->
          ({model | term = Received term}, Cmd.none)
        Err _ ->
          ({model | term = Failure}, Cmd.none)
    GotNumber result ->
      case result of
        Ok num ->
          ({model | number = Received num}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    GotFloat result ->
      case result of
        Ok float ->
          ({model | float = Received float}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    GotListInt result ->
      let _ = Debug.log "ListInt" result in
      case result of
        Ok list ->
          ({model | listInt = Received list}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    GotKeyValuePairs result ->
      let _ = Debug.log "KeyValuePairs" result in
      case result of
        Ok keyValuePairs ->
          ({model | keyValuePairs = Received keyValuePairs}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    GotString result ->
      case result of
        Ok string ->
          ({model | string = Received string}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    GotRole result ->
      let _ = Debug.log "Role" result in
       case result of
         Ok role ->
           ({model | role = Received role}, Cmd.none)
         Err _ ->
           (model, Cmd.none)
    GotPerson result ->
      let _ = Debug.log "Person" result in
      case result of
        Ok person ->
          ({model | person = Received person}, Cmd.none)
        Err _ ->
          (model, Cmd.none)
    RequestTerm termType ->
      ({model | term = Loading}, getTerm termType)
    RequestNumber ->
      ({model | number = Loading}, getNumber)
    RequestFloat ->
      ({model | float = Loading}, getFloat)
    RequestKeyValuePairs ->
      ({model | keyValuePairs = Loading}, getKeyValuePairs)
    RequestListInt ->
      ({model | listInt = Loading}, getListInt)
    RequestString ->
      ({model | string = Loading}, getString)
    RequestRole ->
      ({model | role = Loading}, getRole)
    RequestPerson ->
      ({model | person = Loading}, getPerson)
    UpdatePostTerm str ->
      ({model | toPost = str}, Cmd.none)
    PostInt ->
      case String.toInt model.toPost of
        Just int ->
          (model, postInt int)
        Nothing ->
          (model, Cmd.none)
    PostFloat ->
      case String.toFloat model.toPost of
        Just float ->
          (model, postFloat float)
        Nothing ->
          (model, Cmd.none)
    PostBool ->
      let
          toBool = \bool ->
            case bool of
              "true" -> Just True
              "false" -> Just False
              _ -> Nothing
      in
      case toBool model.toPost of
        Just bool ->
          (model, postBool bool)
        Nothing ->
          (model, Cmd.none)
    PostString ->
      (model, postString model.toPost)
    PostList ->
      (model, postList ["olá", "cacete", "€$%©©®™"])
    PostDict ->
      ( model
      , postDict
          (Dict.fromList
            [ ("Sasha", (Dict.fromList [ ("age", 28), ("cars", 1) ]))
            , ("Anthony", (Dict.fromList [ ("age", 30), ("cars", 0) ]))
            ]
          )
      )
    PostTuple ->
      ( model
      , postTuple
          ( [5.6, 8.4, 5.2, 1.9 ]
          , ( Dict.fromList
                [ ("Notas", Dict.fromList [ ("Sasha", 5.4), ("Jorge", 7.1) ])
                , ("pH", Dict.fromList [ ("Piscina", 8.8), ("Sumo", 5.3) ])
                ]
            )
         , 10
         )
      )
    GotSentTermResponse result ->
      case result of
        Ok resp ->
          (model, Cmd.none)
        Err _ ->
          (model, Cmd.none)


view : Model -> Browser.Document Msg
view {term, number, float, string, person, listInt, keyValuePairs, role} =
  Browser.Document
    "Erlang External Term Format"
    [ Html.div [] (requestButtons)
    , Html.hr [] []
    , Html.div [] [ showTerm term ]
    , Html.hr [] []
    , Html.div [] [ requestNumber ]
    , Html.div [] [ showNumber number ]
    , Html.hr [] []
    , Html.div [] [ requestFloat ]
    , Html.div [] [ showFloat float ]
    , Html.hr [] []
    , Html.div [] [ requestListInt ]
    , Html.div [] [ showListInt listInt ]
    , Html.hr [] []
    , Html.div [] [ requestKeyValuePairs ]
    , Html.div [] [ showKeyValuePairs keyValuePairs ]
    , Html.hr [] []
    , Html.div [] [ requestString ]
    , Html.div [] [ showString string ]
    , Html.hr [] []
    , Html.div [] [ requestRole ]
    , Html.div [] [ showRole role ]
    , Html.hr [] []
    , Html.div [] [ requestPerson ]
    , Html.div [] [ showPerson person ]
    , Html.hr [] []
    , Html.h1 [] [ Html.text "Send request" ]
    , Html.div [] [ Html.text "Send Int", sendInt ]
    , Html.div [] [ Html.text "Send Float", sendFloat ]
    , Html.div [] [ Html.text "Send Bool", sendBool ]
    , Html.div [] [ Html.text "Send String", sendString ]
    , Html.div [] [ Html.text "Send List", sendList ]
    , Html.div [] [ Html.text "Send Dict", sendDict ]
    , Html.div [] [ Html.text "Send Tuple", sendTuple ]
    ]

requestButtons : List (Html.Html Msg)
requestButtons =
  [ Html.button [ Html.Events.onClick (RequestTerm ErlSmallInteger) ]
                [ Html.text "Small Integer" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlInteger) ]
                [ Html.text "Integer" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlFloat) ]
                [ Html.text "Float" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlString) ]
                [ Html.text "String" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlBitBinary) ]
                [ Html.text "Bitstring" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlBinary) ]
                [ Html.text "Binary" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlAtom) ]
                [ Html.text "Atom" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlAtomUtf8) ]
                [ Html.text "Atom UTF-8" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlSmallAtomUtf8) ]
                [ Html.text "Small Atom UTF-8" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlSmallTuple) ]
                [ Html.text "Small Tuple" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlLargeTuple) ]
                [ Html.text "Large Tuple" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlNil) ]
                [ Html.text "Nil" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlList) ]
                [ Html.text "List" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlPid) ]
                [ Html.text "PID" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlPort) ]
                [ Html.text "Port" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlMap) ]
                [ Html.text "Map" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlSmallBigNum) ]
                [ Html.text "Small Big Num" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlLargeBigNum) ]
                [ Html.text "Large Big Num" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlExport) ]
                [ Html.text "Export" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlNewReference) ]
                [ Html.text "New Reference" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlNewFun) ]
                [ Html.text "New Fun" ]
  ]

requestNumber : Html.Html Msg
requestNumber =
  Html.button [ Html.Events.onClick RequestNumber ]
              [ Html.text "Random number" ]

requestFloat : Html.Html Msg
requestFloat =
  Html.button [ Html.Events.onClick RequestFloat ]
              [ Html.text "Random float" ]

requestListInt : Html.Html Msg
requestListInt =
  Html.button [ Html.Events.onClick RequestListInt ]
              [ Html.text "Random List of Integers" ]

requestKeyValuePairs : Html.Html Msg
requestKeyValuePairs =
  Html.button [ Html.Events.onClick RequestKeyValuePairs ]
              [ Html.text "Random Key-Value Pairs" ]

requestString : Html.Html Msg
requestString =
  Html.button [ Html.Events.onClick RequestString ]
              [ Html.text "Random string" ]

requestRole : Html.Html Msg
requestRole =
  Html.button [ Html.Events.onClick RequestRole ]
              [ Html.text "Random role" ]

requestPerson : Html.Html Msg
requestPerson =
  Html.button [ Html.Events.onClick RequestPerson ]
              [ Html.text "Random person" ]

sendInt : Html.Html Msg
sendInt =
  Html.div []
           [ Html.input [ Html.Attributes.type_ "number"
                        , Html.Events.onInput UpdatePostTerm
                        ]
                        []
           , Html.button [ Html.Events.onClick PostInt ]
                         [ Html.text "Submit" ]
           ]

sendFloat : Html.Html Msg
sendFloat =
  Html.div []
           [ Html.input [ Html.Attributes.type_ "number"
                        , Html.Events.onInput UpdatePostTerm
                        ]
                        []
           , Html.button [ Html.Events.onClick PostFloat ]
                         [ Html.text "Submit" ]
           ]

sendBool : Html.Html Msg
sendBool =
  Html.div []
           [ Html.select [ Html.Events.onInput UpdatePostTerm ]
                         [ Html.option [ Html.Attributes.value "true" ]
                                       [ Html.text "true" ]
                         , Html.option [ Html.Attributes.value "false" ]
                                       [ Html.text "false" ]
                         ]
            , Html.button [ Html.Events.onClick PostBool ]
                          [ Html.text "Submit" ]
            ]

sendString : Html.Html Msg
sendString =
  Html.div []
           [ Html.input [ Html.Attributes.type_ "text"
                        , Html.Events.onInput UpdatePostTerm
                        ]
                        []
           , Html.button [ Html.Events.onClick PostString ]
                         [ Html.text "Submit" ]
           ]

sendList : Html.Html Msg
sendList =
  Html.div []
           [ Html.button [ Html.Events.onClick PostList ]
                         [ Html.text "Submit" ]
           ]

sendDict : Html.Html Msg
sendDict =
  Html.div []
           [ Html.button [ Html.Events.onClick PostDict ]
                         [ Html.text "Submit" ]
           ]

sendTuple : Html.Html Msg
sendTuple =
  Html.div []
           [ Html.button [ Html.Events.onClick PostTuple ]
                         [ Html.text "Submit" ]
           ]

showTerm : Payload Eetf.Term -> Html.Html Msg
showTerm payload =
  case payload of
    NotRequested -> Html.text "Not requested."
    Loading -> Html.text "Loading..."
    Failure -> Html.text "Something went wrong."
    Received something -> Html.text "Got something."

showNumber : Payload Int -> Html.Html Msg
showNumber payload =
  case payload of
    Received number -> Html.text ("Received number " ++ String.fromInt number)
    _ -> Html.text "Nothing yet."

showFloat : Payload Float -> Html.Html Msg
showFloat payload =
  case payload of
    Received float -> Html.text ("Received float " ++ String.fromFloat float)
    _ -> Html.text "Nothing yet."

showListInt : Payload (List Int) -> Html.Html Msg
showListInt payload =
  case payload of
    Received list ->
      let str = intoString String.fromInt list in
      Html.text str
    _ ->
      Html.text "Nothing yet."

showKeyValuePairs : Payload (List (Int, Float)) -> Html.Html Msg
showKeyValuePairs payload =
  case payload of
    Received list ->
      let
          func = \(s, f) ->
            "(" ++ String.fromInt s ++ ", "++ (String.fromFloat f) ++ ")"
          str = intoString func list
      in
      Html.text str
    _ ->
      Html.text "Nothing yet."

intoString : (a -> String) -> List a -> String
intoString func list = "[" ++ (doIntoString func list)

doIntoString : (a -> String) -> List a -> String
doIntoString func list =
  case list of
    [] ->
      "]"
    h :: [] ->
      (func h) ++ (doIntoString func [])
    h :: t ->
      (func h) ++ ", " ++ (doIntoString func t)

showString : Payload String -> Html.Html Msg
showString payload =
  case payload of
    Received string -> Html.text ("Received string " ++ string)
    _ -> Html.text "Nothing yet."

showRole : Payload Role -> Html.Html Msg
showRole payload =
  case payload of
    Received role -> Html.text ("Received role " ++ (roleToString role))
    _ -> Html.text "Nothing yet."

showPerson : Payload Person -> Html.Html Msg
showPerson payload =
  case payload of
    Received {name, role, nicknames, houses, posts, company} ->
      Html.div
        []
        [ Html.text (
            "Received person named "
              ++ name
              ++ " with role "
              ++ (roleToString role)
              ++ "."
              ++ "His houses are "
              ++ (intoString houseToString houses)
              ++ "."
              ++ "His posts are "
              ++ (intoString String.fromInt posts)
              ++ "."
              ++ "He usually goes by one of these nicknames "
              ++ (intoString identity nicknames)
              ++ "."
              ++ "His company is named "
              ++ company.name
              ++ " and its share price is "
              ++ String.fromFloat company.sharePrice
          )
        ]
    _ ->
      Html.text "Nothing yet."

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

roleToString : Role -> String
roleToString role =
  case role of
    Owner -> "owner"
    Visitor -> "visitor"
    Admin -> "admin"

houseToString : House -> String
houseToString house =
  "{value => "
  ++ String.fromInt house.value
  ++ " | region => "
  ++ house.region
  ++ "}"
