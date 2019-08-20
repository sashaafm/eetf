module Main exposing (main)

import Browser
import Bytes exposing (Bytes)
import Eetf
import Html
import Html.Events
import Http
import Platform

type alias Flags = ()
type TermType
  = ErlInteger
  | ErlFloat
  | ErlAtom
  | ErlTuple
  | ErlList
  | ErlPid
  | ErlMap
type Payload
  = NotRequested
  | Loading TermType
  | Failure
  | Received Eetf.Term
type alias Model =
  { payload : Payload
  }

type Msg
  = GotResponse (Result Http.Error Eetf.Term)
  | RequestTerm TermType

getTerm : TermType -> Cmd Msg
getTerm termType =
  Http.get
    { url = "http://localhost:4001/term?type=" ++ (termTypeToString termType)
    , expect = Http.expectBytes GotResponse Eetf.decode
    }

termTypeToString : TermType -> String
termTypeToString termType =
  case termType of
    ErlInteger -> "integer"
    ErlFloat -> "float"
    ErlAtom -> "atom"
    ErlTuple -> "tuple"
    ErlList -> "list"
    ErlPid -> "pid"
    ErlMap -> "map"

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Flags -> (Model, Cmd Msg)
init _ = ((Model NotRequested), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotResponse result ->
      let _ = Debug.log "RESULT" result in
      case result of
        Ok term ->
          ({model | payload = Received term}, Cmd.none)
        Err _ ->
          ({model | payload = Failure}, Cmd.none)
    RequestTerm termType ->
      ({model | payload = Loading termType}, getTerm termType)

view : Model -> Browser.Document Msg
view {payload} =
  Browser.Document
    "Erlang External Term Format"
    [ Html.div [] (requestButtons)
    , Html.hr [] []
    , Html.div [] [ showPayload payload ]
    ]

requestButtons : List (Html.Html Msg)
requestButtons =
  [ Html.button [ Html.Events.onClick (RequestTerm ErlInteger) ]
                [ Html.text "Integer" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlFloat) ]
                [ Html.text "Float" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlAtom) ]
                [ Html.text "Atom" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlTuple) ]
                [ Html.text "Tuple" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlList) ]
                [ Html.text "List" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlPid) ]
                [ Html.text "PID" ]
  , Html.button [ Html.Events.onClick (RequestTerm ErlMap) ]
                [ Html.text "Map" ]
  ]

showPayload : Payload -> Html.Html Msg
showPayload payload =
  case payload of
    NotRequested -> Html.text "Not requested."
    Loading termType -> Html.text ("Loading " ++ termTypeToString termType)
    Failure -> Html.text "Something went wrong."
    Received term -> Html.text "Got term: "

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
