module Eetf.Http exposing
  ( expectTerm
  )

{-| Receive HTTP responses in Erlang's External Term Format.

# Expect
@docs expectTerm
-}

import Bytes.Decode
import Http
import Eetf.Decode exposing (Decoder)

{-| Expect the response body to be an Erlang term encoded in the External Term
Format. This function is meant to be used like the `expectString` or
`expectJson` functions in the `elm/http` package:

    import Eetf.Decode
    import Eetf.Http
    import Http

    type Msg
      = GotText (Result Http.Error String)

    getRandomText : Cmd Msg
    getRandomText =
      Http.get
        { url = "https://example.com/texts/123"
        , expect = Eetf.Http.expectTerm GotText Eetf.Decode.string
        }

The response is a sequence of bytes encoded in the External Term Format, but in
this case we expect it to be encoded text that can be turned into a `String`.
-}
expectTerm : (Result Http.Error a -> msg) -> Decoder a -> Http.Expect msg
expectTerm toMsg decoder =
  Http.expectBytesResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (Http.BadUrl url)
        Http.Timeout_ ->
          Err Http.Timeout
        Http.NetworkError_ ->
          Err Http.NetworkError
        Http.BadStatus_ metadata _ ->
          Err (Http.BadStatus metadata.statusCode)
        Http.GoodStatus_ _ body ->
          case Eetf.Decode.decodeBytes decoder body of
            Ok val -> Ok val
            Err err -> Err (Http.BadBody (extractReasonFromError err))

-- Private

extractReasonFromError : Eetf.Decode.Error -> String
extractReasonFromError error =
  case error of
    Eetf.Decode.WrongType reason -> reason
    Eetf.Decode.InvalidTupleSize reason -> reason
    Eetf.Decode.MapFieldNotFound reason -> reason
    Eetf.Decode.IndexOutOfBounds reason -> reason
    Eetf.Decode.IndexLesserThanZero reason -> reason
    Eetf.Decode.NoMatchingDecoder reason -> reason
    Eetf.Decode.InvalidTerm reason -> reason
    Eetf.Decode.UnableToDecode reason -> reason

