module Eetf.Http exposing
  ( expectTerm
  )

import Bytes.Decode
import Http
import Eetf.Decode exposing (Decoder)

{-| Expect the response body to be an Erlang term.
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
    Eetf.Decode.NoMatchingDecoder reason -> reason
    Eetf.Decode.InvalidTerm reason -> reason
    Eetf.Decode.UnableToDecode reason -> reason

