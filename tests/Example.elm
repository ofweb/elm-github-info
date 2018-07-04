module Example exposing (..)

import Main exposing (..)
import Expect
import Fuzz exposing (Fuzzer, string)
import Test exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode as Json


suite : Test
suite =
  describe "Github user decoder"
    [fuzz4 string string string string "user decoder maps to user" <|
      \name avatar link login ->
        let
            json =
                   Json.object
                       [  ("name", Json.string name)
                       ,  ("avatar_url", Json.string avatar)
                       , ("html_url", Json.string link)
                       , ("login", Json.string login)
                      ]
        in
        decodeValue userDecoder json
            |> Expect.equal (Ok {name = name, avatar = avatar, link=link, login=login})
    ]
