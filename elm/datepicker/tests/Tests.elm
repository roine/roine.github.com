module Tests exposing (..)

import Expect exposing (Expectation)
import Extra.Time exposing (toDate, toMonth)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (addMonths)
import Test exposing (..)
import Time exposing (Month(..), utc)


oneMarch2018 =
    1519945032000
        |> Time.millisToPosix


suite : Test
suite =
    describe "Datepicker"
        [ describe "addMonths"
            [ test "adds a month" <|
                \() ->
                    toDate utc (addMonths 1 utc oneMarch2018)
                        |> toMonth
                        |> Expect.equal Apr
            , test "subs a month" <|
                \() ->
                    toDate utc (addMonths -1 utc oneMarch2018)
                        |> toMonth
                        |> Expect.equal Feb
            ]
        ]
