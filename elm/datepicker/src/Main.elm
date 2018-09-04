module Main exposing (..)

import Browser
import Html exposing (Html, button, div, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import Time exposing (Month(..), Weekday(..))


type alias Model =
    { now : Time.Posix
    , here : Time.Zone
    , config : Config
    }


type alias Config =
    { firstDayOfWeek : Time.Weekday }


config : Config
config =
    { firstDayOfWeek = Mon }


initialModel : Model
initialModel =
    { now = Time.millisToPosix 0
    , here = Time.utc
    , config = config
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform NewTime
    )


type alias Date =
    { year : Int
    , month : Time.Month
    , day : Int
    , weekday : Time.Weekday
    , hour : Int
    , minute : Int
    , seconds : Int
    , milliseconds : Int
    , posix : Time.Posix
    , zone : Time.Zone
    }


toDate : Time.Zone -> Time.Posix -> Date
toDate zone time =
    { year = Time.toYear zone time
    , month = Time.toMonth zone time
    , day = Time.toDay zone time
    , weekday = Time.toWeekday zone time
    , hour = Time.toHour zone time
    , minute = Time.toMinute zone time
    , seconds = Time.toSecond zone time
    , milliseconds = Time.toMillis zone time
    , posix = time
    , zone = zone
    }


daysInMonth : Int -> Month -> Int
daysInMonth y m =
    case m of
        Jan ->
            31

        Feb ->
            if isLeapYear y then
                29
            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear y =
    modBy y 400 == 0 || modBy y 100 /= 0 && modBy y 4 == 0



-- UPDATE


type Msg
    = NewTime ( Time.Zone, Time.Posix )
    | ChangeFirstDay Time.Weekday


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        config_ =
            model.config
    in
    case msg of
        NewTime ( zone, time ) ->
            ( { model | here = zone, now = time }
            , Cmd.none
            )

        ChangeFirstDay newFirstDay ->
            ( { model | config = { config_ | firstDayOfWeek = newFirstDay } }
            , Cmd.none
            )


changeDay : Int -> Time.Zone -> Time.Posix -> Time.Posix
changeDay day zone time =
    let
        date =
            toDate zone time

        dayDiff =
            day - date.day

        dayDiffInMillis =
            dayDiff * millisecondsInDay
    in
    (Time.posixToMillis date.posix + dayDiffInMillis)
        |> Time.millisToPosix


millisecondsInDay : Int
millisecondsInDay =
    1000 * 60 * 60 * 24


firstDateOfWeek_ : Time.Weekday -> Time.Zone -> Time.Posix -> Time.Posix
firstDateOfWeek_ firstWeekday zone time =
    repeatUntil
        (\newTime ->
            (Time.posixToMillis newTime - millisecondsInDay)
                |> Time.millisToPosix
        )
        ((==) firstWeekday << .weekday << toDate zone)
        time


firstDateOfWeek : Time.Weekday -> Time.Zone -> Time.Posix -> Time.Posix
firstDateOfWeek firstWeekday zone time =
    let
        cond newTime =
            firstWeekday == .weekday (toDate zone newTime)

        rec newTime =
            if cond newTime then
                newTime
            else
                (Time.posixToMillis newTime - millisecondsInDay)
                    |> Time.millisToPosix
                    |> rec
    in
    rec time


dateEq : Time.Zone -> Time.Posix -> Time.Posix -> Bool
dateEq zone timeA timeB =
    let
        dateA =
            toDate zone timeA

        dateB =
            toDate zone timeB
    in
    ( .day dateA, .month dateA, .year dateA ) == ( .day dateB, .month dateB, .year dateB )


datesInRange : Time.Zone -> Time.Posix -> Time.Posix -> List Time.Posix
datesInRange zone firstDate lastDate =
    let
        rec acc newTime =
            if dateEq zone newTime lastDate then
                acc
            else
                (Time.posixToMillis newTime + millisecondsInDay)
                    |> Time.millisToPosix
                    |> (\t -> rec (acc ++ [ t ]) t)
    in
    rec [ firstDate ] firstDate


lastDateOfWeek : Time.Weekday -> Time.Zone -> Time.Posix -> Time.Posix
lastDateOfWeek lastWeekday zone time =
    repeatUntil
        (\newTime ->
            (Time.posixToMillis newTime + millisecondsInDay)
                |> Time.millisToPosix
        )
        ((==) lastWeekday << .weekday << toDate zone)
        time


repeatUntil_ : (acc -> a -> ( acc, a )) -> (acc -> a -> Bool) -> acc -> a -> acc
repeatUntil_ fn predicate acc n =
    let
        go ac m =
            if predicate acc m then
                ac
            else
                fn ac m
                    |> (\( newAcc, newM ) -> go newAcc newM)
    in
    go acc n


repeatUntil : (a -> a) -> (a -> Bool) -> a -> a
repeatUntil fn predicate n =
    let
        go m =
            if predicate m then
                m
            else
                go (fn m)
    in
    go n


previousWeekday : Time.Weekday -> Time.Weekday
previousWeekday weekday =
    case weekday of
        Mon ->
            Sun

        Tue ->
            Mon

        Wed ->
            Tue

        Thu ->
            Wed

        Fri ->
            Thu

        Sat ->
            Fri

        Sun ->
            Sat


weekDayToString : Weekday -> String
weekDayToString weekday =
    case weekday of
        Mon ->
            "Mo"

        Tue ->
            "Tu"

        Wed ->
            "We"

        Thu ->
            "Th"

        Fri ->
            "Fr"

        Sat ->
            "Sa"

        Sun ->
            "Su"


getDaysOfTheWeek : Weekday -> List Weekday
getDaysOfTheWeek firstDay =
    let
        rec currentDay acc =
            if List.length acc == 7 then
                acc
            else
                rec (previousWeekday currentDay) (currentDay :: acc)
    in
    rec (previousWeekday firstDay) []


listGrouping : Int -> List a -> List (List a)
listGrouping width elements =
    let
        go i list racc acc =
            case list of
                [] ->
                    List.reverse acc

                x :: xs ->
                    if i == width - 1 then
                        go 0 xs [] (List.reverse (x :: racc) :: acc)
                    else
                        go (i + 1) xs (x :: racc) acc
    in
    go 0 elements [] []



-- VIEW


view : Model -> Html Msg
view model =
    let
        date =
            toDate model.here model.now

        firstDayOfMonth =
            changeDay 1 model.here model.now

        lastDayOfMonth =
            changeDay (daysInMonth date.year date.month) model.here model.now

        calendarData =
            listGrouping 7
                (datesInRange model.here
                    (firstDateOfWeek model.config.firstDayOfWeek model.here firstDayOfMonth)
                    (lastDateOfWeek (previousWeekday model.config.firstDayOfWeek) model.here lastDayOfMonth)
                )
    in
    div []
        [ table
            [ style "border-collapse" "collapse"
            , style "border-spacing" "0"
            ]
            [ thead []
                [ tr []
                    (List.map
                        (\day ->
                            th [] [ text (weekDayToString day) ]
                        )
                        (getDaysOfTheWeek model.config.firstDayOfWeek)
                    )
                ]
            , tbody []
                (List.map
                    (\row ->
                        tr []
                            (List.map
                                (\cellDate ->
                                    td [ style "border" "1px solid lightgrey" ]
                                        [ button
                                            [ style "height" "100%"
                                            , style "width" "100%"
                                            , style "background" "none"
                                            , style "border" "0"
                                            , style "padding" "15px"
                                            ]
                                            [ .day (toDate model.here cellDate)
                                                |> String.fromInt
                                                |> text
                                            ]
                                        ]
                                )
                                row
                            )
                    )
                    calendarData
                )
            ]
        , select [] (List.map (\( weekdayStr, weekday ) -> option [ onClick (ChangeFirstDay weekday) ] [ text weekdayStr ]) weekdayList)
        ]


weekdayList =
    [ ( "Mon", Mon ), ( "Tue", Tue ), ( "Wed", Wed ), ( "Thu", Thu ), ( "Fri", Fri ), ( "Sat", Sat ), ( "Sun", Sun ) ]


dateToString : Time.Zone -> Time.Posix -> String
dateToString zone time =
    let
        date =
            toDate zone time
    in
    Debug.toString date.weekday ++ " " ++ Debug.toString date.day ++ " " ++ Debug.toString date.month



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
