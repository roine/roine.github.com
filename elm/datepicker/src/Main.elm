module Main exposing (..)

import Browser
import Extra.Time exposing (..)
import Html exposing (Html, button, div, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task
import Time exposing (Month(..), Weekday(..))


type alias Model =
    { now : Time.Posix
    , here : Time.Zone
    , config : Config
    , currentDateView : Time.Posix
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
    , currentDateView = Time.millisToPosix 0
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel
    , Task.map2 Tuple.pair Time.here Time.now
        |> Task.perform NewTime
    )



-- UPDATE


type Msg
    = NewTime ( Time.Zone, Time.Posix )
    | ChangeFirstDay Time.Weekday
    | PrevMonth
    | NextMonth


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        config_ =
            model.config
    in
    case msg of
        NewTime ( zone, time ) ->
            ( { model | here = zone, now = time, currentDateView = time }
            , Cmd.none
            )

        ChangeFirstDay newFirstDay ->
            ( { model | config = { config_ | firstDayOfWeek = newFirstDay } }
            , Cmd.none
            )

        PrevMonth ->
            ( { model | currentDateView = addMonths -1 model.here model.currentDateView }, Cmd.none )

        NextMonth ->
            ( { model | currentDateView = addMonths 1 model.here model.currentDateView }, Cmd.none )


changeDay : Int -> Time.Zone -> Time.Posix -> Time.Posix
changeDay day zone time =
    let
        (Date date) =
            toDate zone time

        dayDiff =
            day - date.day

        dayDiffInMillis =
            dayDiff * millisecondsInDay
    in
    (Time.posixToMillis date.posix + dayDiffInMillis)
        |> Time.millisToPosix


addDays : Int -> Time.Posix -> Time.Posix
addDays days time =
    (Time.posixToMillis time
        + days
        * 24
        * 60
        * 60
        * 1000
    )
        |> Time.millisToPosix


addMonths : Int -> Time.Zone -> Time.Posix -> Time.Posix
addMonths month zone time =
    let
        (Date date) =
            toDate zone time
    in
    addDays (daysInMonth date.year date.month * month) time


millisecondsInDay : Int
millisecondsInDay =
    1000 * 60 * 60 * 24


firstDateOfWeek : Time.Weekday -> Time.Zone -> Time.Posix -> Time.Posix
firstDateOfWeek firstWeekday zone time =
    let
        cond newTime =
            firstWeekday == toWeekday (toDate zone newTime)

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
        (Date dateA) =
            toDate zone timeA

        (Date dateB) =
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
        ((==) lastWeekday << toWeekday << toDate zone)
        time


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
        (Date date) =
            toDate model.here model.currentDateView

        firstDayOfMonth =
            changeDay 1 model.here model.currentDateView

        lastDayOfMonth =
            changeDay (daysInMonth date.year date.month) model.here model.currentDateView

        calendarData =
            listGrouping 7
                (datesInRange model.here
                    (firstDateOfWeek model.config.firstDayOfWeek model.here firstDayOfMonth)
                    (lastDateOfWeek (previousWeekday model.config.firstDayOfWeek) model.here lastDayOfMonth)
                )
    in
    div [ style "width" "350px" ]
        [ div [ style "display" "flex" ]
            [ div
                [ style "flex" "1"
                , style "text-align" "left"
                ]
                [ button [ onClick PrevMonth ] [ text "Prev" ] ]
            , div
                [ style "flex" "2"
                , style "text-align" "center"
                ]
                [ text (monthToString date.month ++ " " ++ String.fromInt date.year) ]
            , div
                [ style "flex" "1"
                , style "text-align" "right"
                ]
                [ button [ onClick NextMonth ] [ text "Next" ] ]
            ]
        , table
            [ style "border-collapse" "collapse"
            , style "border-spacing" "0"
            , style "width" "100%"
            , style "table-layout" "fixed"
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
                                            , style "padding" "10px 0"
                                            ]
                                            [ toDay (toDate model.here cellDate)
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
        , text "First day of the week"
        , select []
            (List.map
                (\( weekdayStr, weekday ) ->
                    option [ onClick (ChangeFirstDay weekday) ] [ text weekdayStr ]
                )
                weekdayList
            )
        ]


weekdayList : List ( String, Weekday )
weekdayList =
    [ ( "Mon", Mon ), ( "Tue", Tue ), ( "Wed", Wed ), ( "Thu", Thu ), ( "Fri", Fri ), ( "Sat", Sat ), ( "Sun", Sun ) ]



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
