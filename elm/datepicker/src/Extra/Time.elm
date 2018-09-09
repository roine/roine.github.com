module Extra.Time exposing (..)

import Time exposing (Month(..), Weekday(..))


type Date
    = Date
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


toWeekday : Date -> Weekday
toWeekday (Date date) =
    date.weekday


toDay : Date -> Int
toDay (Date date) =
    date.day


toMonth : Date -> Month
toMonth (Date date) =
    date.month


toDate : Time.Zone -> Time.Posix -> Date
toDate zone time =
    Date
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


fromDate : Time.Zone -> Date -> Time.Posix
fromDate zone date =
    Time.millisToPosix 0


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


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


isLeapYear : Int -> Bool
isLeapYear y =
    modBy y 400 == 0 || modBy y 100 /= 0 && modBy y 4 == 0


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
