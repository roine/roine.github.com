---
layout: post
title: Building a calendar UI in ELM - Part 2
---
In the previous part of this tutorial we've seen how to represent the current month view into a list of dates. 
Now let's see how we transform this data into something similar to the screenshot below.

<center><img src="{{ "/elm/datepicker/images/screenshot_1.png" | absolute_url }}" alt="Typical calendar view" width="250"/></center>

__Note__: In the following some code samples are heavily influenced or copied from [elm-community/elm-datepicker][1].

## Transform the flat list into a list of list
We'll need a list of list because we will have multiple rows as well as columns. Let's make a function that takes an `Int` (amount of columns
in a row), a list (our date list) and return a list of list.
```haskell
listGrouping : Int -> List a -> List (List a)
listGrouping width elements =
    let
        rec i list racc acc =
            case list of
                [] ->
                    List.reverse acc

                x :: xs ->
                    if i == width - 1 then
                        go 0 xs [] (List.reverse (x :: racc) :: acc)
                    else
                        go (i + 1) xs (x :: racc) acc
    in
    rec 0 elements [] []
```
Procedure:
- Is the list empty?
    - No? Is the row size equal to the width we expect?
        - No? Push the head of the list (x) in the row accumulator (racc). Retry with the tail (xs) and incremented width (i + 1).
        - Yes? Reverse the row accumulator and then add it in the general accumulator (acc), so we have a list of list. 
        Retry with a reset width (i) and a reset row accumulator (racc).
    - Yes? Return the reversed general accumulator (acc).
    
The data now has the shape we want, we can start working on the UI.

## Rendering the days in the calendar
We're going to use a table to render the days.
```haskell
table [ style "border-collapse" "collapse", style "border-spacing" "0" ]
    [ tbody []
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
```
This is quite simple, we loop through each week and within each week we loop through each day and then render it.


## Rendering the days of the week
Now we'll need to display the days of the week on top of the grid. We'll need to make a function that'll take a day
check the day before add it to the accumulator and repeat until the accumulator's size is 7.
So we'll make a recursive function that will make use of `previousWeekday`
defined in [the previous article][part_1]. Let's see the definition:
```haskell
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
```
We just need to render it now:
```haskell
    thead []
        [ tr []
            (List.map
                (\day ->
                    th [] [ text (weekDayToString day) ]
                )
                (getDaysOfTheWeek model.config.firstDayOfWeek)
            )
        ]
```

This part was quite easy. It's all about getting the data right, the rest is a piece of cake. In the next part we'll
add some actions to the calendar like going to the previous/next month, selecting a day, selecting a month, selecting a year and
then displaying the selection in an input.

[Sample can be found here][sample] and [code can be found on github][tag_code]

__This is a multiple part tutorial__:
- [Part 1][part_1]


[1]: https://github.com/elm-community/elm-datepicker
[part_1]: {% post_url 2018-08-26-building-a-calendar-ui-in-elm-part-1 %}
[sample]: /elm/datepicker/index_2.html
[tag_code]: https://github.com/roine/roine.github.com/blob/elm-datepicker-part2/elm/datepicker/src/Main.elm