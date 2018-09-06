module Main exposing (..)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time


type alias Model =
    { parameter :
        { gravity : Float
        , mass : Float
        , acceleration : Float
        , containerWidth : Int
        , containerHeight : Int
        , boxHeight : Int
        , boxWidth : Int
        }
    , velocity : { x : Float, y : Float }
    , position : { x : Float, y : Float }
    , play : Bool
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { parameter =
        { gravity = 9.81
        , mass = 10
        , acceleration = 0
        , boxWidth = 10
        , boxHeight = 20
        , containerWidth = 600
        , containerHeight = 300
        }
    , velocity = { x = 0, y = 0.1 }
    , position = { x = 0, y = 0 }
    , play = True
    }



-- UPDATE


type Msg
    = Draw Time.Posix
    | TogglePlay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw t ->
            let
                position =
                    model.position
            in
            ( { model | position = { position | y = model.position.y + model.velocity.y } }, Cmd.none )

        TogglePlay ->
            ( { model | play = not model.play }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view ({ parameter, position } as model) =
    div []
        [ div
            [ style "border" "1px solid red"
            , style "width" (intToPx parameter.containerWidth)
            , style "height" (intToPx parameter.containerHeight)
            , style "position" "relative"
            ]
            [ viewBox model
            ]
        , div [] [ text "F=ma" ]
        , button [ onClick TogglePlay ] [ text "Play/Pause" ]
        ]


viewBox { parameter, position } =
    span
        [ style "position" "absolute"
        , style "bottom" (floatToPx position.y)
        , style "left" (floatToPx position.x)
        , style "height" (intToPx parameter.boxHeight)
        , style "width" (intToPx parameter.boxWidth)
        , style "background" "green"
        ]
        []


intToPx : Int -> String
intToPx num =
    String.fromInt num ++ "px"


floatToPx : Float -> String
floatToPx num =
    String.fromFloat num ++ "px"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.play then
        Time.every (1000 / 40) Draw
    else
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
