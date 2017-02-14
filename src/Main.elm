module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List


type Square
    = X
    | O
    | Empty


type alias Grid =
    List (List Square)


type alias Model =
    { grid : Grid
    }


type Msg
    = NoOp


emptyGrid : Grid
emptyGrid =
    List.repeat 3 <|
        List.repeat 3 Empty


initialModel : Model
initialModel =
    { grid = emptyGrid }


view : Model -> Html Msg
view model =
    svg
        [ width "300", height "300", viewBox "0 0 300 300" ]
        [ text_
            [ x "0"
            , y "35"
            , fontFamily "Verdana"
            , fontSize "35"
            ]
            [ text "Hello SVG" ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
