module Main exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import List exposing (..)
import List.Extra exposing (..)


type Square
    = X
    | O
    | Empty


type alias Grid =
    List Square


type alias Model =
    { grid : Grid
    , player : Square
    }


type Msg
    = Select Int Int
    | NoOp


emptyGrid : Grid
emptyGrid =
    repeat 9 Empty


initialModel : Model
initialModel =
    { grid = emptyGrid
    , player = X
    }


view : Model -> Html Msg
view model =
    svg
        [ width "300", height "300", viewBox "0 0 300 300" ]
        (viewLines ++ (viewGrid model.grid))


viewGrid : Grid -> List (Svg Msg)
viewGrid grid =
    grid
        |> groupsOf 3
        |> List.indexedMap viewRow
        |> List.concat


viewRow : Int -> List Square -> List (Svg Msg)
viewRow r row =
    row
        |> List.indexedMap (viewSquare r)


viewSquare : Int -> Int -> Square -> Svg Msg
viewSquare row col square =
    case square of
        X ->
            text_
                [ x (toString <| row * 100 + 50)
                , y (toString <| col * 100 + 50)
                ]
                [ text "X" ]

        O ->
            text_
                [ x (toString <| row * 100 + 50)
                , y (toString <| col * 100 + 50)
                ]
                [ text "O" ]

        Empty ->
            rect
                [ x (toString <| row * 100)
                , y (toString <| col * 100)
                , width "100"
                , height "100"
                , fillOpacity "0"
                , onClick (Select row col)
                ]
                []


viewLines : List (Svg Msg)
viewLines =
    [ viewLine 0 100 300 100
    , viewLine 0 200 300 200
    , viewLine 100 0 100 300
    , viewLine 200 0 200 300
    ]


viewLine : Int -> Int -> Int -> Int -> Svg Msg
viewLine x1_ y1_ x2_ y2_ =
    line
        [ x1 (toString x1_)
        , y1 (toString y1_)
        , x2 (toString x2_)
        , y2 (toString y2_)
        , strokeWidth "5"
        , stroke "black"
        ]
        []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select row col ->
            let
                nextPlayer =
                    case model.player of
                        X ->
                            O

                        _ ->
                            X
            in
                case setAt (row * 3 + col) model.player model.grid of
                    Just g ->
                        { model | grid = g, player = nextPlayer }

                    Nothing ->
                        model

        NoOp ->
            model


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
