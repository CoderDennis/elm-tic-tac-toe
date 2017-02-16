module Game exposing (..)

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


type Player
    = PlayerX
    | PlayerO
    | Cat


type alias Model =
    { grid : Grid
    , player : Player
    }


type Msg
    = Select Int Int
    | Reset


type GameState
    = InProgress
    | Won Player


emptyGrid : Grid
emptyGrid =
    repeat 9 Empty


initialModel : Model
initialModel =
    { grid = emptyGrid
    , player = PlayerX
    }


gameState : Grid -> GameState
gameState grid =
    let
        rows =
            grid |> groupsOf 3

        rowWinners =
            rows
                |> filterMap threeInRow

        colWinners =
            rows
                |> transpose
                |> filterMap threeInRow
    in
        case rowWinners ++ colWinners of
            [ p ] ->
                Won p

            _ ->
                InProgress


threeInRow : List Square -> Maybe Player
threeInRow row =
    case row of
        [ X, X, X ] ->
            Just PlayerX

        [ O, O, O ] ->
            Just PlayerO

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    svg
        [ viewBox "0 0 300 400"
        , preserveAspectRatio "xMidYMid meet"
        , height "100%"
        , width "100%"
        ]
        [ text_
            [ x "20"
            , y "30"
            , onClick Reset
            , fontSize "35"
            ]
            [ text "Reset" ]
        , g [ transform "translate(0,50)" ]
            [ viewLines
            , viewGrid model.grid
            ]
        ]


viewGrid : Grid -> Svg Msg
viewGrid grid =
    g []
        (grid
            |> groupsOf 3
            |> List.indexedMap viewRow
            |> List.concat
        )


viewRow : Int -> List Square -> List (Svg Msg)
viewRow r row =
    row
        |> List.indexedMap (viewSquare r)


xoWidth : String
xoWidth =
    "13"


viewSquare : Int -> Int -> Square -> Svg Msg
viewSquare row col square =
    case square of
        X ->
            g
                [ strokeLinecap "round"
                , strokeWidth xoWidth
                , stroke "black"
                ]
                [ viewLine
                    (row * 100 + 20)
                    (col * 100 + 20)
                    (row * 100 + 80)
                    (col * 100 + 80)
                , viewLine
                    (row * 100 + 80)
                    (col * 100 + 20)
                    (row * 100 + 20)
                    (col * 100 + 80)
                ]

        O ->
            circle
                [ cx (toString <| row * 100 + 50)
                , cy (toString <| col * 100 + 50)
                , r "30"
                , strokeWidth xoWidth
                , stroke "black"
                , fillOpacity "0"
                ]
                []

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


viewLines : Svg Msg
viewLines =
    g
        [ strokeWidth "10"
        , stroke "black"
        ]
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
        ]
        []


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            initialModel

        Select row col ->
            let
                nextPlayer =
                    case model.player of
                        PlayerX ->
                            PlayerO

                        _ ->
                            PlayerX

                square =
                    case model.player of
                        PlayerX ->
                            X

                        _ ->
                            O

                newGrid =
                    setAt (row * 3 + col) square model.grid
            in
                case newGrid of
                    Just g ->
                        { model | grid = g, player = nextPlayer }

                    Nothing ->
                        model


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
