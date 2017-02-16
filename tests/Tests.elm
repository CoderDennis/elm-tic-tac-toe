module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Game exposing (..)


all : Test
all =
    describe "Tic-Tac-Toe Tests"
        [ describe "GameState"
            [ test "Game in progress returns Nothing" <|
                \() ->
                    gameState emptyGrid
                        |> Expect.equal InProgress
            , test "PlayerX won on 2nd row" <|
                \() ->
                    gameState
                        [ Empty
                        , O
                        , O
                        , X
                        , X
                        , X
                        , Empty
                        , Empty
                        , Empty
                        ]
                        |> Expect.equal (Won PlayerX)
            , test "PlayerO won on 2nd column" <|
                \() ->
                    gameState
                        [ Empty
                        , O
                        , X
                        , X
                        , O
                        , Empty
                        , X
                        , O
                        , Empty
                        ]
                        |> Expect.equal (Won PlayerO)
            ]
        ]
