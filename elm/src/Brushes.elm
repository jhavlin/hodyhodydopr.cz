module Brushes exposing (Brush(..), brushPoints)


type Brush
    = B1
    | B2
    | B3
    | B4
    | B5


brushPoints : Brush -> List ( Int, Int )
brushPoints brush =
    case brush of
        B1 ->
            b1

        B2 ->
            b2

        B3 ->
            b3

        B4 ->
            b4

        B5 ->
            b5


b1 : List ( Int, Int )
b1 =
    [ ( 0, 0 ) ]


b2 : List ( Int, Int )
b2 =
    [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ), ( 1, 1 ) ]


b3 : List ( Int, Int )
b3 =
    [ ( 0, 0 ), ( -1, 0 ), ( 1, 0 ), ( 0, 1 ), ( 0, -1 ) ]


b4 : List ( Int, Int )
b4 =
    [ ( 0, -2 )
    , ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( -2, 0 )
    , ( -1, 0 )
    , ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( -1, 1 )
    , ( 0, 1 )
    , ( 1, 1 )
    , ( 0, 2 )
    ]


b5 : List ( Int, Int )
b5 =
    [ ( -1, -3 )
    , ( 0, -3 )
    , ( 1, -3 )
    , ( -2, -2 )
    , ( -1, -2 )
    , ( 0, -2 )
    , ( 1, -2 )
    , ( 2, -2 )
    , ( -3, -1 )
    , ( -2, -1 )
    , ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( 2, -1 )
    , ( 3, -1 )
    , ( -3, 0 )
    , ( -2, 0 )
    , ( -1, 0 )
    , ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( 3, 0 )
    , ( -3, 1 )
    , ( -2, 1 )
    , ( -1, 1 )
    , ( 0, 1 )
    , ( 1, 1 )
    , ( 2, 1 )
    , ( 3, 1 )
    , ( -2, 2 )
    , ( -1, 2 )
    , ( 0, 2 )
    , ( 1, 2 )
    , ( 2, 2 )
    , ( -1, 3 )
    , ( 0, 3 )
    , ( 1, 3 )
    ]
