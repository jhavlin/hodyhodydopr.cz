module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (attribute, class)
import Svg exposing (circle, line, svg, polygon)
import Svg.Attributes as SAttr exposing (cx, cy, height, r, stroke, fill, viewBox, width, x1, x2, y1, y2, points, strokeWidth)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


type alias Point = (Int, Int)

type alias Area =
    { topLeft: Point
    , topRight: Point
    , bottomLeft: Point
    , bottomRight: Point
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Bezkontaktní Velikonoce"
    , body =
        [ div [ class "notranslate", attribute "translate" "no" ]
            [ h1 [ class "site-title" ] [ text "Bezkontaktní Velikonoce" ]
            , picture model
            ]
        ]
    }


verticalSegments : Int
verticalSegments = 64


verticalCoefficients: List Float
verticalCoefficients =
    let
        n = verticalSegments // 4
        angle = pi / 2 / toFloat n
    in
        List.range -n n |> List.map (\i -> sin <| toFloat i * angle)

verticalCoefficientPairs: List (Float, Float)
verticalCoefficientPairs = List.map2 (\a b -> (a, b)) verticalCoefficients
    <| Maybe.withDefault [] <| List.tail verticalCoefficients

layerBorders : List ( Int, Int )
layerBorders =
    [ ( 29, 0 ) -- 1
    , ( 105, 30 ) -- 2
    , ( 146, 60 ) -- 3
    , ( 175, 90 ) -- 4
    , ( 207, 120 ) -- 5
    , ( 229, 150 ) -- 6
    , ( 251, 180 ) -- 7
    , ( 265, 210 ) -- 8
    , ( 280, 240 ) -- 9
    , ( 289, 270 ) -- 10
    , ( 301, 300 ) -- 11
    , ( 308, 330 ) -- 12
    , ( 315, 360 ) -- 13
    , ( 319, 390 ) -- 14
    , ( 322, 420 ) -- 15
    , ( 324, 450 ) -- 16
    , ( 322, 480 ) -- 17
    , ( 320, 510 ) -- 18
    , ( 315, 540 ) -- 19
    , ( 310, 570 ) -- 20
    , ( 300, 600 ) -- 21
    , ( 285, 630 ) -- 22
    , ( 270, 660 ) -- 23
    , ( 248, 690 ) -- 24
    , ( 222, 720 ) -- 25
    , ( 188, 750 ) -- 26
    , ( 140, 780 ) -- 27
    , ( 41, 810 ) -- 28
    ]


layerBorderPairs: List ( (Int, Int), (Int, Int) )
layerBorderPairs = List.map2 (\a b -> (a, b)) layerBorders
    <| Maybe.withDefault [] <| List.tail layerBorders


areas: List (List Area)
areas =
    let
        convertBorderPair ( (w1, y1), (w2, y2) ) =
            let
                toArea (c1, c2) =
                    { topLeft = (round <| c1 * toFloat w1, y1)
                    , topRight = (round <| c2 * toFloat w1, y1)
                    , bottomRight = (round <| c2 * toFloat w2, y2)
                    , bottomLeft = (round <| c1 * toFloat w2, y2)
                    }
            in
            List.map toArea verticalCoefficientPairs

    in
    List.map convertBorderPair layerBorderPairs


polygonPoints : List (List String)
polygonPoints =
    let
        pointToStr (x, y) =
            String.fromInt x ++ "," ++ String.fromInt y

        areaToPoints { topLeft, topRight, bottomRight, bottomLeft } =
            String.concat
                [ pointToStr topLeft
                , " "
                , pointToStr topRight
                , " "
                , pointToStr bottomRight
                , " "
                , pointToStr bottomLeft
                ]

        mapAreaList list = List.map areaToPoints list
    in
        List.map mapAreaList areas


picture : Model -> Html Msg
picture _ =
    let
        -- pointToLine ( x, y ) =
        --     line
        --         [ x1 <| String.fromInt -x
        --         , y1 <| String.fromInt y
        --         , x2 <| String.fromInt x
        --         , y2 <| String.fromInt y
        --         , stroke "black" ]
        --         []

        -- pointToDots ( x, y ) =
        --     verticalCoefficients |> List.map (\c -> circle
        --         [ r "1"
        --         , cx <| String.fromInt <| round -( toFloat x * c)
        --         , cy <| String.fromInt y ]
        --         []
        --         )

        areaToShape layerIndex segmentIndex polygonPointsStr  =
            let
                fillStr = if remainderBy 2 (layerIndex + segmentIndex) == 0 then "yellow" else "black"
            in
            polygon [ points polygonPointsStr, fill fillStr, stroke "#888888", strokeWidth "0.1" ] []

        layerToShapes layerIndex pointsList = List.indexedMap (\i a -> areaToShape layerIndex i a) pointsList


        areaShapes = List.concat <| List.indexedMap layerToShapes polygonPoints
    in
    svg
        [ width "700"
        , height "850"
        , viewBox "-350 -25 700 850"
        , SAttr.class "picture-egg"
        ]
        areaShapes
