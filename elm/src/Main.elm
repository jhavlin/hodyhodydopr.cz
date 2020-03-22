module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, text, button, input)
import Html.Attributes exposing (attribute, class, type_, style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, onMouseLeave, onInput)
import Svg exposing (svg, polygon)
import Svg.Attributes as SAttr exposing (stroke, fill, viewBox, width, points, strokeWidth)
import Time
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
    , rotation: Int
    , colors: Array String
    , autoRotating: Int
    , currentColor: String
    , palette: List String
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
    let
        model =
            { key = key
            , url = url
            , rotation = 0
            , colors = initColorsArray
            , autoRotating = 0
            , currentColor = "#000000"
            , palette = ["#53b9e9", "#fd6617", "#dd5875", "#8a75ad", "#fffc3f", "#ffffff", "#000000"]
            }
    in
    ( model, Cmd.none )


initColorsArray: Array String
initColorsArray = Array.repeat (layersCount * verticalSegments) ""


-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SetColor Int Int String
    | Rotate Int
    | SetAutoRotation Int
    | SetCurrentColor String


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

        SetColor layer segment color ->
            let
                newPalette =
                    if String.isEmpty color || List.member color model.palette then
                        model.palette
                    else
                        color :: List.take (List.length model.palette - 1) model.palette
            in
            ( { model
              | colors = Array.set ((layer * verticalSegments) + segment) color model.colors
              , palette = newPalette
              }
            , Cmd.none
            )

        Rotate direction ->
            ( { model | rotation = remainderBy verticalSegments <| model.rotation + verticalSegments + direction }
            , Cmd.none
            )

        SetAutoRotation direction ->
            ( { model | autoRotating = direction }
            , Cmd.none
            )

        SetCurrentColor color ->
            ( { model | currentColor = color }
            , Cmd.none
            )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.autoRotating /= 0 then
        Time.every 100.0 <| (\_ -> Rotate model.autoRotating)
    else
        Sub.none



-- VIEW

eggColor: String
eggColor = "#efb67f"


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
    , ( 178, 90 ) -- 4
    , ( 207, 120 ) -- 5
    , ( 229, 150 ) -- 6
    , ( 251, 180 ) -- 7
    , ( 268, 210 ) -- 8
    , ( 280, 240 ) -- 9
    , ( 291, 270 ) -- 10
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
    , ( 287, 630 ) -- 22
    , ( 270, 660 ) -- 23
    , ( 248, 690 ) -- 24
    , ( 222, 720 ) -- 25
    , ( 188, 750 ) -- 26
    , ( 140, 780 ) -- 27
    , ( 41, 810 ) -- 28
    ]


layerBorderPairs : List ( (Int, Int), (Int, Int) )
layerBorderPairs = List.map2 (\a b -> (a, b)) layerBorders
    <| Maybe.withDefault [] <| List.tail layerBorders


layersCount : Int
layersCount = List.length layerBorderPairs


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
picture model =
    let
        areaToShape layerIndex segmentIndex polygonPointsStr  =
            let
                visibleSegment = remainderBy verticalSegments <| segmentIndex + verticalSegments + model.rotation
                fillStr = Array.get ((layerIndex * verticalSegments) + visibleSegment) model.colors |> Maybe.withDefault ""
                color = if String.isEmpty fillStr
                    then eggColor
                    else fillStr
            in
            polygon
                [ points polygonPointsStr
                , fill color
                , stroke "#888888"
                , strokeWidth "0.1"
                , SAttr.class "egg-area"
                , onClick <| SetColor layerIndex visibleSegment model.currentColor
                ] []

        layerToShapes layerIndex pointsList = List.indexedMap
            (\i a ->areaToShape layerIndex i a)
            pointsList


        areaShapes = List.concat <| List.indexedMap layerToShapes polygonPoints
    in
    svg
        [ width "700"
        , viewBox "-350 -25 700 850"
        , SAttr.class "picture-egg"
        ]
        areaShapes

palette : Model -> Html Msg
palette model =
    let
        colorToItem color =
            div
                [ class "palette-color"
                , onClick <| SetCurrentColor color
                , style "background" color
                ] []

        eraseItem = div
                [ class "palette-color"
                , onClick <| SetCurrentColor ""
                , style "background" "white"
                ] [ text "☒" ]

        chooseItem = input [ type_ "color", class "palette-select", onInput SetCurrentColor] []

        colorItems = List.map colorToItem model.palette

        selected = if String.isEmpty model.currentColor then eggColor else model.currentColor
    in
    div [ class "palette", style "background" selected ] ((eraseItem :: colorItems) ++ [chooseItem])


view : Model -> Browser.Document Msg
view model =
    { title = "Bezkontaktní Velikonoce"
    , body =
        [ div [ class "notranslate", attribute "translate" "no" ]
            [ h1 [ class "site-title" ] [ text "Bezkontaktní Velikonoce" ]
            , div []
                [ button
                    [ onClick <| Rotate -1
                    , onMouseDown <| SetAutoRotation -1
                    , onMouseUp <| SetAutoRotation 0
                    , onMouseLeave <| SetAutoRotation 0] [text "<-"]
                , button
                    [ onClick <| Rotate 1
                    , onMouseDown <| SetAutoRotation 1
                    , onMouseUp <| SetAutoRotation 0
                    , onMouseLeave <| SetAutoRotation 0
                    ] [text "->"]
                ]
            , palette model
            , div [ class "picture-container" ] [picture model]
            ]
        ]
    }
