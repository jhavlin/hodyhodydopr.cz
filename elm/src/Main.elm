port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import CustomEvents exposing (onMouseDownWithButton, onMouseEnterWithButtons)
import Heroicons.Solid as HIcons
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (attribute, class, href, id, style, type_)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseLeave, onMouseUp)
import Svg exposing (polygon, rect, svg)
import Svg.Attributes as SAttr exposing (fill, height, points, stroke, strokeWidth, viewBox, width, x, y)
import Url


port initTouch : () -> Cmd msg


port rotateBarTouchStarted : (Int -> msg) -> Sub msg


port rotateBarTouchMoved : (Int -> msg) -> Sub msg


port eggTouchStarted : (( Int, Int ) -> msg) -> Sub msg


port eggTouchMoved : (( Int, Int ) -> msg) -> Sub msg



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
    , rotation : Int
    , colors : Array String
    , rotating : Bool
    , currentColor : String
    , palette : List String
    , autoDrawing : Bool
    , pinnedSegment : Maybe Int
    , viewMode : ViewMode
    }


type alias Point =
    ( Int, Int )


type ViewMode
    = Info
    | Edit
    | Show
    | List
    | Share


type alias Area =
    { topLeft : Point
    , topRight : Point
    , bottomLeft : Point
    , bottomRight : Point
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init _ url key =
    let
        model =
            { key = key
            , url = url
            , rotation = 0
            , colors = initColorsArray
            , rotating = False
            , currentColor = "#000000"
            , palette = [ "#53b9e9", "#fd6617", "#dd5875", "#8a75ad", "#fffc3f", "#ffffff", "#000000" ]
            , autoDrawing = False
            , pinnedSegment = Nothing
            , viewMode = Edit
            }
    in
    ( model, initTouch () )


initColorsArray : Array String
initColorsArray =
    Array.repeat (layersCount * verticalSegments) ""


toVisibleSegment : Model -> Int -> Int
toVisibleSegment model renderedSegmentIndex =
    remainderBy verticalSegments <| renderedSegmentIndex + verticalSegments + model.rotation



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | SetColor Int Int String
    | Rotate Int
    | SetRotating Bool
    | SetCurrentColor String
    | SetAutoDrawing Bool
    | PinSegment (Maybe Int)
    | SetRotation Int
    | RotateBarTouchStarted Int
    | RotateBarTouchMoved Int
    | EggTouchStarted ( Int, Int )
    | EggTouchMoved ( Int, Int )
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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

        SetRotation rotation ->
            ( { model | rotation = remainderBy verticalSegments <| rotation + verticalSegments }
            , Cmd.none
            )

        SetRotating rotating ->
            ( { model | rotating = rotating }
            , Cmd.none
            )

        SetCurrentColor color ->
            ( { model | currentColor = color }
            , Cmd.none
            )

        SetAutoDrawing drawing ->
            ( { model | autoDrawing = drawing }
            , Cmd.none
            )

        PinSegment maybeSegment ->
            ( { model | pinnedSegment = maybeSegment }
            , Cmd.none
            )

        RotateBarTouchStarted segment ->
            let
                visibleSegment =
                    toVisibleSegment model segment
            in
            ( { model
                | pinnedSegment = Just visibleSegment
              }
            , Cmd.none
            )

        RotateBarTouchMoved segment ->
            let
                newRotation = case model.pinnedSegment of
                    Just pinned -> remainderBy verticalSegments <| verticalSegments + pinned - segment
                    Nothing -> model.rotation
            in
            ( { model | rotation = newRotation }
            , Cmd.none
            )

        EggTouchStarted ( layer, segment ) ->
            let
                visibleSegment =
                    toVisibleSegment model segment
            in
            ( { model
                | colors = Array.set ((layer * verticalSegments) + visibleSegment) model.currentColor model.colors
              }
            , Cmd.none
            )

        EggTouchMoved ( layer, segment ) ->
            let
                visibleSegment =
                    toVisibleSegment model segment
            in
            ( { model
                | colors = Array.set ((layer * verticalSegments) + visibleSegment) model.currentColor model.colors
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ eggTouchStarted EggTouchStarted
        , eggTouchMoved EggTouchMoved
        , rotateBarTouchStarted RotateBarTouchStarted
        , rotateBarTouchMoved RotateBarTouchMoved
        ]



-- VIEW


eggColor : String
eggColor =
    "#efb67f"


verticalSegments : Int
verticalSegments =
    64


verticalCoefficients : List Float
verticalCoefficients =
    let
        n =
            verticalSegments // 4

        angle =
            pi / 2 / toFloat n
    in
    List.range -n n |> List.map (\i -> sin <| toFloat i * angle)


verticalCoefficientPairs : List ( Float, Float )
verticalCoefficientPairs =
    List.map2 (\a b -> ( a, b )) verticalCoefficients <|
        Maybe.withDefault [] <|
            List.tail verticalCoefficients


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


layerBorderPairs : List ( ( Int, Int ), ( Int, Int ) )
layerBorderPairs =
    List.map2 (\a b -> ( a, b )) layerBorders <|
        Maybe.withDefault [] <|
            List.tail layerBorders


layersCount : Int
layersCount =
    List.length layerBorderPairs


areas : List (List Area)
areas =
    let
        convertBorderPair ( ( w1, y1 ), ( w2, y2 ) ) =
            let
                toArea ( c1, c2 ) =
                    { topLeft = ( round <| c1 * toFloat w1, y1 )
                    , topRight = ( round <| c2 * toFloat w1, y1 )
                    , bottomRight = ( round <| c2 * toFloat w2, y2 )
                    , bottomLeft = ( round <| c1 * toFloat w2, y2 )
                    }
            in
            List.map toArea verticalCoefficientPairs
    in
    List.map convertBorderPair layerBorderPairs


polygonPoints : List (List String)
polygonPoints =
    let
        pointToStr ( x, y ) =
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

        mapAreaList list =
            List.map areaToPoints list
    in
    List.map mapAreaList areas


picture : Model -> Html Msg
picture model =
    let
        areaToShape layerIndex segmentIndex polygonPointsStr =
            let
                visibleSegment =
                    toVisibleSegment model segmentIndex

                fillStr =
                    Array.get ((layerIndex * verticalSegments) + visibleSegment) model.colors |> Maybe.withDefault ""

                color =
                    if String.isEmpty fillStr then
                        eggColor

                    else
                        fillStr
            in
            polygon
                [ points polygonPointsStr
                , fill color
                , stroke "#888888"
                , strokeWidth "0.1"
                , SAttr.class "egg-area"
                , attribute "data-layer-index" <| String.fromInt layerIndex
                , attribute "data-segment-index" <| String.fromInt segmentIndex
                , onClick <| SetColor layerIndex visibleSegment model.currentColor
                , onMouseDownWithButton <|
                    \b ->
                        if b == 0 then
                            SetColor layerIndex visibleSegment model.currentColor

                        else if b == 1 then
                            PinSegment <| Just visibleSegment

                        else
                            NoOp
                , onMouseEnterWithButtons <|
                    \bs ->
                        if bs == 1 && model.autoDrawing then
                            SetColor layerIndex visibleSegment model.currentColor

                        else if bs == 4 && Maybe.withDefault -1 model.pinnedSegment >= 0 then
                            SetRotation <| Maybe.withDefault 0 model.pinnedSegment - segmentIndex

                        else
                            NoOp
                ]
                []

        layerToShapes layerIndex pointsList =
            List.indexedMap
                (\i a -> areaToShape layerIndex i a)
                pointsList

        areaShapes =
            List.concat <| List.indexedMap layerToShapes polygonPoints
    in
    svg
        [ width "700"
        , viewBox "-350 -25 700 850"
        , SAttr.class "picture-egg"
        , SAttr.id "picture-egg"
        , onMouseDown <| SetAutoDrawing True
        , onMouseUp <| SetAutoDrawing False
        , onMouseLeave <| SetAutoDrawing False
        ]
        areaShapes


rotateBar : Model -> Html Msg
rotateBar model =
    let
        coefficientsToRectPosition index ( c1, c2 ) =
            let
                fillColor =
                    if 0 == remainderBy verticalSegments (model.rotation + index + verticalSegments - (verticalSegments // 4)) then
                        "black"

                    else if 0 == remainderBy 8 (model.rotation + index) then
                        "gray"

                    else
                        "white"
            in
            { rectX = c1 * 200
            , rectWidth = (c2 - c1) * 200
            , fillColor = fillColor
            , index = index
            }

        rectPositions =
            List.indexedMap coefficientsToRectPosition verticalCoefficientPairs

        validRectPositions =
            List.filter (\i -> i.rectWidth > 1) rectPositions

        positionToRect { rectX, rectWidth, fillColor, index } =
            let
                visibleSegment =
                    toVisibleSegment model index
            in
            rect
                [ x <| String.fromFloat rectX
                , y <| String.fromFloat <| -18 + (rectWidth / 3)
                , width <| String.fromFloat rectWidth
                , height <| "36"
                , fill fillColor
                , stroke "gray"
                , strokeWidth "1"
                , attribute "data-segment-index" <| String.fromInt index
                , onMouseDown <| PinSegment <| Just visibleSegment
                , onMouseEnterWithButtons <|
                    \b ->
                        if b > 0 && Maybe.withDefault -1 model.pinnedSegment >= 0 then
                            SetRotation <| Maybe.withDefault 0 model.pinnedSegment - index

                        else
                            NoOp
                ]
                []

        rects =
            List.map positionToRect validRectPositions

        bar =
            svg
                [ width "400"
                , viewBox "-200 -25 400 50"
                , SAttr.class "rotate-bar-svg"
                , onMouseDown <| SetRotating True
                , onMouseUp <| SetRotating False
                , onMouseLeave <| SetRotating False
                ]
                rects
    in
    div
        [ class "rotate-bar controls-width"
        , id "rotate-bar"
        ]
        [ bar ]


palette : Model -> Html Msg
palette model =
    let
        colorToItem color =
            div
                [ class "palette-color"
                , onClick <| SetCurrentColor color
                , style "background" color
                ]
                []

        eraseItem =
            div
                [ class "palette-color"
                , onClick <| SetCurrentColor ""
                , style "background" "white"
                ]
                [ text "☒" ]

        chooseItem =
            input [ type_ "color", class "palette-select", onInput SetCurrentColor ] []

        colorItems =
            List.map colorToItem model.palette

        selected =
            if String.isEmpty model.currentColor then
                eggColor

            else
                model.currentColor
    in
    div [ class "palette-outer", style "background" selected ]
        [ div [ class "palette-inner base-width" ] ((eraseItem :: colorItems) ++ [ chooseItem ])
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Bezkontaktní Velikonoce"
    , body =
        [ div [ class "main-area notranslate", attribute "translate" "no" ]
            [ viewMainArea model ]
        , div [ class "main-menu notranslate", attribute "translate" "no" ]
            [ viewMainMenu model ]
        ]
    }


viewMainArea : Model -> Html Msg
viewMainArea model =
    viewEdit model


viewMainMenu : Model -> Html Msg
viewMainMenu model =
    let
        infoItem =
            a [ href "#", class "main-menu-item" ]
                [ HIcons.informationCircle [ SAttr.class "main-menu-item-icon" ] ]

        listItem =
            a [ href "#", class "main-menu-item" ]
                [ HIcons.folder [ SAttr.class "main-menu-item-icon" ] ]

        editItem =
            a [ href "#", class "main-menu-item selected" ]
                [ HIcons.pencil [ SAttr.class "main-menu-item-icon selected" ] ]

        shareItem =
            a [ href "#", class "main-menu-item" ]
                [ HIcons.share [ SAttr.class "main-menu-item-icon" ] ]
    in
    div [ class "main-menu-inner controls-width" ]
        [ infoItem, listItem, editItem, shareItem ]



--     [ infoItem, listItem, editItem, shareItem ]


viewEdit : Model -> Html Msg
viewEdit model =
    div [ class "view-edit notranslate", attribute "translate" "no" ]
        [ div [ class "picture-container base-width" ] [ div [ class "picture-absolute" ] [ picture model ] ]
        , rotateBar model
        , palette model
        ]


viewShow : Model -> Html msg
viewShow model =
    div [] []


viewList : Model -> Html msg
viewList model =
    div [] []


viewInfo : Model -> Html msg
viewInfo model =
    div [] []


viewShare : Model -> Html msg
viewShare model =
    div [] []
