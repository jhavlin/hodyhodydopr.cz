port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import CustomEvents exposing (onMouseDownWithButton, onMouseEnterWithButtons)
import Eggs exposing (Egg, sd)
import Heroicons.Solid as HIcons
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (attribute, class, href, id, style, type_)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseLeave, onMouseUp)
import ParseInt exposing (parseIntHex, toHex)
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
    , egg : Egg
    , rotation : Int
    , colors : Array String
    , rotating : Bool
    , currentColor : String
    , palette : List String
    , autoDrawing : Bool
    , pinnedSegment : Maybe Int
    , viewMode : ViewMode
    }


type ViewMode
    = Info
    | Edit
    | Show
    | List
    | Share


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init _ url key =
    let
        egg =
            Eggs.hd

        model =
            { key = key
            , url = url
            , rotation = 0
            , colors = initColorsArray egg
            , rotating = False
            , currentColor = "#dd5875"
            , palette = [ "#53b9e9", "#fd6617", "#dd5875", "#8a75ad", "#fffc3f", "#ffffff", "#000000" ]
            , autoDrawing = False
            , pinnedSegment = Nothing
            , egg = egg
            , viewMode = Edit
            }
    in
    ( model, initTouch () )


initColorsArray : Egg -> Array String
initColorsArray egg =
    Array.repeat (egg.layersCount * egg.verticalSegments) ""


toVisibleSegment : Model -> Int -> Int
toVisibleSegment model renderedSegmentIndex =
    remainderBy model.egg.verticalSegments <| renderedSegmentIndex + model.egg.verticalSegments + model.rotation



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

updatePalette : String -> List String -> List String
updatePalette color palette =
    if String.isEmpty color then
        palette

    else if List.member color palette then
        color :: List.filter (\c -> c /= color) palette

    else
        color :: List.take (List.length palette - 1) palette

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
            ( { model
                | colors = Array.set ((layer * model.egg.verticalSegments) + segment) color model.colors
                , palette = updatePalette color model.palette
              }
            , Cmd.none
            )

        Rotate direction ->
            ( { model | rotation = remainderBy model.egg.verticalSegments <| model.rotation + model.egg.verticalSegments + direction }
            , Cmd.none
            )

        SetRotation rotation ->
            ( { model | rotation = remainderBy model.egg.verticalSegments <| rotation + model.egg.verticalSegments }
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
                newRotation =
                    case model.pinnedSegment of
                        Just pinned ->
                            remainderBy model.egg.verticalSegments <| model.egg.verticalSegments + pinned - segment

                        Nothing ->
                            model.rotation
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
                | colors = Array.set ((layer * model.egg.verticalSegments) + visibleSegment) model.currentColor model.colors
                , palette = updatePalette model.currentColor model.palette
              }
            , Cmd.none
            )

        EggTouchMoved ( layer, segment ) ->
            let
                visibleSegment =
                    toVisibleSegment model segment
            in
            ( { model
                | colors = Array.set ((layer * model.egg.verticalSegments) + visibleSegment) model.currentColor model.colors
                , palette = updatePalette model.currentColor model.palette
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


adjustColor : String -> Float -> String
adjustColor hexColor coefficient =
    let
        limit n =
            min 255 <| max 0 <| n

        asHex n =
            String.padLeft 2 '0' <| toHex n

        clr =
            if String.length hexColor /= 7 then
                eggColor

            else
                hexColor

        origR =
            toFloat <| Result.withDefault 0 <| parseIntHex <| String.slice 1 3 clr

        origG =
            toFloat <| Result.withDefault 0 <| parseIntHex <| String.slice 3 5 clr

        origB =
            toFloat <| Result.withDefault 0 <| parseIntHex <| String.slice 5 7 clr

        newR =
            limit <| round <| origR * coefficient

        newG =
            limit <| round <| origG * coefficient

        newB =
            limit <| round <| origB * coefficient
    in
    String.concat [ "#", asHex newR, asHex newG, asHex newB ]


picture : Model -> Html Msg
picture model =
    let
        areaToShape layerIndex segmentIndex polygonPointsStr =
            let
                visibleSegment =
                    toVisibleSegment model segmentIndex

                fillStr =
                    Array.get ((layerIndex * model.egg.verticalSegments) + visibleSegment) model.colors |> Maybe.withDefault ""

                baseColor =
                    if String.isEmpty fillStr then
                        eggColor

                    else
                        fillStr

                color =
                    adjustColor baseColor <| 0.6 + (toFloat segmentIndex / toFloat model.egg.verticalSegments)
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
            List.concat <| List.indexedMap layerToShapes model.egg.polygonPoints
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
                    if 0 == remainderBy model.egg.verticalSegments (model.rotation + index + model.egg.verticalSegments - (model.egg.verticalSegments // 4)) then
                        "#000000"

                    else if 0 == remainderBy 8 (model.rotation + index) then
                        "#888888"

                    else
                        "#FFFFFF"
            in
            { rectX = c1 * 200
            , rectWidth = (c2 - c1) * 200
            , fillColor = fillColor
            , index = index
            }

        rectPositions =
            List.indexedMap coefficientsToRectPosition model.egg.verticalCoefficientPairs

        validRectPositions =
            List.filter (\i -> i.rectWidth > 1) rectPositions

        positionToRect { rectX, rectWidth, fillColor, index } =
            let
                visibleSegment =
                    toVisibleSegment model index

                color =
                    adjustColor fillColor <| 0.7 + (toFloat index / toFloat model.egg.verticalSegments)
            in
            rect
                [ x <| String.fromFloat rectX
                , y <| "-18"
                , width <| String.fromFloat rectWidth
                , height <| "36"
                , fill color
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
                , viewBox "-200 -20 400 40"
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


paletteView : Model -> Html Msg
paletteView model =
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
        , paletteView model
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
