port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import CustomEvents exposing (onMouseDownWithButton, onMouseEnterWithButtons)
import Decoders
import Eggs exposing (EggTypeInfo)
import Encoders
import Heroicons.Solid as HIcons
import Html exposing (Html, a, button, div, h1, h2, input, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, style, target, type_)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseLeave, onMouseUp)
import Html.Lazy as Lazy
import Json.Decode as Decode
import Json.Encode as Encode
import ParseInt exposing (parseIntHex, toHex)
import Svg exposing (polygon, rect, svg)
import Svg.Attributes as SAttr exposing (fill, height, points, stroke, strokeWidth, viewBox, width, x, y)
import Types exposing (EggInfo, UrlInfo(..), emptyEggInfo)
import Url
import UrlParsers


port initApp : Encode.Value -> Cmd msg



-- port saveList : Encode.Value -> Cmd msg


port saveEggAndList : Encode.Value -> Cmd msg


port loadEgg : Encode.Value -> Cmd msg



-- port saveLastLocalId : Encode.Value -> Cmd msg


port eggLoaded : (Decode.Value -> msg) -> Sub msg


port rotateBarTouchStarted : (Int -> msg) -> Sub msg


port rotateBarTouchMoved : (Int -> msg) -> Sub msg


port eggTouchStarted : (( Int, Int ) -> msg) -> Sub msg


port eggTouchMoved : (( Int, Int ) -> msg) -> Sub msg



-- MAIN


main : Program Decode.Value Model Msg
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
    , eggInfo : EggInfo
    , eggList : List EggInfo
    , currentEggType : EggTypeInfo
    , implicitLocalId : Int
    , rotation : Int
    , colors : Array String
    , rotating : Bool
    , currentColor : String
    , brush : Brush
    , palette : List String
    , autoDrawing : Bool
    , pinnedSegment : Maybe Int
    , viewMode : ViewMode
    , loadState : LoadState
    , time : Int
    }


type ViewMode
    = Info
    | Edit
    | Show
    | List
    | Share


type LoadState
    = Loading
    | Loaded


type Brush
    = B1
    | B2
    | B3
    | B4
    | B5


initialPalette : List String
initialPalette =
    [ "#53b9e9", "#fd6617", "#dd5875", "#8a75ad", "#fffc3f", "#ffffff", "#000000" ]


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init flagsJson url key =
    let
        decoded =
            Decode.decodeValue Decoders.decodeFlags flagsJson
                |> Result.withDefault { eggList = [], implicitLocalId = 0 }

        eggInfo =
            List.filter (\i -> i.localId == decoded.implicitLocalId) decoded.eggList
                |> List.head
                |> Maybe.withDefault emptyEggInfo

        eggType =
            Eggs.typeInfoForTypeId eggInfo.typeId

        urlInfo =
            UrlParsers.parseUrl url

        model =
            { key = key
            , url = url
            , rotation = 0
            , colors = initColorsArray eggType
            , rotating = False
            , currentColor = "#dd5875"
            , brush = B1
            , palette = Maybe.withDefault initialPalette eggInfo.palette
            , autoDrawing = False
            , pinnedSegment = Nothing
            , currentEggType = eggType
            , eggInfo = eggInfo
            , viewMode = Edit
            , loadState = Loading
            , eggList = decoded.eggList
            , implicitLocalId = decoded.implicitLocalId
            , time = 0
            }
    in
    ( model, initApp <| Encoders.encodeUrlInfo urlInfo )


initColorsArray : EggTypeInfo -> Array String
initColorsArray egg =
    Array.repeat (egg.layersCount * egg.verticalSegments) ""


toVisibleSegment : EggTypeInfo -> Int -> Int -> Int
toVisibleSegment egg rotation renderedSegmentIndex =
    remainderBy egg.verticalSegments <| renderedSegmentIndex + egg.verticalSegments + rotation



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Paint Int Int
    | SetRotating Bool
    | SetCurrentColor String
    | SetAutoDrawing Bool
    | PinSegment (Maybe Int)
    | SetRotation Int
    | RotateBarTouchStarted Int
    | RotateBarTouchMoved Int
    | RotateBarMouseEnteredSegment Int Int
    | EggTouchStarted ( Int, Int )
    | EggTouchMoved ( Int, Int )
    | EggMouseDownInSegment Int Int Int
    | EggMouseEnterInSegment Int Int Int
    | ToggleBrush
    | SetViewMode ViewMode
    | NewEgg String
    | EggLoaded Decode.Value
    | NoOp


updateEggList : Maybe EggInfo -> List EggInfo -> List EggInfo
updateEggList eggInfoOpt eggList =
    case eggInfoOpt of
        Just eggInfo ->
            eggInfo :: List.filter (\i -> i.localId /= eggInfo.localId) eggList

        Nothing ->
            eggList


updatePalette : String -> List String -> List String
updatePalette color palette =
    if String.isEmpty color then
        palette

    else if List.member color palette then
        color :: List.filter (\c -> c /= color) palette

    else
        color :: List.take (List.length palette - 1) palette


paint : Model -> Int -> Int -> { colors : Array String, palette : List String }
paint model layerIndex segmentIndex =
    let
        ( a, r ) =
            case model.brush of
                B1 ->
                    ( 0, 1 )

                B2 ->
                    ( 1, 1 )

                B3 ->
                    ( 2, 2 )

                B4 ->
                    ( 3, 2 )

                B5 ->
                    ( 4, 3 )

        nearSegments =
            List.range -a a

        nearLayers =
            List.range -a a

        segmentFn layerOffset segmentOffset colors =
            if
                layerIndex
                    + layerOffset
                    >= 0
                    && layerIndex
                    + layerOffset
                    < model.currentEggType.layersCount
                    && (abs layerOffset < r || abs segmentOffset < r)
            then
                let
                    actualSegment =
                        toVisibleSegment model.currentEggType model.rotation (segmentIndex + segmentOffset)

                    actualLayer =
                        layerIndex + layerOffset
                in
                Array.set ((actualLayer * model.currentEggType.verticalSegments) + actualSegment) model.currentColor colors

            else
                colors

        layerFn layerOffset colors =
            List.foldl (segmentFn layerOffset) colors nearSegments

        updatedColors =
            List.foldl layerFn model.colors nearLayers
    in
    { colors = updatedColors
    , palette = updatePalette model.currentColor model.palette
    }


setCurrentEggByLocalId : Url.Url -> Int -> Model -> Model
setCurrentEggByLocalId url localId model =
    let
        eggInfo =
            List.filter (\i -> i.localId == localId) model.eggList |> List.head |> Maybe.withDefault model.eggInfo

        eggType =
            Eggs.typeInfoForTypeId eggInfo.typeId

        colors =
            initColorsArray eggType
    in
    { model | url = url, eggInfo = eggInfo, currentEggType = eggType, colors = colors, viewMode = Edit, loadState = Loading }


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
            let
                urlInfo =
                    UrlParsers.parseUrl url
            in
            case urlInfo of
                ImplicitUrl ->
                    ( setCurrentEggByLocalId url model.implicitLocalId model, loadEgg <| Encoders.encodeUrlInfo urlInfo )

                LocalUrl localId ->
                    ( setCurrentEggByLocalId url localId model, loadEgg <| Encoders.encodeUrlInfo urlInfo )

                _ ->
                    ( { model | url = url }, Cmd.none )

        Paint layer segment ->
            let
                { colors, palette } =
                    paint model layer segment
            in
            ( { model
                | colors = colors
                , palette = palette
              }
            , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = model.eggInfo.localId }
            )

        SetRotation rotation ->
            ( { model | rotation = remainderBy model.currentEggType.verticalSegments <| rotation + model.currentEggType.verticalSegments }
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
                    toVisibleSegment model.currentEggType model.rotation segment
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
                            remainderBy model.currentEggType.verticalSegments <| model.currentEggType.verticalSegments + pinned - segment

                        Nothing ->
                            model.rotation
            in
            ( { model | rotation = newRotation }
            , Cmd.none
            )

        RotateBarMouseEnteredSegment index buttons ->
            if buttons > 0 && Maybe.withDefault -1 model.pinnedSegment >= 0 then
                ( { model | rotation = Maybe.withDefault 0 model.pinnedSegment - index }, Cmd.none )

            else
                ( model, Cmd.none )

        EggTouchStarted ( layerIndex, segmentIndex ) ->
            let
                { colors, palette } =
                    paint model layerIndex segmentIndex
            in
            ( { model | colors = colors, palette = palette }
            , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = model.eggInfo.localId }
            )

        EggTouchMoved ( layerIndex, segmentIndex ) ->
            let
                { colors, palette } =
                    paint model layerIndex segmentIndex
            in
            ( { model | colors = colors, palette = palette }
            , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = model.eggInfo.localId }
            )

        EggMouseDownInSegment layerIndex segmentIndex button ->
            let
                newModel =
                    if button == 0 then
                        let
                            { colors, palette } =
                                paint model layerIndex segmentIndex
                        in
                        { model | colors = colors, palette = palette }

                    else if button == 1 then
                        let
                            visibleSegment =
                                toVisibleSegment model.currentEggType model.rotation segmentIndex
                        in
                        { model | pinnedSegment = Just visibleSegment }

                    else
                        model
            in
            ( newModel, Cmd.none )

        EggMouseEnterInSegment layerIndex segmentIndex buttons ->
            if buttons == 1 && model.autoDrawing then
                let
                    { colors, palette } =
                        paint model layerIndex segmentIndex
                in
                ( { model | colors = colors, palette = palette }
                , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = model.eggInfo.localId }
                )

            else if buttons == 4 && Maybe.withDefault -1 model.pinnedSegment >= 0 then
                ( { model | rotation = Maybe.withDefault 0 model.pinnedSegment - segmentIndex }, Cmd.none )

            else
                ( model, Cmd.none )

        ToggleBrush ->
            let
                newBrush =
                    case model.brush of
                        B1 ->
                            B2

                        B2 ->
                            B3

                        B3 ->
                            B4

                        B4 ->
                            B5

                        B5 ->
                            B1
            in
            ( { model | brush = newBrush }, Cmd.none )

        SetViewMode mode ->
            ( { model | viewMode = mode }, Cmd.none )

        NewEgg resolution ->
            let
                eggType =
                    Eggs.typeInfoForTypeId resolution

                colors =
                    initColorsArray eggType

                localId =
                    1 + (Maybe.withDefault 1 <| List.maximum <| List.map .localId model.eggList)

                eggInfo =
                    { localId = localId
                    , key = Nothing
                    , evidence = Nothing
                    , typeId = eggType.id
                    , palette = Just initialPalette
                    , histogram = Nothing
                    , local = True
                    , title = ""
                    , message = ""
                    }

                eggList =
                    eggInfo :: model.eggList
            in
            ( { model | currentEggType = eggType, colors = colors, viewMode = Edit, eggList = eggList }
            , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { colors = colors, list = eggList, localId = localId }
            )

        EggLoaded jsonValue ->
            let
                decoded =
                    Decode.decodeValue Decoders.decodeEggLoadedInfo jsonValue
            in
            case decoded of
                Ok { eggInfoOpt, colors } ->
                    let
                        eggInfo =
                            Maybe.withDefault model.eggInfo eggInfoOpt

                        eggType =
                            Eggs.typeInfoForTypeId eggInfo.typeId
                    in
                    ( { model
                        | eggInfo = eggInfo
                        , eggList = updateEggList eggInfoOpt model.eggList
                        , currentEggType = eggType
                        , colors =
                            if Array.length colors == 0 then
                                initColorsArray eggType

                            else
                                colors
                        , loadState = Loaded
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ eggTouchStarted EggTouchStarted
        , eggTouchMoved EggTouchMoved
        , rotateBarTouchStarted RotateBarTouchStarted
        , rotateBarTouchMoved RotateBarTouchMoved
        , eggLoaded EggLoaded
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


pictureView : Model -> Html Msg
pictureView model =
    let
        areaToShape layerIndex segmentIndex polygonPointsStr =
            let
                visibleSegment =
                    toVisibleSegment model.currentEggType model.rotation segmentIndex

                fillStr =
                    Array.get ((layerIndex * model.currentEggType.verticalSegments) + visibleSegment) model.colors |> Maybe.withDefault ""

                baseColor =
                    if String.isEmpty fillStr then
                        eggColor

                    else
                        fillStr

                color =
                    adjustColor baseColor <| 0.6 + (toFloat segmentIndex / toFloat model.currentEggType.verticalSegments)
            in
            polygon
                [ points polygonPointsStr
                , fill color
                , stroke "#888888"
                , strokeWidth "0.1"
                , SAttr.class "egg-area"
                , attribute "data-layer-index" <| String.fromInt layerIndex
                , attribute "data-segment-index" <| String.fromInt segmentIndex
                , onClick <| Paint layerIndex segmentIndex
                , onMouseDownWithButton <| EggMouseDownInSegment layerIndex segmentIndex
                , onMouseEnterWithButtons <| EggMouseEnterInSegment layerIndex segmentIndex
                ]
                []

        layerToShapes layerIndex pointsList =
            List.indexedMap
                (\i a -> areaToShape layerIndex i a)
                pointsList

        areaShapes =
            List.concat <| List.indexedMap layerToShapes model.currentEggType.polygonPoints
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


rotateBarView : EggTypeInfo -> Int -> Html Msg
rotateBarView egg rotation =
    let
        coefficientsToRectPosition index ( c1, c2 ) =
            let
                fillColor =
                    if 0 == remainderBy egg.verticalSegments (rotation + index + egg.verticalSegments - (egg.verticalSegments // 4)) then
                        "#000000"

                    else if 0 == remainderBy 8 (rotation + index) then
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
            List.indexedMap coefficientsToRectPosition egg.verticalCoefficientPairs

        validRectPositions =
            List.filter (\i -> i.rectWidth > 1) rectPositions

        positionToRect { rectX, rectWidth, fillColor, index } =
            let
                visibleSegment =
                    toVisibleSegment egg rotation index

                color =
                    adjustColor fillColor <| 0.7 + (toFloat index / toFloat egg.verticalSegments)
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
                , onMouseEnterWithButtons <| RotateBarMouseEnteredSegment index
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


paletteView : String -> List String -> Html Msg
paletteView currentColor palette =
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
            List.map colorToItem palette

        selected =
            if String.isEmpty currentColor then
                eggColor

            else
                currentColor
    in
    div [ class "palette-outer", style "background" selected ]
        [ div [ class "palette-inner base-width" ] ((eraseItem :: colorItems) ++ [ chooseItem ])
        ]


brushSelectView : Brush -> String -> Html Msg
brushSelectView brush color =
    let
        size =
            case brush of
                B1 ->
                    "2"

                B2 ->
                    "4"

                B3 ->
                    "8"

                B4 ->
                    "10"

                B5 ->
                    "12"

        showColor =
            if String.length color == 7 then
                color

            else
                eggColor

        sizeCircle =
            Svg.circle
                [ SAttr.cx "0"
                , SAttr.cy "0"
                , SAttr.r size
                , fill showColor
                , strokeWidth "2"
                , SAttr.stroke <| adjustColor showColor 0.6
                ]
                []

        borderCircle =
            Svg.circle
                [ SAttr.cx "0"
                , SAttr.cy "0"
                , SAttr.r "14"
                , fill "none"
                , strokeWidth "2"
                , SAttr.stroke <| adjustColor showColor 0.6
                ]
                []
    in
    div
        [ class "brush-select"
        , onClick ToggleBrush
        ]
        [ svg
            [ width "40"
            , viewBox "-20 -20 40 40"
            , SAttr.class "brush-select-svg"
            ]
            [ sizeCircle, borderCircle ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Bezkontaktní Velikonoce"
    , body =
        [ div [ class "main-area notranslate", attribute "translate" "no" ]
            (viewMainArea model)
        , div [ class "main-menu notranslate", attribute "translate" "no" ]
            [ viewMainMenu model ]
        ]
    }


viewMainArea : Model -> List (Html Msg)
viewMainArea model =
    let
        others =
            case model.viewMode of
                Info ->
                    [ viewInfo model ]

                List ->
                    [ viewList model ]

                Share ->
                    [ viewShare model ]

                _ ->
                    []
    in
    viewEdit model :: others


viewMainMenu : Model -> Html Msg
viewMainMenu model =
    let
        selected mode =
            if mode == model.viewMode then
                " selected"

            else
                ""

        infoItem =
            div [ class <| "main-menu-item" ++ selected Info, onClick <| SetViewMode Info ]
                [ HIcons.informationCircle [ SAttr.class <| "main-menu-item-icon" ++ selected Info ] ]

        listItem =
            div [ class <| "main-menu-item" ++ selected List, onClick <| SetViewMode List ]
                [ HIcons.folder [ SAttr.class <| "main-menu-item-icon" ++ selected List ] ]

        editItem =
            div [ class <| "main-menu-item" ++ selected Edit, onClick <| SetViewMode Edit ]
                [ HIcons.pencil [ SAttr.class <| "main-menu-item-icon" ++ selected Edit ] ]

        shareItem =
            div [ class <| "main-menu-item" ++ selected Share, onClick <| SetViewMode Share ]
                [ HIcons.share [ SAttr.class <| "main-menu-item-icon" ++ selected Share ] ]
    in
    div [ class "main-menu-inner controls-width" ]
        [ infoItem, listItem, editItem, shareItem ]



--     [ infoItem, listItem, editItem, shareItem ]


viewEdit : Model -> Html Msg
viewEdit model =
    let
        ready =
            div [ class "view-edit notranslate", attribute "translate" "no" ]
                [ div [ class "picture-container base-width" ]
                    [ div [ class "picture-absolute" ]
                        [ pictureView model
                        , div [ class "picture-controls-anchor" ]
                            [ div [ class "picture-controls-line" ]
                                [ Lazy.lazy2 brushSelectView model.brush model.currentColor
                                ]
                            ]
                        ]
                    ]
                , Lazy.lazy2 rotateBarView model.currentEggType model.rotation
                , Lazy.lazy2 paletteView model.currentColor model.palette
                ]
    in
    case model.loadState of
        Loaded ->
            ready

        _ ->
            div [] []


viewInfo : Model -> Html msg
viewInfo model =
    let
        top =
            div [ class "view-info-top" ]
                [ h1 [] [ text "Bezkontaktní Velikonoce" ]
                , p [] [ text "[ Vývojová, nedokončená verze! ]" ]
                , p []
                    [ text "Navrhněte kraslici pro ty, které nemůžete podarovat osobně. "
                    ]
                , p []
                    [ text "Stačí vybrat barvy a nanést je na kybervajíčko. "
                    , text "Až budete se svým výtvorem spokojeni, klikněte na tlačítko pro sdílení (vpravo), "
                    , text "kraslici uložte, zkopírujte si URL adresu pro prohlížení a pošlete ji koledníkům."
                    ]
                , p [] [ text "Protože máme přestupný rok, mohou malovat i kluci." ]
                , p [] [ span [ class "larger" ] [ text "♥" ], text " ", text "Věnováno všem, co se starají." ]
                ]

        bottom =
            div
                [ class "view-info-bottom" ]
                [ p []
                    [ span [ class "copyleft" ] [ text " ©" ]
                    , text " 2020"
                    , text " "
                    , a [ href "https://primitiweb.cz", target "_new" ] [ text "primitiweb.cz" ]
                    ]
                ]
    in
    div [ class "view-info view-cover notranslate", attribute "translate" "no" ]
        [ div [ class "base-width" ]
            [ div [ class "view-info-outer" ]
                [ top
                , bottom
                ]
            ]
        ]


viewShow : Model -> Html msg
viewShow model =
    div [] []


viewList : Model -> Html Msg
viewList model =
    let
        idView id typeId =
            div [ class "egg-list-item-id" ] [ text <| String.fromInt id, span [ class "egg-type-id" ] [ text typeId ] ]

        titleView title =
            div [ class "egg-list-item-name" ]
                [ text <|
                    if String.isEmpty <| String.trim title then
                        "(bez názvu)"

                    else
                        title
                ]

        histogramColor color =
            div
                [ class "egg-list-item-histogram-color"
                , style "background"
                    (if String.isEmpty <| String.trim color then
                        eggColor

                     else
                        color
                    )
                ]
                []

        histogramView histogram =
            case histogram of
                Just colorList ->
                    div [ class "egg-list-item-histogram" ] (List.map histogramColor colorList)

                Nothing ->
                    div [ class "egg-list-item-histogram" ] [ histogramColor eggColor ]

        eggHref eggInfo =
            String.concat [ "#moje/", String.fromInt eggInfo.localId ]

        eggItemView eggInfo =
            li [ class "egg-list-item" ]
                [ a [ href <| eggHref eggInfo, class "egg-list-item-link" ]
                    [ idView eggInfo.localId eggInfo.typeId
                    , titleView eggInfo.title
                    , histogramView eggInfo.histogram
                    ]
                ]

        eggListView =
            ul [ class "egg-list" ] (List.map eggItemView model.eggList)
    in
    div [ class "view-info view-cover notranslate", attribute "translate" "no" ]
        [ div [ class "base-width" ]
            [ div [ class "view-info-outer" ]
                [ div [ class "view-info-top" ]
                    [ h1 [] [ text "Moje kraslice" ]
                    , h2 [] [ text "Nová kraslice" ]
                    , p [] [ text "Zvolte rozlišení:" ]
                    , ul []
                        [ li [] [ button [ class "resolution-button", onClick <| NewEgg "ld" ] [ text "Hrubé" ] ]
                        , li [] [ button [ class "resolution-button", onClick <| NewEgg "sd" ] [ text "Polohrubé" ] ]
                        , li [] [ button [ class "resolution-button", onClick <| NewEgg "hd" ] [ text "Hladké" ] ]
                        ]
                    , h2 [] [ text "Dříve otevřené kraslice" ]
                    , eggListView
                    ]
                ]
            ]
        ]


viewShare : Model -> Html msg
viewShare model =
    div [ class "view-info view-cover notranslate", attribute "translate" "no" ]
        [ div [ class "base-width" ]
            [ div [ class "view-info-outer" ]
                [ div [ class "view-info-top" ]
                    [ h1 [] [ text "Uložit & sdílet" ]
                    , p [] [ text "[ Vývojová, nedokončená verze! ]" ]
                    , p [] [ text "Ukládání a sdílení není ještě hotové :-(" ]
                    ]
                ]
            ]
        ]
