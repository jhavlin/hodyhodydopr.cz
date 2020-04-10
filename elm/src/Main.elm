port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import CustomEvents exposing (onMouseDownWithButton, onMouseEnterWithButtons)
import Debug
import Decoders
import Eggs exposing (EggTypeInfo)
import Encoders
import Heroicons.Solid as HIcons
import Html exposing (Html, a, button, div, h1, h2, input, li, p, span, text, textarea, ul)
import Html.Attributes exposing (attribute, class, href, id, readonly, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseDown, onMouseLeave, onMouseUp)
import Html.Lazy as Lazy
import Json.Decode as Decode
import Json.Encode as Encode
import ParseInt exposing (parseIntHex, toHex)
import Svg exposing (polygon, rect, svg)
import Svg.Attributes as SAttr exposing (fill, height, points, stroke, strokeWidth, viewBox, width, x, y)
import Types exposing (EggInfo, RenderData, UrlInfo(..), emptyEggInfo)
import Url
import UrlParsers


port initApp : Encode.Value -> Cmd msg


port initTouch : () -> Cmd msg


port saveList : Encode.Value -> Cmd msg


port saveEggAndList : Encode.Value -> Cmd msg


port loadEgg : Encode.Value -> Cmd msg


port saveOnline : Encode.Value -> Cmd msg


port copyToClipboard : String -> Cmd msg



-- port saveLastLocalId : Encode.Value -> Cmd msg


port localEggLoaded : (Decode.Value -> msg) -> Sub msg


port remoteEggLoaded : (Decode.Value -> msg) -> Sub msg


port rotateBarTouchStarted : (Int -> msg) -> Sub msg


port rotateBarTouchMoved : (Int -> msg) -> Sub msg


port eggTouchStarted : (( Int, Int ) -> msg) -> Sub msg


port eggTouchMoved : (( Int, Int ) -> msg) -> Sub msg


port savedOnline : (Decode.Value -> msg) -> Sub msg



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
    , baseUrl : String
    , urlInfo : UrlInfo
    , eggList : List EggInfo
    , eggData : EggData
    , implicitLocalId : Int
    , rotation : Int
    , rotating : Bool
    , currentColor : String
    , brush : Brush
    , autoDrawing : Bool
    , pinnedSegment : Maybe Int
    , viewMode : ViewMode
    , time : Int
    , saveState : SaveState
    }


type ViewMode
    = Picture
    | Info
    | Share
    | List


type EggData
    = Loading
    | Local { localId : Int, renderData : RenderData }
    | Remote { renderData : RenderData, title : String, message : String }


type Brush
    = B1
    | B2
    | B3
    | B4
    | B5


type SaveState
    = None
    | Saving


initialPalette : List String
initialPalette =
    [ "#53b9e9", "#fd6617", "#dd5875", "#8a75ad", "#fffc3f", "#ffffff", "#000000" ]


asBaseUrl : Url.Url -> String
asBaseUrl url =
    let
        lastSlash =
            url.path |> String.indices "/" |> List.reverse |> List.head

        basePath =
            case lastSlash of
                Just index ->
                    String.slice 0 (index + 1) url.path

                Nothing ->
                    url.path
    in
    String.concat
        [ case url.protocol of
            Url.Http ->
                "http://"

            Url.Https ->
                "https://"
        , url.host
        , Maybe.withDefault "" <| Maybe.map (\p -> ":" ++ String.fromInt p) url.port_
        , basePath
        ]


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init flagsJson url key =
    let
        decoded =
            Decode.decodeValue Decoders.decodeFlags flagsJson
                |> Result.withDefault { eggList = [], implicitLocalId = 0 }

        urlInfo =
            UrlParsers.parseUrl url

        eggData =
            Loading

        model =
            { key = key
            , baseUrl = asBaseUrl url
            , urlInfo = urlInfo
            , rotation = 0
            , rotating = False
            , currentColor = "#dd5875"
            , brush = B1
            , autoDrawing = False
            , pinnedSegment = Nothing
            , viewMode = Picture
            , eggList = reorderEggList urlInfo decoded.implicitLocalId decoded.eggList
            , eggData = eggData
            , implicitLocalId = decoded.implicitLocalId
            , time = 0
            , saveState = None
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
    | LocalEggLoaded Decode.Value
    | RemoteEggLoaded Decode.Value
    | SetTitle String
    | SetMessage String
    | NoOp
    | SaveOnline
    | SavedOnline Decode.Value
    | CopyToClipboard String


updateEggList : Maybe EggInfo -> List EggInfo -> List EggInfo
updateEggList eggInfoOpt eggList =
    case eggInfoOpt of
        Just info ->
            info :: List.filter (\i -> i.localId /= info.localId) eggList

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


currentEggInfo : Model -> EggInfo
currentEggInfo model =
    Maybe.withDefault emptyEggInfo <| List.head model.eggList


updateCurrentEggInfo : (EggInfo -> EggInfo) -> List EggInfo -> List EggInfo
updateCurrentEggInfo fn eggList =
    let
        first =
            Maybe.withDefault emptyEggInfo <| List.head eggList
    in
    fn first :: (Maybe.withDefault [] <| List.tail eggList)


currentPalette : Model -> List String
currentPalette model =
    Maybe.withDefault initialPalette (currentEggInfo model).palette


paint : Model -> RenderData -> List String -> Int -> Int -> { colors : Array String, palette : List String, histogram : List String }
paint model { colors, eggType } palette layerIndex segmentIndex =
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

        segmentFn layerOffset segmentOffset clrs =
            if
                layerIndex
                    + layerOffset
                    >= 0
                    && layerIndex
                    + layerOffset
                    < eggType.layersCount
                    && (abs layerOffset < r || abs segmentOffset < r)
            then
                let
                    actualSegment =
                        toVisibleSegment eggType model.rotation (segmentIndex + segmentOffset)

                    actualLayer =
                        layerIndex + layerOffset
                in
                Array.set ((actualLayer * eggType.verticalSegments) + actualSegment) model.currentColor clrs

            else
                clrs

        layerFn layerOffset clrs =
            List.foldl (segmentFn layerOffset) clrs nearSegments

        updatedColors =
            List.foldl layerFn colors nearLayers

        step =
            Array.length updatedColors // 10

        -- not an actual histogram yet, rather samples of colors
        histogram =
            List.range 0 9 |> List.map (\i -> i * step) |> List.map (\i -> Array.get i updatedColors) |> List.map (Maybe.withDefault "")
    in
    { colors = updatedColors
    , palette = updatePalette model.currentColor palette
    , histogram = histogram
    }


updateModelAfterPaint : Model -> { colors : Array String, palette : List String, histogram : List String } -> Model
updateModelAfterPaint model { colors, palette, histogram } =
    case model.eggData of
        Local { localId, renderData } ->
            { model
                | eggList = updateCurrentEggInfo (\i -> { i | palette = Just palette, histogram = Just histogram }) model.eggList
                , eggData = Local { localId = localId, renderData = { renderData | colors = colors } }
            }

        _ ->
            model


getRenderData : Model -> Maybe RenderData
getRenderData model =
    case model.eggData of
        Local { renderData } ->
            Just renderData

        Remote { renderData } ->
            Just renderData

        _ ->
            Nothing


reorderEggList : UrlInfo -> Int -> List EggInfo -> List EggInfo
reorderEggList urlInfo implicitLocalId list =
    let
        moveToStart localId =
            case List.partition (\i -> i.localId == localId) list of
                ( a, b ) ->
                    a ++ b
    in
    case urlInfo of
        LocalUrl localId ->
            moveToStart localId

        ImplicitUrl ->
            moveToStart implicitLocalId

        _ ->
            list


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

                eggList =
                    reorderEggList urlInfo model.implicitLocalId model.eggList
            in
            ( { model
                | eggData = Loading
                , eggList = eggList
                , viewMode = Picture
                , rotation = 0
              }
            , loadEgg <| Encoders.encodeUrlInfo urlInfo
            )

        Paint layer segment ->
            case model.eggData of
                Local { localId, renderData } ->
                    let
                        { colors, palette, histogram } =
                            paint model renderData (currentPalette model) layer segment
                    in
                    ( updateModelAfterPaint model { colors = colors, palette = palette, histogram = histogram }
                    , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = localId }
                    )

                _ ->
                    ( model, Cmd.none )

        SetRotation rotation ->
            let
                verticalSegments =
                    case model.eggData of
                        Local { renderData } ->
                            renderData.eggType.verticalSegments

                        Remote { renderData } ->
                            renderData.eggType.verticalSegments

                        _ ->
                            0
            in
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
                visibleSegment renderData =
                    toVisibleSegment renderData.eggType model.rotation segment

                pinnedSegment =
                    case model.eggData of
                        Local { renderData } ->
                            Just <| visibleSegment renderData

                        Remote { renderData } ->
                            Just <| visibleSegment renderData

                        _ ->
                            Nothing
            in
            ( { model
                | pinnedSegment = pinnedSegment
              }
            , Cmd.none
            )

        RotateBarTouchMoved segment ->
            let
                verticalSegments =
                    case model.eggData of
                        Local { renderData } ->
                            renderData.eggType.verticalSegments

                        Remote { renderData } ->
                            renderData.eggType.verticalSegments

                        _ ->
                            0

                newRotation =
                    case model.pinnedSegment of
                        Just pinned ->
                            remainderBy verticalSegments <| verticalSegments + pinned - segment

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
            case model.eggData of
                Local { localId, renderData } ->
                    let
                        { colors, palette, histogram } =
                            paint model renderData (currentPalette model) layerIndex segmentIndex
                    in
                    ( updateModelAfterPaint model { colors = colors, palette = palette, histogram = histogram }
                    , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = localId }
                    )

                _ ->
                    ( model, Cmd.none )

        EggTouchMoved ( layerIndex, segmentIndex ) ->
            case model.eggData of
                Local { localId, renderData } ->
                    let
                        { colors, palette, histogram } =
                            paint model renderData (currentPalette model) layerIndex segmentIndex
                    in
                    ( updateModelAfterPaint model { colors = colors, palette = palette, histogram = histogram }
                    , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = localId }
                    )

                _ ->
                    ( model, Cmd.none )

        EggMouseDownInSegment layerIndex segmentIndex button ->
            case model.eggData of
                Local { localId, renderData } ->
                    if button == 0 then
                        let
                            { colors, palette, histogram } =
                                paint model renderData (currentPalette model) layerIndex segmentIndex
                        in
                        ( updateModelAfterPaint model { colors = colors, palette = palette, histogram = histogram }
                        , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = localId }
                        )

                    else if button == 1 then
                        let
                            visibleSegment =
                                toVisibleSegment renderData.eggType model.rotation segmentIndex
                        in
                        ( { model | pinnedSegment = Just visibleSegment }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Remote { renderData } ->
                    let
                        visibleSegment =
                            toVisibleSegment renderData.eggType model.rotation segmentIndex
                    in
                    ( { model | pinnedSegment = Just visibleSegment }, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        EggMouseEnterInSegment layerIndex segmentIndex buttons ->
            case model.eggData of
                Local { localId, renderData } ->
                    if buttons == 1 && model.autoDrawing then
                        let
                            { colors, palette, histogram } =
                                paint model renderData (currentPalette model) layerIndex segmentIndex
                        in
                        ( updateModelAfterPaint model { colors = colors, palette = palette, histogram = histogram }
                        , saveEggAndList <| Encoders.encodeSaveEggAndListInfo { list = model.eggList, colors = colors, localId = localId }
                        )

                    else if buttons == 4 && Maybe.withDefault -1 model.pinnedSegment >= 0 then
                        ( { model | rotation = Maybe.withDefault 0 model.pinnedSegment - segmentIndex }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Remote _ ->
                    if buttons > 0 && Maybe.withDefault -1 model.pinnedSegment >= 0 then
                        ( { model | rotation = Maybe.withDefault 0 model.pinnedSegment - segmentIndex }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Loading ->
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
                    , secret = Nothing
                    , evidence = Nothing
                    , onlineVersion = 0
                    , typeId = eggType.id
                    , palette = Just initialPalette
                    , histogram = Nothing
                    , local = True
                    , title = ""
                    , message = ""
                    }

                eggList =
                    eggInfo :: model.eggList

                eggData =
                    Local { localId = localId, renderData = { colors = colors, eggType = eggType } }
            in
            ( { model
                | eggList = eggList
                , eggData = eggData
                , viewMode = Picture
              }
            , Cmd.batch
                [ saveEggAndList <| Encoders.encodeSaveEggAndListInfo { colors = colors, list = eggList, localId = localId }
                , Nav.pushUrl model.key (String.concat [ "#moje/", String.fromInt localId ])
                ]
            )

        LocalEggLoaded jsonValue ->
            let
                decoded =
                    Decode.decodeValue Decoders.decodeLocalEggLoadedInfo jsonValue
            in
            case ( decoded, List.head model.eggList ) of
                ( Ok { localId, colors }, Just eggInfo ) ->
                    if eggInfo.localId == localId then
                        let
                            eggType =
                                Eggs.typeInfoForTypeId eggInfo.typeId

                            validColors =
                                if Array.length colors == 0 then
                                    initColorsArray eggType

                                else
                                    colors
                        in
                        ( { model
                            | eggData = Local { localId = localId, renderData = { colors = validColors, eggType = eggType } }
                            , rotation = 1
                            , viewMode = Picture
                          }
                        , initTouch ()
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RemoteEggLoaded jsonValue ->
            let
                decodeResult =
                    Decode.decodeValue Decoders.decodeRemoteEggLoadedInfo jsonValue
            in
            case decodeResult of
                Ok { colors, typeId, title, message } ->
                    let
                        eggType =
                            Eggs.typeInfoForTypeId typeId

                        eggData =
                            Remote { renderData = { colors = colors, eggType = eggType }, title = title, message = message }
                    in
                    ( { model | eggData = eggData }, initTouch () )

                _ ->
                    ( model, Cmd.none )

        SetTitle title ->
            let
                eggList =
                    updateCurrentEggInfo (\i -> { i | title = title }) model.eggList
            in
            case model.eggData of
                Local _ ->
                    ( { model | eggList = eggList }, saveList <| Encoders.encodeEggList eggList )

                _ ->
                    ( model, Cmd.none )

        SetMessage message ->
            let
                eggList =
                    updateCurrentEggInfo (\i -> { i | message = message }) model.eggList
            in
            case model.eggData of
                Local _ ->
                    ( { model | eggList = eggList }, saveList <| Encoders.encodeEggList eggList )

                _ ->
                    ( model, Cmd.none )

        SaveOnline ->
            case model.eggData of
                Local { renderData } ->
                    let
                        commandData =
                            Encoders.encodeSaveOnlineData
                                { colors = renderData.colors
                                , eggInfo = currentEggInfo model
                                }
                    in
                    ( { model | saveState = Saving }, saveOnline commandData )

                _ ->
                    ( model, Cmd.none )

        SavedOnline jsonValue ->
            let
                decoded =
                    Decode.decodeValue Decoders.decodeSavedOnlineData jsonValue
            in
            case decoded of
                Ok { key, secret, evidence, onlineVersion, localId } ->
                    if localId == (currentEggInfo model).localId then
                        let
                            updateInfo eggInfo =
                                { eggInfo
                                    | key = Just key
                                    , secret = Just secret
                                    , evidence = Just evidence
                                    , onlineVersion = onlineVersion
                                }

                            eggList =
                                updateCurrentEggInfo updateInfo model.eggList
                        in
                        ( { model | eggList = eggList, saveState = None }, saveList <| Encoders.encodeEggList eggList )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CopyToClipboard text ->
            ( model, copyToClipboard text )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ eggTouchStarted EggTouchStarted
        , eggTouchMoved EggTouchMoved
        , rotateBarTouchStarted RotateBarTouchStarted
        , rotateBarTouchMoved RotateBarTouchMoved
        , localEggLoaded LocalEggLoaded
        , remoteEggLoaded RemoteEggLoaded
        , savedOnline SavedOnline
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


eggView : Int -> Array String -> EggTypeInfo -> Html Msg
eggView rotation colors eggType =
    let
        areaToShape layerIndex segmentIndex polygonPointsStr =
            let
                visibleSegment =
                    toVisibleSegment eggType rotation segmentIndex

                fillStr =
                    Array.get ((layerIndex * eggType.verticalSegments) + visibleSegment) colors |> Maybe.withDefault ""

                baseColor =
                    if String.isEmpty fillStr then
                        eggColor

                    else
                        fillStr

                color =
                    adjustColor baseColor <| 0.6 + (toFloat segmentIndex / toFloat eggType.verticalSegments)
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
            List.concat <| List.indexedMap layerToShapes eggType.polygonPoints
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
    viewPicture model :: others


viewMainMenu : Model -> Html Msg
viewMainMenu model =
    let
        selected mode =
            if mode == model.viewMode then
                " selected"

            else
                ""

        toggle mode =
            if mode == model.viewMode then
                SetViewMode Picture

            else
                SetViewMode mode

        editItem =
            div [ class <| "main-menu-item" ++ selected Picture, onClick <| SetViewMode Picture ]
                [ HIcons.pencil [ SAttr.class <| "main-menu-item-icon" ++ selected Picture ] ]

        infoItem =
            div [ class <| "main-menu-item" ++ selected Info, onClick <| toggle Info ]
                [ HIcons.informationCircle [ SAttr.class <| "main-menu-item-icon" ++ selected Info ] ]

        listItem =
            div [ class <| "main-menu-item" ++ selected List, onClick <| toggle List ]
                [ HIcons.folder [ SAttr.class <| "main-menu-item-icon" ++ selected List ] ]

        shareItem =
            div [ class <| "main-menu-item" ++ selected Share, onClick <| toggle Share ]
                [ HIcons.share [ SAttr.class <| "main-menu-item-icon" ++ selected Share ] ]
    in
    div [ class "main-menu-inner controls-width" ]
        [ editItem, listItem, infoItem, shareItem ]



--     [ infoItem, listItem, editItem, shareItem ]


viewPicture : Model -> Html Msg
viewPicture model =
    let
        ready renderData inPictureControls bottomControls =
            div [ class "view-edit notranslate", attribute "translate" "no" ]
                ([ div [ class "picture-container base-width" ]
                    [ div [ class "picture-absolute" ]
                        [ eggView model.rotation renderData.colors renderData.eggType
                        , div [ class "picture-controls-anchor" ]
                            [ div [ class "picture-controls-line" ]
                                inPictureControls
                            ]
                        ]
                    ]
                 , Lazy.lazy2 rotateBarView renderData.eggType model.rotation
                 ]
                    ++ bottomControls
                )
    in
    case model.eggData of
        Local { renderData } ->
            ready renderData
                [ Lazy.lazy2 brushSelectView model.brush model.currentColor ]
                [ Lazy.lazy2 paletteView model.currentColor (currentPalette model) ]

        Remote { renderData, title } ->
            ready renderData [] [ div [ class "picture-bottom-title" ] [ text title ] ]

        Loading ->
            div [] []


viewInfo : Model -> Html msg
viewInfo _ =
    let
        top =
            div [ class "view-info-top" ]
                [ h1 [] [ text "Bezkontaktní Velikonoce" ]
                , p []
                    [ text "Navrhněte kraslici pro ty, které nemůžete podarovat osobně. "
                    ]
                , p []
                    [ text "Stačí vybrat barvy a nanést je na kybervajíčko. "
                    , text "Až budete se svým výtvorem spokojeni, klikněte na tlačítko pro sdílení (vpravo), "
                    , text "kraslici uložte, zkopírujte si URL adresu pro prohlížení a pošlete ji koledníkům."
                    ]
                , p [] [ text "Protože máme přestupný rok, mohou malovat i kluci." ]
                , p []
                    [ span [ class "larger", style "color" "red" ]
                        [ text "♥" ]
                    , text " Věnováno všem, co se starají."
                    ]
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
                        [ li []
                            [ button [ class "resolution-button", onClick <| NewEgg "sd" ]
                                [ HIcons.plus [ SAttr.class "egg-list-add-icon" ]
                                , text "Polohrubé"
                                , span [ class "egg-type-id" ] [ text "sd" ]
                                ]
                            ]
                        , li []
                            [ button [ class "resolution-button", onClick <| NewEgg "ld" ]
                                [ HIcons.plus [ SAttr.class "egg-list-add-icon" ]
                                , text "Hrubé"
                                , span [ class "egg-type-id" ] [ text "ld" ]
                                ]
                            ]
                        , li []
                            [ button [ class "resolution-button", onClick <| NewEgg "hd" ]
                                [ HIcons.plus [ SAttr.class "egg-list-add-icon" ]
                                , text "Hladké"
                                , span [ class "egg-type-id" ] [ text "hd" ]
                                ]
                            ]
                        ]
                    , h2 [] [ text "Ošatka" ]
                    , eggListView
                    ]
                ]
            ]
        ]


viewShare : Model -> Html Msg
viewShare model =
    let
        eggInfo =
            currentEggInfo model

        shareUrl key secret =
            String.concat [ model.baseUrl, "koleda.html#ukaz/", key, "/", secret ]

        sharingInfo key secret =
            div []
                [ div [] [ text "Zkopírujte níže uvedený odkaz a pošlete jej koledníkům, třeba e-mailem." ]
                , div [ class "share-url-line" ]
                    [ input [ class "share-url-input", readonly True, value <| shareUrl key secret ] []
                    , button [ class "share-url-button", onClick <| CopyToClipboard <| shareUrl key secret ]
                        [ HIcons.clipboardCopy [ SAttr.class "share-icon-clipboard" ] ]
                    ]
                ]

        content =
            case model.eggData of
                Local _ ->
                    [ h1 [] [ text "Uložit & sdílet" ]
                    , div [ class "share-box share-tip-simple" ]
                        [ p []
                            [ HIcons.lightBulb [ SAttr.class "share-icon" ]
                            , text "Pokud Vám stačí pohled na vajíčko z\u{00A0}jedné strany, můžete jednoduše získat snímek obrazovky "
                            , text "a rozeslat ho třeba e-mailem či chatovací aplikací nebo zveřejnit na sociálních sítích. "
                            ]
                        , p [ class "smaller" ]
                            [ text "(Nezapomeňte však odstranit případné citlivé údaje, které by se mohly na snímku vyskytovat.)"
                            ]
                        ]
                    , p []
                        [ text "Pojmenujte vajíčko, případně připojte vzkaz, "
                        , text "uložte vajíčko na internetu tlačítkem níže a zašlete koledníkům vytvořený odkaz."
                        ]
                    , div [ class "share-label" ] [ text "Název" ]
                    , input [ class "share-input", type_ "text", onInput SetTitle, value eggInfo.title ] []
                    , div [ class "share-label" ] [ text "Vzkaz" ]
                    , textarea [ class "share-input", onInput SetMessage, value eggInfo.message ] []
                    , button [ class "share-button", onClick SaveOnline ]
                        [ text <|
                            case model.saveState of
                                None ->
                                    "Uložit"

                                Saving ->
                                    "Ukládám"
                        ]
                    , case ( eggInfo.key, eggInfo.secret ) of
                        ( Just key, Just secret ) ->
                            sharingInfo key secret

                        _ ->
                            div [] []
                    , div [ class "share-box share-legal" ]
                        [ p []
                            [ HIcons.questionMarkCircle [ SAttr.class "share-icon" ]
                            , text "Veškerá data jsou na serveru uložena v zašifrované formě a provozovat služby jen nemůže číst. "
                            , text "Žádné uživatelské údaje nejsou zpracovávány ani zaznamenávány. "
                            , text "Uložená vajíčka mohou být po Velikonocích smazána. Dostupnost služby není garantována. "
                            , text "Použití služby je na vlastní nebezpečí. "
                            ]
                        ]
                    ]

                Remote { title, message } ->
                    [ h1 [] [ text "Informace o kraslici" ]
                    , div [ class "share-label" ] [ text "Název:" ]
                    , p [] [ text title ]
                    , div [ class "share-label" ] [ text "Vzkaz:" ]
                    , p [] [ text message ]
                    ]

                _ ->
                    []
    in
    div [ class "view-info view-cover notranslate", attribute "translate" "no" ]
        [ div [ class "base-width" ]
            [ div [ class "view-info-outer" ]
                [ div [ class "view-info-top" ]
                    content
                ]
            ]
        ]
