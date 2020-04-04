module Encoders exposing (encodeFullEggInfo, encodeSaveEggAndListInfo, encodeUrlInfo)

import Json.Encode as E
import Types exposing (EggInfo, FullEggInfo, SaveEggAndListInfo, UrlInfo(..))


encodeEggInfo : EggInfo -> E.Value
encodeEggInfo eggInfo =
    E.object
        [ ( "localId", E.int eggInfo.localId )
        , ( "key", encodeMaybe E.string eggInfo.key )
        , ( "evidence", encodeMaybe E.string eggInfo.evidence )
        , ( "typeId", E.string eggInfo.typeId )
        , ( "palette", encodeMaybe (E.list E.string) eggInfo.palette )
        , ( "histogram", encodeMaybe (E.list E.string) eggInfo.histogram )
        , ( "local", E.bool eggInfo.local )
        , ( "title", E.string eggInfo.title )
        , ( "message", E.string eggInfo.message )
        ]


encodeFullEggInfo : FullEggInfo -> E.Value
encodeFullEggInfo { eggInfo, colors } =
    let
        colorsJson =
            E.array E.string colors
    in
    E.object [ ( "eggInfo", encodeEggInfo eggInfo ), ( "colors", colorsJson ) ]


encodeUrlInfo : UrlInfo -> E.Value
encodeUrlInfo urlInfo =
    case urlInfo of
        LocalUrl localId ->
            E.object [ ( "localId", E.int localId ) ]

        ImplicitUrl ->
            E.object []

        EditUrl key evidence ->
            E.object [ ( "key", E.string key ), ( "evidence", E.string evidence ) ]

        ShowUrl key ->
            E.object [ ( "key", E.string key ) ]


encodeSaveEggAndListInfo : SaveEggAndListInfo -> E.Value
encodeSaveEggAndListInfo info =
    E.object
        [ ( "localId", E.int info.localId )
        , ( "list", E.list encodeEggInfo info.list )
        , ( "colors", E.array E.string info.colors )
        ]


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe fn maybe =
    case maybe of
        Just v ->
            fn v

        Nothing ->
            E.null
