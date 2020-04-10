module Encoders exposing
    ( encodeDeleteEggInfo
    , encodeEggList
    , encodeFullEggInfo
    , encodeSaveEggAndListInfo
    , encodeSaveOnlineData
    , encodeUrlInfo
    )

import Array exposing (Array)
import Json.Encode as E
import Types exposing (DeleteEggInfo, EggInfo, FullEggInfo, SaveEggAndListInfo, UrlInfo(..))


encodeEggInfo : EggInfo -> E.Value
encodeEggInfo eggInfo =
    E.object
        [ ( "localId", E.int eggInfo.localId )
        , ( "key", encodeMaybe E.string eggInfo.key )
        , ( "secret", encodeMaybe E.string eggInfo.secret )
        , ( "evidence", encodeMaybe E.string eggInfo.evidence )
        , ( "onlineVersion", E.int eggInfo.onlineVersion )
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
            E.object
                [ ( "urlType", E.string "local" )
                , ( "localId", E.int localId )
                ]

        ImplicitUrl ->
            E.object
                [ ( "urlType", E.string "implicit" )
                ]

        ShowUrl key secret ->
            E.object
                [ ( "urlType", E.string "show" )
                , ( "key", E.string key )
                , ( "secret", E.string secret )
                ]


encodeEggList : List EggInfo -> E.Value
encodeEggList eggList =
    E.list encodeEggInfo eggList


encodeSaveEggAndListInfo : SaveEggAndListInfo -> E.Value
encodeSaveEggAndListInfo info =
    E.object
        [ ( "localId", E.int info.localId )
        , ( "list", E.list encodeEggInfo info.list )
        , ( "colors", E.array E.string info.colors )
        ]


encodeDeleteEggInfo : DeleteEggInfo -> E.Value
encodeDeleteEggInfo info =
    E.object
        [ ( "localId", E.int info.localId )
        , ( "list", E.list encodeEggInfo info.list )
        ]


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe fn maybe =
    case maybe of
        Just v ->
            fn v

        Nothing ->
            E.null


encodeSaveOnlineData : { colors : Array String, eggInfo : EggInfo } -> E.Value
encodeSaveOnlineData { colors, eggInfo } =
    E.object
        [ ( "eggInfo", encodeEggInfo eggInfo )
        , ( "colors", E.array E.string colors )
        ]
