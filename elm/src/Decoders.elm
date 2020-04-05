module Decoders exposing (decodeEggInfo, decodeFlags, decodeFullEggInfo, decodeLocalEggLoadedInfo, decodeRemoteEggLoadedInfo)

import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Types exposing (EggInfo, Flags, FullEggInfo, LocalEggLoadedInfo, RemoteEggLoadedInfo)


decodeEggInfo : D.Decoder EggInfo
decodeEggInfo =
    D.succeed EggInfo
        |> required "localId" D.int
        |> optional "key" (D.maybe D.string) Nothing
        |> optional "secret" (D.maybe D.string) Nothing
        |> optional "evidence" (D.maybe D.string) Nothing
        |> required "typeId" D.string
        |> optional "palette" (D.maybe <| D.list D.string) Nothing
        |> optional "histogram" (D.maybe <| D.list D.string) Nothing
        |> required "local" D.bool
        |> required "title" D.string
        |> required "message" D.string


decodeFullEggInfo : D.Decoder FullEggInfo
decodeFullEggInfo =
    D.succeed FullEggInfo
        |> required "eggInfo" decodeEggInfo
        |> required "colors" (D.array D.string)


decodeFlags : D.Decoder Flags
decodeFlags =
    D.succeed Flags
        |> required "list" (D.list decodeEggInfo)
        |> required "implicitLocalId" D.int


decodeLocalEggLoadedInfo : D.Decoder LocalEggLoadedInfo
decodeLocalEggLoadedInfo =
    D.succeed LocalEggLoadedInfo
        |> required "localId" D.int
        |> required "colors" (D.array D.string)


decodeRemoteEggLoadedInfo : D.Decoder RemoteEggLoadedInfo
decodeRemoteEggLoadedInfo =
    D.succeed RemoteEggLoadedInfo
        |> required "eggInfo" decodeEggInfo
        |> required "colors" (D.array D.string)
