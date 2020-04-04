module Decoders exposing (decodeEggInfo, decodeEggLoadedInfo, decodeFlags, decodeFullEggInfo)

import Json.Decode as D
import Json.Decode.Pipeline exposing (optional, required)
import Types exposing (EggInfo, EggLoadedInfo, Flags, FullEggInfo)


decodeEggInfo : D.Decoder EggInfo
decodeEggInfo =
    D.succeed EggInfo
        |> required "localId" D.int
        |> optional "key" (D.maybe D.string) Nothing
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


decodeEggLoadedInfo : D.Decoder EggLoadedInfo
decodeEggLoadedInfo =
    D.succeed EggLoadedInfo
        |> optional "eggInfo" (D.maybe decodeEggInfo) Nothing
        |> required "colors" (D.array D.string)
