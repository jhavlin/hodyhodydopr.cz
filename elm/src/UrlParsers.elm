module UrlParsers exposing (parseUrl)

import Types exposing (UrlInfo(..))
import Url exposing (Url)


parseUrl : Url -> UrlInfo
parseUrl url =
    let
        path =
            case url.fragment of
                Just fragment ->
                    String.split "/" fragment

                Nothing ->
                    []
    in
    case path of
        "moje" :: localIdStr :: [] ->
            case String.toInt localIdStr of
                Nothing ->
                    ImplicitUrl

                Just localId ->
                    LocalUrl localId

        "ukaz" :: key :: secret :: [] ->
            ShowUrl key secret

        _ ->
            ImplicitUrl
