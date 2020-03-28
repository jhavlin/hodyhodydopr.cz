module CustomEvents exposing (onMouseDownWithButton, onMouseEnterWithButtons, onMouseUpWithButton)

import Html
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, field, int, map)


withButtonDecoder : (Int -> msg) -> Decoder msg
withButtonDecoder message =
    map message (field "button" int)


withButtonsDecoder : (Int -> msg) -> Decoder msg
withButtonsDecoder message =
    map message (field "buttons" int)


onMouseDownWithButton : (Int -> msg) -> Html.Attribute msg
onMouseDownWithButton message =
    on "mousedown" <| withButtonDecoder message


onMouseUpWithButton : (Int -> msg) -> Html.Attribute msg
onMouseUpWithButton message =
    on "mouseup" <| withButtonDecoder message


onMouseEnterWithButtons : (Int -> msg) -> Html.Attribute msg
onMouseEnterWithButtons message =
    on "mouseenter" <| withButtonsDecoder message
