module CustomEvents exposing (onClickStopping, onMouseDownWithButton, onMouseEnterWithButtons, onMouseUpWithButton)

import Html
import Html.Events exposing (custom, on)
import Json.Decode exposing (Decoder, field, int, map, succeed)


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


onClickStopping : msg -> Html.Attribute msg
onClickStopping message =
    custom "click" (map alwaysStop (succeed message))


alwaysStop : msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
alwaysStop message =
    { message = message
    , stopPropagation = True
    , preventDefault = True
    }
