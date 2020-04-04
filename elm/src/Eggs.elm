module Eggs exposing (Area, EggTypeInfo, Point, hd, ld, sd, typeInfoForTypeId)


type alias Point =
    ( Int, Int )


type alias Area =
    { topLeft : Point
    , topRight : Point
    , bottomLeft : Point
    , bottomRight : Point
    }


type alias EggTypeInfo =
    { id : String
    , verticalSegments : Int
    , verticalCoefficients : List Float
    , verticalCoefficientPairs : List ( Float, Float )
    , layerBorders : List ( Int, Int )
    , layerBorderPairs : List ( ( Int, Int ), ( Int, Int ) )
    , layersCount : Int
    , areas : List (List Area)
    , polygonPoints : List (List String)
    }


coefficientsForSegmentCount : Int -> List Float
coefficientsForSegmentCount verticalSegments =
    let
        n =
            verticalSegments // 4

        angle =
            pi / 2 / toFloat n
    in
    List.range -n n |> List.map (\i -> sin <| toFloat i * angle)


pairAdjacents : List a -> List ( a, a )
pairAdjacents list =
    List.map2 (\a b -> ( a, b )) list <|
        Maybe.withDefault [] <|
            List.tail list


createAreas : List ( ( Int, Int ), ( Int, Int ) ) -> List ( Float, Float ) -> List (List Area)
createAreas layerBorderPairs verticalCoefficientPairs =
    let
        convertBorderPair ( ( w1, y1 ), ( w2, y2 ) ) =
            let
                toArea ( c1, c2 ) =
                    { topLeft = ( round <| c1 * toFloat w1, y1 )
                    , topRight = ( round <| c2 * toFloat w1, y1 )
                    , bottomRight = ( round <| c2 * toFloat w2, y2 )
                    , bottomLeft = ( round <| c1 * toFloat w2, y2 )
                    }
            in
            List.map toArea verticalCoefficientPairs
    in
    List.map convertBorderPair layerBorderPairs


createPolygonPoints : List (List Area) -> List (List String)
createPolygonPoints areas =
    let
        pointToStr ( x, y ) =
            String.fromInt x ++ "," ++ String.fromInt y

        areaToPoints { topLeft, topRight, bottomRight, bottomLeft } =
            String.concat
                [ pointToStr topLeft
                , " "
                , pointToStr topRight
                , " "
                , pointToStr bottomRight
                , " "
                , pointToStr bottomLeft
                ]

        mapAreaList list =
            List.map areaToPoints list
    in
    List.map mapAreaList areas


makeEggInfo : String -> Int -> List ( Int, Int ) -> EggTypeInfo
makeEggInfo id verticalSegments layerBorders =
    let
        verticalCoefficients =
            coefficientsForSegmentCount verticalSegments

        verticalCoefficientPairs =
            pairAdjacents verticalCoefficients

        layerBorderPairs : List ( ( Int, Int ), ( Int, Int ) )
        layerBorderPairs =
            pairAdjacents layerBorders

        layersCount : Int
        layersCount =
            List.length layerBorderPairs

        areas : List (List Area)
        areas =
            createAreas layerBorderPairs verticalCoefficientPairs

        polygonPoints : List (List String)
        polygonPoints =
            createPolygonPoints areas
    in
    { id = id
    , verticalSegments = verticalSegments
    , verticalCoefficients = verticalCoefficients
    , verticalCoefficientPairs = verticalCoefficientPairs
    , layerBorders = layerBorders
    , layerBorderPairs = layerBorderPairs
    , layersCount = layersCount
    , areas = areas
    , polygonPoints = polygonPoints
    }


ld : EggTypeInfo
ld =
    makeEggInfo "ld"
        32
        [ ( 29, 0 ) -- 1
        , ( 146, 60 ) -- 3
        , ( 207, 120 ) -- 5
        , ( 251, 180 ) -- 7
        , ( 280, 240 ) -- 9
        , ( 301, 300 ) -- 11
        , ( 315, 360 ) -- 13
        , ( 322, 420 ) -- 15
        , ( 322, 480 ) -- 17
        , ( 315, 540 ) -- 19
        , ( 300, 600 ) -- 21
        , ( 270, 660 ) -- 23
        , ( 222, 720 ) -- 25
        , ( 140, 780 ) -- 27
        , ( 30, 840 ) -- 28
        ]


sd : EggTypeInfo
sd =
    makeEggInfo "sd"
        64
        [ ( 29, 0 ) -- 1
        , ( 105, 30 ) -- 2
        , ( 146, 60 ) -- 3
        , ( 178, 90 ) -- 4
        , ( 207, 120 ) -- 5
        , ( 229, 150 ) -- 6
        , ( 251, 180 ) -- 7
        , ( 268, 210 ) -- 8
        , ( 280, 240 ) -- 9
        , ( 291, 270 ) -- 10
        , ( 301, 300 ) -- 11
        , ( 308, 330 ) -- 12
        , ( 315, 360 ) -- 13
        , ( 319, 390 ) -- 14
        , ( 322, 420 ) -- 15
        , ( 324, 450 ) -- 16
        , ( 322, 480 ) -- 17
        , ( 320, 510 ) -- 18
        , ( 315, 540 ) -- 19
        , ( 310, 570 ) -- 20
        , ( 300, 600 ) -- 21
        , ( 287, 630 ) -- 22
        , ( 270, 660 ) -- 23
        , ( 248, 690 ) -- 24
        , ( 222, 720 ) -- 25
        , ( 188, 750 ) -- 26
        , ( 140, 780 ) -- 27
        , ( 41, 810 ) -- 28
        ]


hd : EggTypeInfo
hd =
    makeEggInfo "hd"
        128
        [ ( 29, 0 ) -- 1
        , ( 105, 30 ) -- 2
        , ( 125, 45 ) -- 2
        , ( 146, 60 ) -- 3
        , ( 166, 75 ) -- 3
        , ( 178, 90 ) -- 4
        , ( 192, 105 ) -- 4
        , ( 207, 120 ) -- 5
        , ( 217, 135 ) -- 5
        , ( 229, 150 ) -- 6
        , ( 239, 165 ) -- 6
        , ( 251, 180 ) -- 7
        , ( 259, 195 ) -- 7
        , ( 268, 210 ) -- 8
        , ( 274, 225 ) -- 8
        , ( 280, 240 ) -- 9
        , ( 285, 255 ) -- 9
        , ( 291, 270 ) -- 10
        , ( 295, 285 ) -- 10
        , ( 301, 300 ) -- 11
        , ( 305, 315 ) -- 11
        , ( 308, 330 ) -- 12
        , ( 311, 345 ) -- 12
        , ( 315, 360 ) -- 13
        , ( 318, 375 ) -- 13
        , ( 319, 390 ) -- 14
        , ( 320, 405 ) -- 14
        , ( 322, 420 ) -- 15
        , ( 323, 435 ) -- 15
        , ( 324, 450 ) -- 16
        , ( 323, 465 ) -- 16
        , ( 322, 480 ) -- 17
        , ( 322, 495 ) -- 17
        , ( 320, 510 ) -- 18
        , ( 318, 525 ) -- 18
        , ( 315, 540 ) -- 19
        , ( 313, 555 ) -- 19
        , ( 310, 570 ) -- 20
        , ( 305, 585 ) -- 20
        , ( 300, 600 ) -- 21
        , ( 295, 615 ) -- 21
        , ( 287, 630 ) -- 22
        , ( 280, 645 ) -- 22
        , ( 270, 660 ) -- 23
        , ( 260, 675 ) -- 23
        , ( 248, 690 ) -- 24
        , ( 235, 705 ) -- 24
        , ( 222, 720 ) -- 25
        , ( 207, 735 ) -- 25
        , ( 188, 750 ) -- 26
        , ( 140, 780 ) -- 27
        , ( 41, 810 ) -- 28
        ]


typeInfoForTypeId : String -> EggTypeInfo
typeInfoForTypeId typeId =
    case typeId of
        "ld" ->
            ld

        "sd" ->
            sd

        "hd" ->
            hd

        _ ->
            sd
