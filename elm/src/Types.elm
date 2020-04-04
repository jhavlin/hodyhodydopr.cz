module Types exposing (EggInfo, EggLoadedInfo, Flags, FullEggInfo, SaveEggAndListInfo, UrlInfo(..), emptyEggInfo)

import Array exposing (Array)


type alias EggInfo =
    { localId : Int
    , key : Maybe String
    , evidence : Maybe String
    , typeId : String
    , palette : Maybe (List String)
    , histogram : Maybe (List String)
    , local : Bool
    , title : String
    , message : String
    }


type alias FullEggInfo =
    { eggInfo : EggInfo
    , colors : Array String
    }


type alias Flags =
    { eggList : List EggInfo
    , implicitLocalId : Int
    }


type UrlInfo
    = LocalUrl Int
    | EditUrl String String
    | ShowUrl String
    | ImplicitUrl


type alias EggLoadedInfo =
    { eggInfoOpt : Maybe EggInfo
    , colors : Array String
    }


type alias SaveEggAndListInfo =
    { list : List EggInfo
    , colors : Array String
    , localId : Int
    }


emptyEggInfo : EggInfo
emptyEggInfo =
    { localId = 1
    , key = Nothing
    , evidence = Nothing
    , typeId = "sd"
    , palette = Nothing
    , histogram = Nothing
    , local = True
    , title = ""
    , message = ""
    }
