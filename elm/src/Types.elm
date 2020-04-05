module Types exposing
    ( EggInfo
    , Flags
    , FullEggInfo
    , LocalEggLoadedInfo
    , RemoteEggLoadedInfo
    , RenderData
    , SaveEggAndListInfo
    , UrlInfo(..)
    , emptyEggInfo
    )

import Array exposing (Array)
import Eggs


type alias EggInfo =
    { localId : Int
    , key : Maybe String
    , secret : Maybe String
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
    | ShowUrl String String
    | ImplicitUrl


type alias LocalEggLoadedInfo =
    { localId : Int
    , colors : Array String
    }


type alias RemoteEggLoadedInfo =
    { eggInfo : EggInfo
    , colors : Array String
    }


type alias SaveEggAndListInfo =
    { list : List EggInfo
    , colors : Array String
    , localId : Int
    }


type alias RenderData =
    { colors : Array String
    , eggType : Eggs.EggTypeInfo
    }


emptyEggInfo : EggInfo
emptyEggInfo =
    { localId = 1
    , key = Nothing
    , secret = Nothing
    , evidence = Nothing
    , typeId = "sd"
    , palette = Nothing
    , histogram = Nothing
    , local = True
    , title = ""
    , message = ""
    }
