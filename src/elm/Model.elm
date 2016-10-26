module Model exposing (..)

import Dict exposing (Dict)
import FNV
import Focus exposing (..)
import Math.Vector2 exposing (..)
import Mouse
import Foci exposing (..)
import Util exposing (..)


type alias Model =
    { processByID : ProcessDict
    , jackByID : JackDict
    , containerByID : ContainerDict
    , linkByID : LinkDict
    , epoch : Int
    , playing : Bool
    , jacksVisible : Bool
    , calcCache : CalcCache
    , drag : Maybe Drag
    , selected : Maybe Selectable
    , globalTransform : Transform
    }



----------------


type alias Transform =
    { translate : Vec2, scale : Float }


type alias ID =
    Int


type alias ProcessDict =
    Dict ID Process


type alias JackDict =
    Dict ID Jack


type alias LinkDict =
    Dict ID Link


type alias ContainerDict =
    Dict ID Container


type alias Process =
    { id : ID
    , name : String
    , description : String
    , imageURL : Maybe String
    , position : Vec2
    , rect : Rect
    }


type alias Link =
    { id : ID
    , containerID : ID
    , jackID : ID
    , textOffset : Float
    }


type alias Container =
    { id : ID
    , name : String
    , capacity : Float
    , initialAmount : Float
    , position : Vec2
    , radius : Float
    }


type alias Resource =
    { name : String
    , state : MatterState
    }


type alias Jack =
    { id : JackID
    , name : String
    , processID : ID
    , rate :
        -- the potential amount of flow, TODO: needs to be a function
        Float
    , units : String
    , direction : JackDirection
    , position : Vec2
    , rect : Rect
    , matterState : MatterState
    }


type alias Rect =
    ( Float, Float )


type alias Drag =
    { start : Mouse.Position
    , current : Mouse.Position
    , target : Draggable
    }


type Selectable
    = SelectedContainer ContainerID


type Draggable
    = DragProcess Process
    | DragJack Jack
    | DragContainer Container
    | DragScreen


type MatterState
    = SolidState
    | LiquidState
    | GasState
    | EnergyState
    | LightState
    | UnspecifiedState


type JackDirection
    = Input
    | Output


type alias ProcessID =
    ID


type alias JackID =
    ID


type alias ContainerID =
    ID


type alias LinkID =
    ID



-- Calc


type alias Calc =
    { jackByID : Dict JackID JackCalc
    , containerByID : Dict ContainerID ContainerCalc
    , linkByID :
        -- duplicate of what's in Model
        Dict LinkID Link
    , epoch : Int
    }


type alias CalcCache =
    Dict Int Calc


type alias JackCalc =
    { flow : Float }


type alias ContainerCalc =
    { amount : Float }


getProcessPosition : Model -> Process -> Vec2
getProcessPosition { drag } { id, position } =
    case drag of
        Nothing ->
            position

        Just drag ->
            case drag.target of
                DragProcess dragProcess ->
                    if dragProcess.id == id then
                        position `add` dragOffset drag
                    else
                        position

                _ ->
                    position


getJacksByProcessID : { a | jackByID : JackDict } -> ProcessID -> List Jack
getJacksByProcessID { jackByID } processID =
    jackByID
        |> Dict.values
        |> List.filter (.processID >> (==) processID)


getGetters { processByID, jackByID, containerByID, linkByID } =
    { getProcess = (flip seize) processByID
    , getJack = (flip seize) jackByID
    , getContainer = (flip seize) containerByID
    , getLink = (flip seize) linkByID
    }


getContainerPosition : Model -> Container -> Vec2
getContainerPosition { drag } { id, position } =
    case drag of
        Nothing ->
            position

        Just drag ->
            case drag.target of
                DragContainer dragContainer ->
                    if dragContainer.id == id then
                        position `add` dragOffset drag
                    else
                        position

                _ ->
                    position


getJackProcess : Model -> Jack -> Process
getJackProcess model jack =
    seize jack.processID model.processByID



-- TODO: make the (loose) jack position entirely dependent on process position


getJackPosition : Model -> Jack -> Vec2
getJackPosition { drag, processByID } { id, position, processID } =
    let
        process =
            seize processID processByID
    in
        case drag of
            Nothing ->
                position

            Just ({ start, current, target } as drag) ->
                let
                    offset =
                        dragOffset drag
                in
                    case target of
                        DragProcess dragProcess ->
                            if dragProcess.id == process.id then
                                position `add` offset
                            else
                                position

                        DragJack dragJack ->
                            if dragJack.id == id then
                                position `add` offset
                            else
                                position

                        _ ->
                            position


getJackFlow : Jack -> Float
getJackFlow jack =
    case jack.direction of
        Input ->
            -jack.rate

        Output ->
            jack.rate


getGlobalTransform : Model -> Transform
getGlobalTransform model =
    case model.drag of
        Nothing ->
            model.globalTransform

        Just ({ target } as drag) ->
            case target of
                DragScreen ->
                    update (globalTransform => translate) (add <| dragOffset drag) model
                        |> .globalTransform

                _ ->
                    model.globalTransform


getSun model =
    model.containerByID |> Dict.values |> List.filter (\c -> c.name == "Sun") |> List.head |> fromJust "no sun"



-- helpers ------------------------------------------
--dragOffset : { a | current : Mouse.Position, start : Mouse.Position } -> Vec2


dragOffset { current, start } =
    vec2 (toFloat <| current.x - start.x) (toFloat <| current.y - start.y)



-- IDs


generateProcessID name =
    -- TODO: prevent collision
    110000000000 + FNV.hashString name


generateJackID processName jackName jackDirection =
    220000000000 + FNV.hashString (processName ++ jackName ++ (toString jackDirection))


generateContainerID name =
    -- TODO: prevent collision
    440000000000 + FNV.hashString name
