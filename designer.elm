import Debug exposing (log)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)

import Dict exposing (Dict)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { processByID : ProcessDict
  , portByID : Dict ID Port
  --, flowById : Dict ID Flow

  -- ui
  , drag : Maybe Drag
  }


type alias ID = Int

type alias ProcessDict = Dict ID Process

--type alias Positioned a =
--  { a | position : Position
--  }

type alias Process =
    { id : ID
    , name : String
    , description : String
    , imageURL : Maybe String
    , position : Position
    }

type alias Flow =
  { source : Process
  , dest : Process
  , via : Resource
  }

type alias Resource =
  { name : String
  , state : MatterState
  }

type Draggable = DragProcess Process | DragPort Port

type MatterState = Solid | Liquid | Gas | Plasma

type PortDirection = Input | Output

type alias Port =
  { id : ID
  , name : String
  , processID : ID
  , rate : Float
  , direction : PortDirection
  , position : Position
  }

type alias Drag =
  { start : Position
  , current : Position
  , target : Draggable
  }


toDictByID : List { a | id : ID } -> Dict ID { a | id : ID }
toDictByID seq =
  seq
  |> List.indexedMap (\i data -> (i, { data | id = i }))
  |> Dict.fromList

seize : comparable -> Dict comparable b -> b
seize v d =
  case Dict.get v d of
    Nothing -> (Debug.crash <| "Failed lookup: " ++ (toString v) ++ " of " ++ (toString d))
    Just ret -> ret

mkProcess {name, position} = Process 0 name "" Nothing position

mkPort processByID {name, processID, direction} =
  let
    process = case Dict.get processID processByID of
      Just process -> process
      Nothing -> (Debug.crash (toString processID))
    position = addPos process.position <| Position 50 50
  in Port 0 name processID 42.0 direction position

init : ( Model, Cmd Msg )
init =
  let
    processByID : ProcessDict
    processByID =
      [ { name = "Biodigester", position = Position 100 100 }
      , { name = "Can of Beans", position = Position 300 400 }
      , { name = "Can of Beans", position = Position 500 200 }
      ] |> (List.map mkProcess) |> toDictByID
    portByID : Dict ID Port
    portByID =
      [ { name = "Effluent", processID = 1, direction = Input }
      , { name = "Biogas", processID = 2, direction = Output }
      ] |> (List.map (mkPort processByID)) |> toDictByID
    flowById = [] |> toDictByID
  in (
    Model
      processByID
      portByID
      --Dict.empty
      Nothing
     , Cmd.none )


-- UPDATE


type Msg
    = DragStart Draggable Position
    | DragAt Position
    | DragEnd Position

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateMulti : List comparable -> (Maybe a -> Maybe a) -> Dict comparable a -> Dict comparable a
updateMulti keys f dict =
  List.foldl
    (\k d -> Dict.update k f d)
    dict
    keys

getPorts : Model -> Process -> List Port
getPorts {portByID} process =
  portByID
  |> Dict.values
  |> List.filter (\x -> x.processID == process.id)

updateHelp : Msg -> Model -> Model
updateHelp msg ({processByID, portByID, drag} as model) =
  case msg of
    DragStart target xy ->
      { model | drag = (Just (Drag xy xy target)) }

    DragAt xy ->
      { model | drag = (Maybe.map (\{start, target} -> Drag start xy target) model.drag) }

    DragEnd _ ->
      case drag of
        Nothing -> Debug.crash "not gonna happen"
        Just drag ->
          case drag.target of
            DragProcess dragProcess ->
              let
                updatePosition process =
                  { process | position = getProcessPosition model process }
                updatePortPosition flowport =
                  { flowport | position = getPortPosition model flowport }
                attachedPorts = getPorts model dragProcess
              in
                { model
                | processByID = processByID |> Dict.update dragProcess.id (Maybe.map updatePosition)
                , portByID = portByID |> updateMulti (List.map .id attachedPorts) (Maybe.map updatePortPosition)
                , drag = Nothing }
            DragPort dragPort ->
              let
                updatePosition flowport =
                  { flowport | position = getPortPosition model flowport }
              in
                { model
                | portByID = portByID |> Dict.update dragPort.id (Maybe.map updatePosition)
                , drag = Nothing }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    dragging =
      case model.drag of
        Nothing ->
          Sub.none

        Just _ ->
          Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
  in
    Sub.batch [ dragging ]


-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  let
    drawProcess : Process -> Svg Msg
    drawProcess process =
      let
        backgroundColor = "blue"
        realPosition = getProcessPosition model process
        cx = realPosition.x
        cy = realPosition.y
        w = 150
        h = floor(150 * 1.9)
      in
        rect
          [ onMouseDown' <| DragProcess process
          , x (cx - w // 2 |> toString)
          , y (cy - h // 2 |> toString)
          , width (w |> toString)
          , height (h |> toString)
          , stroke backgroundColor
          , fill "white"
          , Html.Attributes.style
              [ "cursor" => "move"
              ]
          ]
          []

    drawPort : Port -> Svg Msg
    drawPort flowport =
      let
        process = seize flowport.processID model.processByID
        portPosition = getPortPosition model flowport
      in
        circle
          [ cx (portPosition.x |> toString)
          , cy (portPosition.y |> toString)
          , r "20"
          , onMouseDown' <| DragPort flowport
          ] []
    --drawLink : Model -> Flow -> Svg Msg
    --drawLink {processByID} {source, dest} =
    --  let
    --    a = toString source.position.x
    --    b = toString source.position.y
    --    c = toString dest.position.x
    --    d = toString dest.position.y
    --  in
    --    line
    --      [ x1 a
    --      , y1 b
    --      , x2 c
    --      , y2 d
    --      , stroke "black" ]
    --      []
  in
    Svg.svg
      [ width "100%"
      , height "100%"
      ]
      [
        Svg.g [] (model.processByID |> Dict.values |> List.map drawProcess)
      , Svg.g [] (model.portByID |> Dict.values |> List.map drawPort)
      ]



px : Int -> String
px number =
  toString number ++ "px"


getDraggableID : Draggable -> String
getDraggableID draggable =
  case draggable of
    DragProcess p -> "process-" ++ (toString p.id)
    DragPort p -> "port-" ++ (toString p.id)

dragOffset {current, start} =
  Position
    (current.x - start.x)
    (current.y - start.y)

getProcessPosition : Model -> Process -> Position
getProcessPosition {drag} {id, position} =
    case drag of
      Nothing ->
        position

      Just {start, current, target} ->
        case target of
          DragProcess dragProcess ->
            if dragProcess.id == id
            then
              Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)
            else
              position
          _ -> position

addPos p1 p2 = Position (p1.x + p2.x) (p1.y + p2.y)

getPortPosition : Model -> Port -> Position
getPortPosition {drag, processByID} {id, position, processID} =
  let
    process = seize processID processByID
  in
    case drag of
      Nothing ->
        position

      Just ({start, current, target} as drag) ->
        let offset = dragOffset drag
        in
          case target of
            DragProcess dragProcess ->
              if dragProcess.id == process.id
              then addPos position offset
              else position
            DragPort dragPort ->
              if dragPort.id == id
              then addPos position offset
              else position


onMouseDown' : Draggable -> Attribute Msg
onMouseDown' target =
  on "mousedown" (Json.map (DragStart target) Mouse.position)
