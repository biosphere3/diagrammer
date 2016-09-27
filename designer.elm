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
  , process : Process
  }


toDictByID : List { a | id : ID } -> Dict ID { a | id : ID }
toDictByID seq =
  seq
  |> List.indexedMap (\i data -> (i, { data | id = i }))
  |> Dict.fromList

getUnsafe : comparable -> Dict comparable b -> b
getUnsafe v d =
  case Dict.get v d of
    Nothing -> (Debug.crash <| "Failed lookup: " ++ (toString v) ++ " of " ++ (toString d))
    Just ret -> ret

mkProcess {name, position} = Process 0 name "" Nothing position

mkPort processByID {name, processID, direction} =
  let
    process = case Dict.get processID processByID of
      Just process -> process
      Nothing -> (Debug.crash (toString processID))
    position = Position 20 20
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
      , { name = "Biogas", processID = 1, direction = Output }
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
    = DragStart Process Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({processByID, drag} as model) =
  case msg of
    DragStart process xy ->
      { model | drag = (Just (Drag xy xy process)) }

    DragAt xy ->
      { model | drag = (Maybe.map (\{start, process} -> Drag start xy process) drag) }

    DragEnd _ ->
      case drag of
        Nothing -> model  -- will never happen
        Just drag ->
          let
            update : ID -> Process -> Process
            update _ process =
              if drag.process.id == process.id
              then { process | position = getPosition (Just drag) process }
              else process
            newprocessById : ProcessDict
            newprocessById = (Dict.map update processByID)
          in
            { model | processByID = newprocessById, drag = Nothing }



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
        realPosition = getPosition model.drag process
        backgroundColor = "blue"
        cx = realPosition.x
        cy = realPosition.y
        w = 150
        h = floor(150 * 1.9)
      in
        rect
          [ onMouseDown' process
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
        process = getUnsafe flowport.processID model.processByID
      in
        circle
          [ cx (process.position.x + flowport.position.x |> toString)
          , cy (process.position.y + flowport.position.y |> toString)
          , r "20"
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


getPosition : Maybe Drag -> Process -> Position
getPosition drag ({position, id}) =
  case drag of
    Nothing ->
      position

    Just {start, current, process} ->
      if process.id == id
      then
        Position
          (position.x + current.x - start.x)
          (position.y + current.y - start.y)
      else
        position


onMouseDown' : Process -> Attribute Msg
onMouseDown' process =
  on "mousedown" (Json.map (DragStart process) Mouse.position)
