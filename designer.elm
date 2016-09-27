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
  { processById : ProcessDict
  --, portById : Dict ID Port
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
  --Positioned (
    { id : ID
    , name : String
    , description : String
    , imageURL : Maybe String
    , position : Position
    }
  --)

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
  { name : String
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


toDictByID : List Process -> ProcessDict
toDictByID seq =
  seq
  |> List.indexedMap (\i data -> (i, { data | id = i }))
  |> Dict.fromList


mkProcess : { name : String, position : Position } -> Process
mkProcess data =
  Process 0 data.name "" Nothing data.position
  --{ data | id = 0, imageURL = Nothing, position = Position 0 0 }

init : ( Model, Cmd Msg )
init =
  let
    processById : ProcessDict
    processById =
      [ { name = "Biodigester", position = Position 100 100 }
      , { name = "Can of Beans", position = Position 300 400 }
      , { name = "Can of Beans", position = Position 500 200 }
      ] |> (List.map mkProcess) |> toDictByID
    portById = [] |> toDictByID
    flowById = [] |> toDictByID
  in (
    Model
      processById
      --Dict.empty
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
updateHelp msg ({processById, drag} as model) =
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
            newprocessById = (Dict.map update processById)
          in
            { model | processById = newprocessById, drag = Nothing }



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
    drawProcess : ID -> Process -> Svg Msg
    drawProcess _ process =
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
    --drawLink : Model -> Flow -> Svg Msg
    --drawLink {processById} {source, dest} =
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
        Svg.g [] (Dict.map drawProcess model.processById |> Dict.values)
      --, Svg.g [] (List.map (drawLink model) model.links)
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
