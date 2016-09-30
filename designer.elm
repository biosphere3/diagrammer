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

import Util exposing (..)

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
  , jackByID : JackDict
  , flowByID : FlowDict
  , containerByID : ContainerDict

  -- ui
  , drag : Maybe Drag
  }


type alias ID = Int

type alias ProcessDict = Dict ID Process
type alias JackDict = Dict ID Jack
type alias FlowDict = Dict ID Flow
type alias ContainerDict = Dict ID Container

--type alias Positioned a =
--  { a | position : Position
--  }

jackRadius = 20

type alias Process =
    { id : ID
    , name : String
    , description : String
    , imageURL : Maybe String
    , position : Position
    , shape : Shape
    }

type alias Flow =
  { id : ID
  , containerID : ID
  , jackID : ID
  , direction : FlowDirection
  --, via : Resource
  }

type alias Container =
  { id : ID
  , name : String
  , position : Position
  , shape : Shape
  }

type alias Resource =
  { name : String
  , state : MatterState
  }

type Shape = Rect Int Int
           | Chevron Int Int
           | Circle Int

type Draggable = DragProcess Process | DragJack Jack | DragContainer Container

type MatterState = Solid | Liquid | Gas | Plasma

type JackDirection = Input | Output
type FlowDirection = InFlow | OutFlow

type alias Jack =
  { id : ID
  , name : String
  , processID : ID
  , rate : Float
  , direction : JackDirection
  , position : Position
  , shape : Shape
  }

type alias Drag =
  { start : Position
  , current : Position
  , target : Draggable
  }


mkProcess {name, position} = Process 0 name "" Nothing position (Rect 160 80)

mkContainer {name, position} = Container 0 name position (Rect 160 160)

mkFlow {containerID, jackID, direction} = Flow containerID jackID direction

mkJack processByID {name, processID, direction} =
  let
    process = case Dict.get processID processByID of
      Just process -> process
      Nothing -> (Debug.crash (toString processID))
    position = process.position /+/ Position 50 50
    shape = Circle jackRadius
  in Jack 0 name processID 42.0 direction position shape

init : ( Model, Cmd Msg )
init =
  let
    processByID : ProcessDict
    processByID =
      [ { name = "Biodigester", position = Position 100 100 }
      , { name = "Can of Beans", position = Position 300 400 }
      , { name = "Can of Beans", position = Position 500 200 }
      ] |> (List.map mkProcess) |> toDictByID

    jackByID : JackDict
    jackByID =
      [ { name = "Effluent", processID = 1, direction = Input }
      , { name = "Biogas", processID = 2, direction = Output }
      ] |> (List.map (mkJack processByID)) |> toDictByID

    --flowByID : FlowDict
    --flowByID =
    --  [] |> (List.map mkFlow) |> toDictByID

    containerByID : ContainerDict
    containerByID =
      [ { name = "Rain Barrel", position = Position 100 600}
      ] |> (List.map mkContainer) |> toDictByID
  in (
    Model
      processByID
      jackByID
      Dict.empty --flowByID
      containerByID
      --Dict.empty
      Nothing
     , Cmd.none )


-- UPDATE

jacksCollide : Jack -> Jack -> Bool
jacksCollide a b =
  (log "dists" <| distanceSquared a.position b.position) < jackRadius * jackRadius


jackCollisions : Model -> Jack -> List Jack
jackCollisions model jack =
  let
    jacks = Dict.values model.jackByID
    hit = \j -> j.id /= jack.id && (jacksCollide jack j)
  in
    List.filter hit jacks


type Msg
    = DragStart Draggable Position
    | DragAt Position
    | DragEnd Position

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )

getJacks : Model -> Process -> List Jack
getJacks {jackByID} process =
  jackByID
  |> Dict.values
  |> List.filter (\x -> x.processID == process.id)


joinJacks : Model -> List Jack -> Model
joinJacks model jacks = model
  -- add a link between these jacks


updateHelp : Msg -> Model -> Model
updateHelp msg ({processByID, jackByID, containerByID, drag} as model) =
  case msg of
    DragStart target xy ->
      { model | drag = (Just (Drag xy xy target)) }

    DragAt xy ->
      { model | drag = (Maybe.map (\{start, target} -> Drag start xy target) model.drag) }

    DragEnd _ ->
      case drag of
        Nothing -> Debug.crash "not gonna happen"
        Just drag ->
          let
            model' = { model | drag = Nothing }
            updateProcessPosition process =
              { process | position = getProcessPosition model process }
            updateJackPosition jack =
              { jack | position = getJackPosition model jack }
            updateContainerPosition container =
              { container | position = getContainerPosition model container }
          in case drag.target of
            DragProcess dragProcess ->
              let
                attachedJacks = getJacks model dragProcess
              in
                { model'
                | processByID = processByID |> Dict.update dragProcess.id (Maybe.map updateProcessPosition)
                , jackByID = jackByID |> updateMulti (List.map .id attachedJacks) (Maybe.map updateJackPosition)
                }
            DragContainer dragContainer ->
              { model'
              | containerByID = containerByID |> Dict.update dragContainer.id (Maybe.map updateContainerPosition)
              }
            DragJack dragJack ->
              let
                collisions = jackCollisions model (updateJackPosition dragJack)
              in
                if List.length collisions > 0
                then
                  { model'
                  | jackByID = jackByID |> Dict.update dragJack.id (Maybe.map updateJackPosition)
                  }
                else
                  model'



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

drawShape : Shape -> Position -> List (Svg.Attribute Msg) -> Svg Msg
drawShape shape position attrs =
  case shape of
    Rect w h ->
      let
        cx = position.x
        cy = position.y
      in
        rect
        ( [ x (cx - w // 2 |> toString)
          , y (cy - h // 2 |> toString)
          , width (w |> toString)
          , height (h |> toString)
          ] ++ attrs )
          []
    Circle radius ->
      circle
      ( [ cx <| toString position.x
        , cy <| toString position.y
        , r <| toString radius
        ] ++ attrs )
        []
    Chevron width height ->
      drawShape (Circle width) position attrs


view : Model -> Html Msg
view model =
  let
    drawProcess : Process -> Svg Msg
    drawProcess process =
      let
        backgroundColor = "cornflowerblue"
        realPosition = getProcessPosition model process
      in
        drawShape process.shape realPosition
          [ onMouseDown' <| DragProcess process
          , fill backgroundColor
          , stroke "white"
          , Html.Attributes.style
              [ "cursor" => "move"
              ]
          ]

    drawContainer : Container -> Svg Msg
    drawContainer container =
      let
        backgroundColor = "orange"
        realPosition = getContainerPosition model container
      in
        drawShape container.shape realPosition
          [ onMouseDown' <| DragContainer container
          , fill backgroundColor
          , stroke "white"
          , Html.Attributes.style
              [ "cursor" => "move"
              ]
          ]

    drawJack : Jack -> Svg Msg
    drawJack jack =
      let
        jackPosition = getJackPosition model jack
      in
        drawShape jack.shape jackPosition
          [ onMouseDown' <| DragJack jack
          , Html.Attributes.style
              [ "cursor" => "move"
              ]
          ]

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
      [ Svg.g [] (model.processByID |> Dict.values |> List.map drawProcess)
      , Svg.g [] (model.containerByID |> Dict.values |> List.map drawContainer)
      , Svg.g [] (model.jackByID |> Dict.values |> List.map drawJack)
      ]



px : Int -> String
px number =
  toString number ++ "px"


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

getContainerPosition : Model -> Container -> Position
getContainerPosition {drag} {id, position} =
    case drag of
      Nothing ->
        position

      Just {start, current, target} ->
        case target of
          DragContainer dragContainer ->
            if dragContainer.id == id
            then
              Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)
            else
              position
          _ -> position

(/+/) p1 p2 = Position (p1.x + p2.x) (p1.y + p2.y)
(/-/) p1 p2 = Position (p1.x - p2.x) (p1.y - p2.y)

norm  p = p.x * p.x + p.y * p.y
distanceSquared : Position -> Position -> Int
distanceSquared p1 p2 = norm <| p1 /-/ p2
distance p1 p2 = sqrt <| toFloat <| distanceSquared p1 p2

getJackPosition : Model -> Jack -> Position
getJackPosition {drag, processByID} {id, position, processID} =
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
              then position /+/ offset
              else position
            DragJack dragJack ->
              if dragJack.id == id
              then position /+/ offset
              else position
            _ -> position


onMouseDown' : Draggable -> Attribute Msg
onMouseDown' target =
  on "mousedown" (Json.map (DragStart target) Mouse.position)
