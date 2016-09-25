import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
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
    { thingDict : ThingDict
    , drag : Maybe Drag
    }

type alias Thing =
    { id : ThingID
    , position : Position
    }

type alias ThingID = Int
type alias ThingDict = Dict ThingID Thing


type alias Drag =
    { start : Position
    , current : Position
    , thing : Thing
    }


init : ( Model, Cmd Msg )
init =
  ( Model initThingDict Nothing, Cmd.none )

initThingDict : ThingDict
initThingDict = Dict.fromList
  [ (1, Thing 1 (Position 200 200))
  , (2, Thing 2 (Position 400 400))
  ]


-- UPDATE


type Msg
    = DragStart Thing Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({thingDict, drag} as model) =
  case msg of
    DragStart thing xy ->
      Model thingDict (Just (Drag xy xy thing))

    DragAt xy ->
      Model thingDict (Maybe.map (\{start, thing} -> Drag start xy thing) drag)

    DragEnd _ ->
      case drag of
        Nothing -> model  -- will never happen
        Just drag ->
          let
            update : ThingID -> Thing -> Thing
            update _ thing =
              if drag.thing.id == thing.id
              then { thing | position = getPosition (Just drag) thing.position }
              else thing
            newThingDict : ThingDict
            newThingDict = (Dict.map update thingDict)
          in
            Model newThingDict Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  let
    drawThing : ThingID -> Thing -> Html Msg
    drawThing _ thing =
      let
        realPosition = getPosition model.drag thing.position
      in
        div
          [ onMouseDown thing
          , style
              [ "background-color" => "#3C8D2F"
              , "cursor" => "move"

              , "width" => "100px"
              , "height" => "100px"
              , "border-radius" => "4px"
              , "position" => "absolute"
              , "left" => px realPosition.x
              , "top" => px realPosition.y

              , "color" => "white"
              , "display" => "flex"
              , "align-items" => "center"
              , "justify-content" => "center"
              ]
          ]
          [ text "Drag Me!"
          ]
  in div [] (model.thingDict |> (Dict.map drawThing) |> Dict.values)



px : Int -> String
px number =
  toString number ++ "px"


getPosition : Maybe Drag -> Position -> Position
getPosition drag position =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)


onMouseDown : Thing -> Attribute Msg
onMouseDown thing =
  on "mousedown" (Json.map (DragStart thing) Mouse.position)
