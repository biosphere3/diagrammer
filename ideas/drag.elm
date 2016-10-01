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
    { thingDict : ThingDict
    , links : List Link
    , drag : Maybe Drag
    , hoverThing : Maybe Thing
    }

type alias Thing =
    { id : ThingID
    , position : Position
    }

type alias Link =
  { source : Thing
  , dest : Thing
  }

type alias ThingID = Int
type alias ThingDict = Dict ThingID Thing
type alias ThingDef = Position

type alias Drag =
    { start : Position
    , current : Position
    , thing : Thing
    }


init : ( Model, Cmd Msg )
init =
  ( Model initThingDict initLinks Nothing Nothing, Cmd.none )

thingDefs : List ThingDef
thingDefs =
  let
    total = 32
    r = 500.0
    cx = 500
    cy = 500
    getPos : Float -> Position
    getPos n = Position
      (floor ((cos n) * r + cx))
      (floor ((sin n) * r + cy))
  in
    List.indexedMap
      (\n _ -> (getPos ( 2 * pi * (toFloat n) / total)))
      (List.repeat total 0)

initThingDict : ThingDict
initThingDict =
  let
    mkThing : ThingID -> ThingDef -> (ThingID, Thing)
    mkThing id position = (id, Thing id position)
  in
    thingDefs
    |> (List.indexedMap mkThing)
    |> Dict.fromList

pairs : List Thing -> List Link
pairs xs =
  case xs of
    ( x :: x' :: xs' ) -> (Link x x') :: (pairs (x' :: xs'))
    _ -> []

initLinks : List Link
initLinks =
  [1, 11, 22]
  |> List.filterMap (\x -> Dict.get x initThingDict)
  |> pairs


-- UPDATE


type Msg
    = DragStart Thing Position
    | DragAt Position
    | DragEnd Position
    | HoverIn Thing
    | HoverOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )


updateHelp : Msg -> Model -> Model
updateHelp msg ({thingDict, drag} as model) =
  case msg of
    DragStart thing xy ->
      { model | drag = (Just (Drag xy xy thing)) }

    DragAt xy ->
      { model | drag = (Maybe.map (\{start, thing} -> Drag start xy thing) drag) }

    DragEnd _ ->
      case drag of
        Nothing -> model  -- will never happen
        Just drag ->
          let
            update : ThingID -> Thing -> Thing
            update _ thing =
              if drag.thing.id == thing.id
              then { thing | position = getPosition (Just drag) thing }
              else thing
            newThingDict : ThingDict
            newThingDict = (Dict.map update thingDict)
          in
            { model | thingDict = newThingDict, drag = Nothing }

    HoverIn thing ->
      { model | hoverThing = Just thing}

    HoverOut ->
      { model | hoverThing = Nothing}



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
    drawThing : ThingID -> Thing -> Svg Msg
    drawThing _ thing =
      let
        realPosition = getPosition model.drag thing
        backgroundColor =
          if model.hoverThing == Just thing
          then "#3C8D2F"
          else "blue"
      in
        circle
          [ onMouseDown' thing
          --, onMouseEnter (HoverIn thing)
          --, onMouseLeave HoverOut
          , cx (realPosition.x |> toString)
          , cy (realPosition.y |> toString)
          , fill backgroundColor
          , r "50"
          , Html.Attributes.style
              [ "cursor" => "move"
              ]
          ]
          []
    drawLink : Model -> Link -> Svg Msg
    drawLink model {source, dest} =
      let
        a = toString source.position.x
        b = toString source.position.y
        c = toString dest.position.x
        d = toString dest.position.y
      in
        line
          [ x1 a
          , y1 b
          , x2 c
          , y2 d
          , stroke "black" ]
          []
  in
    Svg.svg
      [ width "100%"
      , height "100%"
      ]
      [
        Svg.g [] (Dict.map drawThing model.thingDict |> Dict.values)
      , Svg.g [] (List.map (drawLink model) model.links)
      ]



px : Int -> String
px number =
  toString number ++ "px"


getPosition : Maybe Drag -> Thing -> Position
getPosition drag ({position, id}) =
  case drag of
    Nothing ->
      position

    Just {start, current, thing} ->
      if thing.id == id
      then
        Position
          (position.x + current.x - start.x)
          (position.y + current.y - start.y)
      else
        position


onMouseDown' : Thing -> Attribute Msg
onMouseDown' thing =
  on "mousedown" (Json.map (DragStart thing) Mouse.position)
