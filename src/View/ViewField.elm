module View.ViewField exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave)
import Json.Decode as Json exposing ((:=))
import Math.Vector2 exposing (..)
import Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Util exposing (..)
import Model exposing (..)
import Shape
import State exposing (Msg(..))

(=>) = (,)

view : Model -> Html Msg
view model =
  let
    drawProcess : Process -> Svg Msg
    drawProcess process =
      let
        backgroundColor = "cornflowerblue"
        realPosition = getProcessPosition model process
        (w, h) = case process.shape of
          Rect w h -> (w, h)
          _ -> Debug.crash "never"

        attrs =
          [ onMouseDown' <| DragProcess process
          , fill backgroundColor
          , stroke "white"
          , rx "10"
          , ry "10"
          , xlinkHref <| "//placekitten.com/" ++ toString w ++ "/" ++ toString h
          , Html.Attributes.style
              [ "cursor" => "move"
              ]
          ] ++ shapeAttrs process.shape realPosition

        headAttrs =
          --[ x (toString <| realPosition.x - w // 2)
          [ x (toString <| (getX realPosition))
          , y (toString <| (getY realPosition) - 90)
          , width (toString w)
          , height "40"
          ]

        textAttrs = headAttrs ++
          [ fontSize "20px"
          , textAnchor "middle"
          ]

        boxAttrs = headAttrs ++
          [ stroke "black"
          , fill "white"
          , strokeWidth "2px"
          ]

        body = image attrs []
        textBox = rect boxAttrs []
        textContent = text' textAttrs [(text process.name)]
        head = g [] [textContent]
      in
        g []
          [ head
          , body ]


    drawContainer : Container -> Svg Msg
    drawContainer container =
      let
        backgroundColor = "orange"
        realPosition = getContainerPosition model container
        attrs =
          [ onMouseDown' <| DragContainer container
          , fill backgroundColor
          , stroke "white"
          , xlinkHref "//placekitten.com/400"
          , Html.Attributes.style
              [ "cursor" => "move"
              ]
          ] ++ shapeAttrs container.shape realPosition

        textAttrs =
          [ fontSize "20px"
          , x (toString <| getX realPosition)
          , y (toString <| getY realPosition)
          , textAnchor "middle"
          ]
      in
        g []
          [ rect attrs []
          , text' textAttrs [text container.name]
          ]

    drawJack : Jack -> Svg Msg
    drawJack jack =
      let
        jackCoords = toRecord <| getJackPosition model jack
        x0 = toString <| jackCoords.x
        y0 = toString <| jackCoords.y
        outline =
          Svg.path
            [ onMouseDown' <| DragJack jack
            , d <| Shape.chevron 100 50
            , Html.Attributes.style
                [ "cursor" => "move"
                , "fill" => "white"
                , "stroke" => "black"
                , "strokeWidth" => "3"
                ]
            ] []
        content = text'
          [x "20", alignmentBaseline "middle"]
          [text <| jack.name ]
      in
        g [ transform <| "translate(" ++ x0 ++ "," ++ y0 ++ ")"]
          [ outline, content ]

    drawFlow : Model -> Flow -> Svg Msg
    drawFlow {containerByID, jackByID} {containerID, jackID} =
      let
        container = seize containerID containerByID
        containerCoords = toRecord <| getContainerPosition model container
        jack = seize jackID jackByID
        jackCoords = toRecord <| getJackPosition model jack
        cx = toString <| containerCoords.x
        cy = toString <| containerCoords.y
        jx = toString <| jackCoords.x + 50
        jy = toString <| jackCoords.y
      in
        line
          [ x1 cx
          , y1 cy
          , x2 jx
          , y2 jy
          , stroke "black"
          , strokeWidth "25"
          ]
          []

    (gx, gy) = toTuple <| .translate <| getGlobalTransform model
  in
    Svg.svg
      [ width "100%"
      , height "100%"
      ]
      [ Svg.g
        [ transform <| "translate( " ++ (toString gx) ++ "," ++ (toString gy) ++ " )"]
        [ Svg.g [] (model.flowByID |> Dict.values |> List.map (drawFlow model))
        , Svg.g [] (model.processByID |> Dict.values |> List.map drawProcess)
        , Svg.g [] (model.containerByID |> Dict.values |> List.map drawContainer)
        , Svg.g [] (model.jackByID |> Dict.values |> List.map drawJack)
        ]
      ]


shapeAttrs : Shape -> Vec2 -> List (Svg.Attribute Msg)
shapeAttrs shape position =
  case shape of
    Rect w h ->
      let
        pos = position `sub` (vec2 (w / 2) (h / 2))
      in
        [ x ((getX pos) |> toString)
        , y ((getY pos) |> toString)
        , width (w |> toString)
        , height (h |> toString)
        ]
    Circle radius ->
      [ cx <| toString (getX position)
      , cy <| toString (getY position)
      , r <| toString radius
      ]
    Chevron width height ->
      []


onMouseDown' : Draggable -> Attribute Msg
onMouseDown' target =
  on "mousedown" (Json.map (DragStart target) Mouse.position)

