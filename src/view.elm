module View exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
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
          [ x (toString <| realPosition.x)
          , y (toString <| realPosition.y - 90)
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
          , x (toString realPosition.x)
          , y (toString realPosition.y)
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
        jackPosition = getJackPosition model jack
        x0 = toString <| jackPosition.x
        y0 = toString <| jackPosition.y
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
    drawFlow {containerByID, jackByID} {containerID, jackID, direction} =
      let
        container = seize containerID containerByID
        containerPosition = getContainerPosition model container
        jack = seize jackID jackByID
        jackPosition = getJackPosition model jack
        cx = toString <| containerPosition.x
        cy = toString <| containerPosition.y
        jx = toString <| jackPosition.x + 50
        jy = toString <| jackPosition.y
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
  in
    Svg.svg
      [ width "100%"
      , height "100%"
      ]
      [ Svg.g [] (model.flowByID |> Dict.values |> List.map (drawFlow model))
      , Svg.g [] (model.processByID |> Dict.values |> List.map drawProcess)
      , Svg.g [] (model.containerByID |> Dict.values |> List.map drawContainer)
      , Svg.g [] (model.jackByID |> Dict.values |> List.map drawJack)
      ]




shapeAttrs : Shape -> Position -> List (Svg.Attribute Msg)
shapeAttrs shape position =
  case shape of
    Rect w h ->
      let
        cx = position.x
        cy = position.y
      in
        [ x (cx - w // 2 |> toString)
        , y (cy - h // 2 |> toString)
        , width (w |> toString)
        , height (h |> toString)
        ]
    Circle radius ->
      [ cx <| toString position.x
      , cy <| toString position.y
      , r <| toString radius
      ]
    Chevron width height ->
      []


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




onMouseDown' : Draggable -> Attribute Msg
onMouseDown' target =
  on "mousedown" (Json.map (DragStart target) Mouse.position)

