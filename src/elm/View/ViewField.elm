module View.ViewField exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave)
import Json.Decode as Json exposing (..)
import Math.Vector2 exposing (..)
import Mouse
import String
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
    xf = getGlobalTransform model
    (gx, gy) = toTuple <| .translate <| xf
    scale = .scale <| xf

    looseJacks = model.jackByID |> Dict.values |> List.filter (not << isConnected model)
  in
    Svg.svg
      [ width "100%"
      , height "100%"
      , onMouseWheel'
      ]
      [ Svg.g
        [ transform <| " scale(" ++ (toString scale) ++ ")" ++ "translate( " ++ (toString gx) ++ "," ++ (toString gy) ++ " )"
        ]
        [ Svg.g [] (model.linkByID |> Dict.values |> List.map (drawLink model))
        , Svg.g [] (model.processByID |> Dict.values |> List.map (drawProcess model))
        , Svg.g [] (model.containerByID |> Dict.values |> List.map (drawContainer model))
        , Svg.g [] (looseJacks |> List.map (drawJack model))
        ]
      ]


drawProcess : Model -> Process -> Svg Msg
drawProcess model process =
  let
    backgroundColor = "cornflowerblue"
    realPosition = getProcessPosition model process
    (w, h) = process.rect

    attrs =
      [ onMouseDown' <| DragProcess process
      , fill backgroundColor
      , stroke "white"
      , rx "10"
      , ry "10"
      , xlinkHref <| "http://placekitten.com/" ++ toString w ++ "/" ++ toString h
      , Html.Attributes.style
          [ "cursor" => "move"
          ]
      ] ++ rectAttrs process.rect realPosition

    headAttrs =
      [ width (toString w)
      , height "40"
      ]

    textAttrs = headAttrs ++
      [ x (toString <| (getX realPosition))
      , y (toString <| (getY realPosition) - 90)
      , fontSize "20px"
      , textAnchor "middle"
      ]

    boxAttrs = headAttrs ++
      [ x (toString <| (getX realPosition) - 80)
      , y (toString <| (getY realPosition) - 120)
      , fill "white"
      , stroke "#ddd"
      , strokeWidth "1px"
      ]

    body = image attrs []
    textBox = rect boxAttrs []
    textContent = text' textAttrs [(text process.name)]
    head = g [] [textBox, textContent]
  in
    g []
      [ head
      , body ]


drawContainer : Model -> Container -> Svg Msg
drawContainer model container =
  let
    backgroundColor = "orange"
    realPosition = getContainerPosition model container
    attrs =
      [ onMouseDown' <| DragContainer container
      , fill backgroundColor
      , stroke "white"
      , xlinkHref "http://placekitten.com/400"
      , Html.Attributes.style
          [ "cursor" => "move"
          ]
      ] ++ rectAttrs container.rect realPosition

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

drawJack : Model -> Jack -> Svg Msg
drawJack model jack =
  let
    jackCoords = toRecord <| getJackPosition model jack
    x0 = toString <| jackCoords.x
    y0 = toString <| jackCoords.y
    outline =
      Svg.path
        [ onMouseDown' <| DragJack jack
        , d <| Shape.chevron Shape.jackDimensions
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

drawLink : Model -> Link -> Svg Msg
drawLink ({containerByID, jackByID} as model) link =
  let
    container = seize link.containerID containerByID
    jack = seize link.jackID jackByID
    process = getJackProcess model jack
    containerCoords = toRecord <| getContainerPosition model container
    processCoords = toRecord <| getProcessPosition model process
    cx = toString <| containerCoords.x
    cy = toString <| containerCoords.y
    jx = toString <| processCoords.x
    jy = toString <| processCoords.y

    color = case jack.direction of
      Input -> "cornflowerblue"
      Output -> "gold"

    domID = "link-" ++ (toString link.id)
    dval = ["M", cx, ",", cy, "L", jx, ",", jy] |> String.join " "
    linkLine =
      Svg.path
        [ d dval
        , stroke color
        , strokeWidth "25"
        , id domID
        ]
        []

    linkText =
      text'
        [ alignmentBaseline "bottom"
        , fill "white"
        , fontSize "16px"
        , fontFamily "Helvetica Neue, sans-serif"
        , dy "5"
        ]
        [ textPath
          [ xlinkHref <| "#" ++ domID
          , startOffset <| toString link.textOffset
          ]
          [ text <| String.join " (2kg/day) › › › › › " <| repeat 100 (String.toUpper jack.name)
          ]
        ]


  in
    g []
      [ linkLine
      , linkText
      ]


isConnected : Model -> Jack -> Bool
isConnected model jack =
  let
    link = model.linkByID |> Dict.values |> List.filter (\{jackID} -> jack.id == jackID) |> List.head
  in
    case link of
      Just _ -> True
      Nothing -> False

rectAttrs : Rect -> Vec2 -> List (Svg.Attribute Msg)
rectAttrs (w, h) position =
  let
    pos = position `sub` (vec2 (w / 2) (h / 2))
  in
    [ x ((getX pos) |> toString)
    , y ((getY pos) |> toString)
    , width (w |> toString)
    , height (h |> toString)
    ]


onMouseDown' : Draggable -> Attribute Msg
onMouseDown' target =
  on "mousedown" (Json.map (DragStart target) Mouse.position)


onMouseWheel' : Attribute Msg
onMouseWheel' =
  let
    makeMouseWheelMsg : Int -> Int -> Float -> (Int, Int) -> Msg
    makeMouseWheelMsg clientX clientY deltaY dimensions =
      MouseWheelTurn (Mouse.Position clientX clientY) dimensions deltaY
    targetDecoder = object2 (,) ("width" := int) ("height" := int)
    decoder : Json.Decoder Msg
    decoder = object4 makeMouseWheelMsg ("clientX" := int) ("clientY" := int) ("deltaY" := float) ("target" := targetDecoder)
  in
    on "wheel" decoder

