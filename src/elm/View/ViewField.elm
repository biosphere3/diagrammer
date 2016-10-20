module View.ViewField exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave, onMouseUp)
import Json.Decode as Json exposing ((:=), int, float, object4, object2)
import Math.Vector2 exposing (..)
import Mouse
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Maybe exposing (..)
import Util exposing (..)
import Model exposing (..)
import Calc exposing (getCalc)
import Shape
import State exposing (Msg(..))

(=>) = (,)

view : Model -> Html Msg
view model =
  let
    xf = getGlobalTransform model
    (gx, gy) = toTuple <| .translate <| xf
    scale = .scale <| xf
    (calc, _) = getCalc model
  in
    Svg.svg
      [ width "100%"
      , height "100%"
      , onMouseWheel'
      ]
      [ Svg.g
        [ transform <| " scale(" ++ (toString scale) ++ ")" ++ "translate( " ++ (toString gx) ++ "," ++ (toString gy) ++ " )"
        ]
        [ Svg.g [] (model.linkByID |> Dict.values |> List.map (drawLink model calc))
        , Svg.g [] (model.processByID |> Dict.values |> List.map (drawProcess model calc))
        , Svg.g [] (model.containerByID |> Dict.values |> List.map (drawContainer model calc))
        ]
      ]


drawProcess : Model -> Calc -> Process -> Svg Msg
drawProcess model calc process =
  let
    realPosition = toRecord <| getProcessPosition model process
    (w, h) = process.rect
    imageURL = process.imageURL |> withDefault ("http://placekitten.com/" ++ toString w ++ "/" ++ toString h)

    jacks = getJacksByProcessID model process.id
    looseJacks = jacks |> List.filter (not << isConnected model)

    attrs =
      [ onMouseDown' <| DragProcess process
      , rx "10"
      , ry "10"
      , x (toString <| -w / 2)
      , y (toString <| -h / 2)
      , width (toString w)
      , height (toString h)
      , xlinkHref <| imageURL
      , Html.Attributes.style
          [ "cursor" => "move"
          ]
      ]

    headAttrs =
      [ width (toString w)
      , height "40"
      ]

    textAttrs = headAttrs ++
      [ x (toString <| 0)
      , y (toString <| 0 - 90)
      , fontSize "20px"
      , textAnchor "middle"
      ]

    boxAttrs = headAttrs ++
      [ x (toString <| 0 - 80)
      , y (toString <| 0 - 120)
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
      [ g [ transform <| fn2 "transform" realPosition.x realPosition.y ]
        [ head
        , body
        ]
      , g [] (looseJacks |> List.map (drawJack model calc))
      ]

drawJack : Model -> Calc -> Jack -> Svg Msg
drawJack model calc jack =
  let
    jackCoords = toRecord <| getJackPosition model jack
    x0 = toString <| jackCoords.x
    y0 = toString <| jackCoords.y

    isDragging = isDragSubjectID model jack.id  -- would rather not compute this every time

    outline =
      Svg.path (
        [ onMouseDown' <| DragJack jack
        , onMouseUp <| DragEndTargetJack jack
        , d <| Shape.chevron Shape.jackDimensions
        , Html.Attributes.style
            [ "cursor" => "move"
            , "fill" => "white"
            , "stroke" => "black"
            , "strokeWidth" => "2"
            , "pointer-events" => if isDragging then "none" else "auto"  -- don't catch mouseUp while dragging
            ]
        ])
        []
    content = text'
      [x "50", alignmentBaseline "middle", textAnchor "middle"]
      [text <| jack.name ]
  in
    g [ transform <| "translate(" ++ x0 ++ "," ++ y0 ++ ")"]
      [ outline, content ]

isDragSubjectID model id =
  model.drag
  |> Maybe.map (\drag ->
    case drag.target of
      DragJack dragJack -> dragJack.id == id
      _ -> False
    )
  |> Maybe.withDefault False

drawContainer : Model -> Calc -> Container -> Svg Msg
drawContainer model calc container =
  let
    backgroundColor = "orange"
    realPosition = getContainerPosition model container
    amountDisplay =
      Dict.get container.id calc.containerByID
      |> Maybe.map (toString << .amount)
      |> Maybe.withDefault "???"

    displayText = container.name ++ " " ++ amountDisplay

    attrs =
      [ onMouseDown' <| DragContainer container
      , onMouseUp <| DragEndTargetContainer container
      , fill backgroundColor
      , stroke "white"
      , xlinkHref "http://placekitten.com/400"
      , Html.Attributes.style
          [ "cursor" => "move"
          , "pointer-events" =>
              if (isDragSubjectID model container.id) then "none" else "auto"
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
      , text' textAttrs [text displayText]
      ]

drawLink : Model -> Calc -> Link -> Svg Msg
drawLink ({containerByID, jackByID} as model) calc link =
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

    flowDisplay =
      Dict.get jack.id calc.jackByID
      |> map (toString << .flow)
      |> withDefault "???"

    color = case jack.direction of
      Input -> "#008800"
      Output -> "#004400"

    arrowText = case jack.direction of
      Input -> "  ❯ ❯ ❯  "
      Output -> "  ❮ ❮ ❮  "

    linkClass = case jack.direction of
      Input -> "input"
      Output -> "output"

    domID = "link-" ++ (toString link.id)
    dval = ["M", cx, ",", cy, "L", jx, ",", jy] |> String.join " "
    displayText =
      (String.toUpper jack.name) ++ " " ++ flowDisplay

    linkStripe =
      Svg.path
        [ d dval
        , stroke color
        , strokeWidth "20"
        , id domID
        , class "link-stripe"
        ]
        []

    linkLine =
      Svg.path
        [ d dval
        , stroke "white"
        , strokeWidth "5"
        , strokeDasharray "10,20"
        , id domID
        , class "link-line"
        ]
        []

    linkText =
      text'
        [ alignmentBaseline "bottom"
        , fill "black"
        , fontSize "16px"
        , fontFamily "Helvetica Neue, sans-serif"
        , dy "-15"
        ]
        [ textPath
          [ xlinkHref <| "#" ++ domID
          ]
          [ text <| String.join arrowText <| repeat 100 displayText
          ]
        ]


  in
    g [ class <| "link " ++ linkClass
      ]
      [ linkStripe
      , linkLine
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

