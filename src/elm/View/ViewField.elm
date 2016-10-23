module View.ViewField exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave, onMouseUp, onDoubleClick)
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
import JackRouting
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
      , Html.Attributes.style
        [ "background" => "#ddd"
        ]
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
    (w, h) = jack.rect

    isDragging = isDragSubjectID model jack.id  -- would rather not compute this every time

    color = getStateColor jack.matterState

    outline =
      Svg.path (
        [ onMouseDown' <| DragJack jack
        , onMouseUp <| DragEndTargetJack jack
        , d <| Shape.chevron Shape.jackDimensions
        , Html.Attributes.style
            [ "cursor" => "move"
            , "fill" => color
            , "stroke" => "#ddd"
            , "strokeWidth" => "1"
            , "pointer-events" => if isDragging then "none" else "auto"  -- don't catch mouseUp while dragging
            ]
        ])
        []

    contentName = text'
      [ x <| toString (w / 2 - 10)
      , y <| toString (-h / 5)
      , alignmentBaseline "middle"
      , textAnchor "middle"
      , fill "white"
      , fontSize "20px"
      ]
      [ text <| jack.name ]

    contentQuantity = text'
      [ x <| toString (w / 2 - 10)
      , y <| toString (h / 5)
      , alignmentBaseline "middle"
      , textAnchor "middle"
      , fill "white"
      ]
      [ text <| toString jack.rate ++ " " ++ jack.units ]
  in
    g [ transform <| "translate(" ++ x0 ++ "," ++ y0 ++ ")"]
      [ outline, contentName, contentQuantity ]

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
    realPosition = toRecord <| getContainerPosition model container
    radius = container.radius

    amountDisplay =
      Dict.get container.id calc.containerByID
      |> Maybe.map (toString << .amount)
      |> Maybe.withDefault "???"

    displayText = container.name ++ " " ++ amountDisplay

    attrs =
      [ onMouseDown' <| DragContainer container
      , onMouseUp <| DragEndTargetContainer container
      , cx "0"
      , cy "0"
      , r (toString <| radius)
      , fill "white"
      , stroke "black"
      , strokeWidth "3"
      , xlinkHref "http://placekitten.com/400"
      , Html.Attributes.style
          [ "cursor" => "move"
          , "pointer-events" =>
              if (isDragSubjectID model container.id) then "none" else "auto"
          ]
      ]

    textAttrs =
      [ fontSize "20px"
      , x "0"
      , y "0"
      , fill "black"
      , textAnchor "middle"
      , alignmentBaseline "middle"
      ]
  in
    g [ transform <| fn2 "translate" realPosition.x realPosition.y
      , onDoubleClick (RemoveContainer container) ]
      [ circle attrs []
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

    stripeColor = getStateColor jack.matterState
    textColor = getStateContrastColor jack.matterState

    arrowText = case jack.direction of
      Input -> "   ❮ ❮ ❮   "
      Output -> "  ❯ ❯ ❯   "

    linkClass = case jack.direction of
      Input -> "input"
      Output -> "output"

    domID = "link-" ++ (toString link.id)
    dval = JackRouting.linkPath model process container
    displayText =
      (String.toUpper jack.name) ++ "  " ++ flowDisplay ++ " " ++ jack.units

    linkStripe =
      Svg.path
        [ d dval
        , fill "none"
        , stroke stripeColor
        , strokeWidth "25"
        , id domID
        , class "link-stripe"
        ]
        []

    linkLine =
      Svg.path
        [ d dval
        , fill "none"
        , stroke textColor
        , strokeWidth "5"
        , strokeDasharray "10,20"
        , id domID
        , class "link-line"
        ]
        []

    linkText =
      text'
        [ alignmentBaseline "bottom"
        , fill textColor
        , fontSize "14px"
        , fontFamily "Helvetica Neue, sans-serif"
        , dy "5"
        ]
        [ textPath
          [ xlinkHref <| "#" ++ domID
          ]
          [ text <| String.join arrowText <| repeat 100 displayText
          ]
        ]


  in
    g [ class <| "link " ++ linkClass
      , onDoubleClick (RemoveLink link)
      ]
      [ linkStripe
      , linkLine
      , linkText
      ]


getStateColor : MatterState -> String
getStateColor state =
  case state of
    SolidState -> "green"
    LiquidState -> "blue"
    GasState -> "orange"
    EnergyState -> "red"
    LightState -> "yellow"
    UnspecifiedState -> "gray"


getStateContrastColor : MatterState -> String
getStateContrastColor state =
  case state of
    LightState -> "black"
    _ -> "white"


isConnected : Model -> Jack -> Bool
isConnected model jack =
  let
    link = model.linkByID |> Dict.values |> List.filter (\{jackID} -> jack.id == jackID) |> List.head
  in
    case link of
      Just _ -> True
      Nothing -> False


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

