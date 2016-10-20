module Shape exposing (..)

import String
import Math.Vector2 exposing (..)

jackDimensions = (130, 60)


type Shape = Circle Float
           | Rect Float Float


getProcessShape {rect} =
    let (width, height) = rect
    in Rect width height

getContainerShape {radius} =
    Circle radius


chevron : (Float, Float) -> String
chevron (width, height) =
  let
    angle = degrees <| 60
    dy = height / 2
    dx = -dy / tan angle
    ts = toString
  in
    [ "M", (pair 0 0)
    , "l", (pair dx dy)
    , "l", (pair width 0)
    , "l", (pair -dx -dy)
    , "l", (pair  dx -dy)
    , "l", (pair -width 0)
    , "l", (pair -dx dy)
    ] |> String.join " "

pair x y =
    (toString x) ++ "," ++ (toString y)
