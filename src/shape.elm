module Shape exposing (..)

import String

chevron : Float -> Float -> String
chevron width height =
  let
    angle = degrees 60
    dy = height / 2
    dx = dy / tan angle
    ts = toString
    pair x y = (toString x) ++ "," ++ (toString y)
  in
    [ "M", (pair 0 0)
    , "l", (pair dx dy)
    , "l", (pair width 0)
    , "l", (pair -dx -dy)
    , "l", (pair  dx -dy)
    , "l", (pair -width 0)
    , "l", (pair -dx dy)
    ] |> String.join " "
