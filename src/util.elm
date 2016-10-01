module Util exposing (..)

import Mouse exposing (Position)

import Debug
import Dict exposing (Dict)


toDictByID : List { a | id : Int } -> Dict Int { a | id : Int }
toDictByID seq =
  let
    f i data =
      if data.id
      then (data.id, data)
      else (i, { data | id = i })
  in
    seq
    |> List.indexedMap (\i data -> (i, { data | id = i }))
    |> Dict.fromList

seize : comparable -> Dict comparable b -> b
seize v d =
  case Dict.get v d of
    Nothing -> (Debug.crash <| "Failed lookup: " ++ (toString v) ++ " of " ++ (toString d))
    Just ret -> ret


updateMulti : List comparable -> (Maybe a -> Maybe a) -> Dict comparable a -> Dict comparable a
updateMulti keys f dict =
  List.foldl
    (\k d -> Dict.update k f d)
    dict
    keys


px : Int -> String
px number =
  toString number ++ "px"

-- VECTOR MATH ----------------------


norm  p = p.x * p.x + p.y * p.y
distanceSquared : Position -> Position -> Int
distanceSquared p1 p2 = norm <| p1 /-/ p2
distance p1 p2 = sqrt <| toFloat <| distanceSquared p1 p2



(/+/) p1 p2 = Position (p1.x + p2.x) (p1.y + p2.y)
(/-/) p1 p2 = Position (p1.x - p2.x) (p1.y - p2.y)
