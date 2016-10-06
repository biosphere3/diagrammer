module JackRouting exposing (..)

type Direction = East
               | South
               | West
               | North


type alias Plan = (Direction, Direction)
