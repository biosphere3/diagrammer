module JackRouting exposing (..)

import Math.Vector2 exposing (..)
import Maybe
import String

import Model exposing (..)
import Shape exposing (Shape(..))
import Util exposing (..)

type Face = East
          | South
          | West
          | North


type alias OrthogonalPlan =
    { distance : Float
    , faces : (Face, Face)
    , contacts : (Vec2, Vec2)
    }

type alias Thing = (Shape, Vec2)


linkPath model process container =
    let
        processPosition = getProcessPosition model process
        containerPosition = getContainerPosition model container
        processShape = Shape.getProcessShape process
        containerShape = Shape.getContainerShape container
        plan =
            orthogonalPlan
                (processShape, processPosition)
                (containerShape, containerPosition)
        (contact1, contact2) = plan.contacts
        (px, py) = toTuple contact1
        (cx, cy) = toTuple contact2
    in
        [ "M", pair px py
        --, "L", pair cx py
        , "L", pair cx cy
        ] |> String.join " "

orthogonalPlan : Thing -> Thing -> OrthogonalPlan
orthogonalPlan ((shape1, position1) as thing1) ((shape2, position2) as thing2) =
    let
        getOffset1 = faceOffset shape1
        getOffset2 = faceOffset shape2
        getPlan (f1, f2) =
            let
                p1 = (contactPoint thing1 f1)
                p2 = (contactPoint thing2 f2)
                d = distance p1 p2
            in
                OrthogonalPlan d (f1, f2) (p1, p2)

        bestPlan =
            possibleFacePairs position1 position2
            |> List.map getPlan
            |> List.sortBy .distance
            |> List.head
            |> fromJust "no plan"
    in
        bestPlan

possibleFacePairs : Vec2 -> Vec2 -> List (Face, Face)
possibleFacePairs p1 p2 =
    let
        (f1a, f2a) = if getX p2 > getX p1
            then (East, West)
            else (West, East)

        (f1b, f2b) = if getY p2 > getY p1
            then (South, North)
            else (North, South)
    in
        [ (f1a, f2a)
        , (f1a, f2b)
        , (f1b, f2a)
        , (f1b, f2b)
        ]

faceUnit : Face -> Vec2
faceUnit face =
    case face of
        East -> vec2 1 0
        South -> vec2 0 1
        West -> vec2 -1 0
        North -> vec2 0 -1

faceOffset : Shape -> Face -> Vec2
faceOffset shape face =
    let
        unit = faceUnit face
    in case shape of
        Circle radius ->
            radius `scale` unit
        Rect width height ->
            vec2 (getX unit * width / 2) (getY unit * height / 2)

contactPoint : (Shape, Vec2) -> Face -> Vec2
contactPoint (shape, position) face =
    position `add` (faceOffset shape face)

pair x y =
    (toString x) ++ "," ++ (toString y)