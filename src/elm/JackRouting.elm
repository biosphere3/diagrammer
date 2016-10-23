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

type Axis = AxisX | AxisY

type alias OrthoPlan =
    { distanceSquared : Float
    , faces : (Face, Face)
    , contacts : (Vec2, Vec2)
    }

type alias Thing = (Shape, Vec2)

axis face =
    case face of
        West -> AxisX
        East -> AxisX
        North -> AxisY
        South -> AxisY

{- Order of operations:
    1. find the (Face, Face) part of the plan
    2. for each Face, for each Link, adjust the contact point based on how many other links are attached to this face
        - This looks like sorting links based on their straight-line angle from the central contact point
    3. draw all them Links!
-}

linkPath model process container =
    let
        processPosition = getProcessPosition model process
        containerPosition = getContainerPosition model container
        processShape = Shape.getProcessShape process
        containerShape = Shape.getContainerShape container
        plan =
            orthoPlan
                (processShape, processPosition)
                (containerShape, containerPosition)
    in
        orthoRoute plan

orthoRoute : OrthoPlan -> String
orthoRoute plan =
    let
        cornerRadius = 50
        (face1, face2) = plan.faces
        (contact1, contact2) = plan.contacts
        (px, py) = toTuple contact1
        (cx, cy) = toTuple contact2
        points =
            [ (px, py) ] ++ inflectionPoints plan ++ [ (cx, cy) ]
            |> List.map fromTuple

        triples =
            List.map3
                (\a b c -> (a, b, c))
                (List.drop 0 points)
                (List.drop 1 points)
                (List.drop 2 points)

        f (v0, v1, v2) d =
            let
                dir1 = direction v1 v0
                dir2 = direction v1 v2
                dist1 = distance v1 v0
                dist2 = distance v1 v2
                r = cornerRadius `min` (dist1 / 2) `min` (dist2 / 2)
                t0 = v1 `sub` scale r dir1 |> toTuple
                t1 = v1 |> toTuple
                t2 = v1 `sub` scale r dir2 |> toTuple
            in
                if r == 0 || isInfinite r
                then
                    d ++ " L " ++ pair t1
                else
                    d ++ " L " ++ pair t0 ++ " Q " ++ pair t1 ++ " " ++ pair t2

        dMid = List.foldl f "" triples
    in
        "M " ++ pair (px, py) ++ dMid ++ " L " ++ pair (cx, cy)


inflectionPoints plan =
    let
        ((px, py), (cx, cy)) = tupmap2 toTuple plan.contacts
        axes = tupmap2 axis plan.faces
        hx = (cx + px) / 2
        hy = (cy + py) / 2
    in
        case axes of
            (AxisX, AxisY) ->
                [ (cx, py) ]
            (AxisY, AxisX) ->
                [ (px, cy) ]
            (AxisX, AxisX) ->
                [ (hx, py)
                , (hx, cy)
                ]
            (AxisY, AxisY) ->
                [ (px, hy)
                , (cx, hy)
                ]

orthoPlan : Thing -> Thing -> OrthoPlan
orthoPlan ((shape1, position1) as thing1) ((shape2, position2) as thing2) =
    let
        getOffset1 = faceOffset shape1
        getOffset2 = faceOffset shape2
        getPlan (f1, f2) =
            let
                p1 = (contactPoint thing1 f1)
                p2 = (contactPoint thing2 f2)
                d = distanceSquared p1 p2
            in
                OrthoPlan d (f1, f2) (p1, p2)

        bestPlan =
            possibleFacePairs position1 position2
            |> List.map getPlan
            |> List.sortBy .distanceSquared
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

pair (x, y) =
    (toString x) ++ "," ++ (toString y)