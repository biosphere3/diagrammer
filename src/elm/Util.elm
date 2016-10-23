module Util exposing (..)

import Math.Vector2 exposing (..)
import Mouse exposing (Position)
import Debug
import Dict exposing (Dict)


toDictByID : List { a | id : Int } -> Dict Int { a | id : Int }
toDictByID seq =
    let
        f i data =
            if data.id > 0 then
                ( data.id, data )
            else
                ( i, { data | id = i } )
    in
        seq
            |> List.indexedMap f
            |> Dict.fromList


seize : comparable -> Dict comparable b -> b
seize v d =
    case Dict.get v d of
        Nothing ->
            (Debug.crash <| "Failed lookup: " ++ (toString v) ++ " of " ++ (toString d))

        Just ret ->
            ret


fromJust ref m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.crash ref


updateMulti : List comparable -> (Maybe a -> Maybe a) -> Dict comparable a -> Dict comparable a
updateMulti keys f dict =
    List.foldl
        (\k d -> Dict.update k f d)
        dict
        keys


fn2 : String -> a -> a -> String
fn2 name x y =
    "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"


px : Int -> String
px number =
    toString number ++ "px"


repeat : Int -> a -> List a
repeat n v =
    let
        r : List a -> ( Int, a ) -> List a
        r vs ( n, v ) =
            if n == 0 then
                vs
            else
                r (v :: vs) ( n - 1, v )
    in
        r [] ( n, v )


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( x :: xs', y :: ys' ) ->
            ( x, y ) :: zip xs' ys'

        ( _, _ ) ->
            []


tupmap2 : (a -> b) -> ( a, a ) -> ( b, b )
tupmap2 f ( x, y ) =
    ( f x, f y )


tupmap3 : (a -> b) -> ( a, a, a ) -> ( b, b, b )
tupmap3 f ( x, y, z ) =
    ( f x, f y, f z )



-- VECTOR MATH ----------------------


centroid ps =
    let
        sum =
            List.foldl add (vec2 0 0) ps

        n =
            toFloat <| List.length ps
    in
        scale (1.0 / n) sum
