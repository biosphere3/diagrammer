module Init exposing (..)


import Dict exposing (Dict)
import Math.Vector2 exposing (..)

import List exposing (..)
import Model exposing (..)
import Shape
import State exposing (Msg(..))
import Util exposing (..)


type alias Flags =
  { library : LibraryDef }

type alias LibraryDef =
  { processes : List ProcessDef }

type alias ProcessDef =
  { name : String
  , excerpt : String
  , image : Maybe String
  , inputs : List JackDef
  , outputs : List JackDef
  }

type alias JackDef =
  { name : String
  , rate : Float
  , units : String
  , per : String
  --, state : Maybe String
  }


init : Flags -> ( Model, Cmd Msg )
init flags =
  let

    (processes, jacks) =
      let
        step : ProcessDef -> (List Process, List Jack) -> (List Process, List Jack)
        step def (ps, js) =
          parseProcessWithJacks def |> \(p, js') -> (p :: ps, js ++ js')
      in
        List.foldl step ([], []) flags.library.processes

    processByID : ProcessDict
    processByID = processes |> toDictByID

    jackByID : JackDict
    jackByID = jacks |> toDictByID

    linkByID : LinkDict
    linkByID = Dict.empty

    containerByID : ContainerDict
    containerByID =
      containers |> (List.map mkContainer) |> toDictByID
  in
    (
      { processByID = processByID
      , jackByID = jackByID
      , containerByID = containerByID
      , linkByID = linkByID
      , epoch = 0
      , drag = Nothing
      , globalTransform = { translate = vec2 0 300, scale = 1.0}
      , calcCache = Dict.empty
      }
      , Cmd.none
    )

parseProcessWithJacks : ProcessDef -> (Process, List Jack)
parseProcessWithJacks ({inputs, outputs} as def) =
  let
    process = parseProcess def
    parseInput = parseJack process Input
    parseOutput = parseJack process Output
    jacks = (indexedMap parseInput inputs) ++ (indexedMap parseOutput outputs)
  in
    (process, jacks)

parseProcess : ProcessDef -> Process
parseProcess {name, excerpt, image} =
  { id = generateProcessID name
  , name = name
  , description = excerpt
  , imageURL = image
  , position = vec2 200 200
  , rect = (160, 160)
  }

parseJack : Process -> JackDirection -> Int -> JackDef -> Jack
parseJack  process direction order {name, rate, units, per} =
  let
    h = snd Shape.jackDimensions
    y = toFloat <| h - h * order
    offset = case direction of
      Input -> vec2 -180 y
      Output -> vec2 80 y
  in
    { id = generateJackID process.name name direction
    , name = name
    , processID = process.id
    , rate = rate
    , direction = direction
    , position = process.position `add` offset
    , rect = Shape.jackDimensions
    }


containers =
  [ { id = generateContainerID "Sun", name = "Sun", position = vec2 600 100}
  ]

mkContainer : { a | name : String, position : Vec2 }  -> Container
mkContainer {name, position} =
  { id = generateContainerID name
  , name = name
  , position = position
  , radius = 100
  , capacity = 1/0
  , initialAmount = 1/0
  }
