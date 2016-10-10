module Init exposing (..)


import Dict exposing (Dict)
import FNV
import Math.Vector2 exposing (..)

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

    flowByID : FlowDict
    flowByID = Dict.empty

    containerByID : ContainerDict
    containerByID =
      containers |> (List.map mkContainer) |> toDictByID
  in
    (
      { processByID = processByID
      , jackByID = jackByID
      , containerByID = containerByID
      , flowByID = flowByID
      , epoch = 0
      , drag = Nothing
      , globalTransform = { translate = vec2 0 0, scale = 1.0}
      }
      , Cmd.none
    )

parseProcessWithJacks : ProcessDef -> (Process, List Jack)
parseProcessWithJacks ({inputs, outputs} as def) =
  let
    process = parseProcess def
    parseInput = parseJack process Input
    parseOutput = parseJack process Output
    jacks = (List.map parseInput inputs) ++ (List.map parseOutput outputs)
  in
    (process, jacks)

parseProcess : ProcessDef -> Process
parseProcess {name, excerpt, image} =
  { id = 110000000000 + FNV.hashString name
  , name = name
  , description = excerpt
  , imageURL = image
  , position = vec2 200 200
  , rect = (160, 160)
  }

parseJack : Process -> JackDirection -> JackDef -> Jack
parseJack  process direction {name, rate, units, per} =
  let
    offset = case direction of
      Input -> vec2 -200 50
      Output -> vec2 100 50
  in
    { id = 220000000000 + FNV.hashString name
    , name = name
    , processID = process.id
    , rate = rate
    , direction = direction
    , position = process.position `add` offset
    , rect = Shape.jackDimensions
    }


containers =
  [ { id = FNV.hashString "Sun", name = "Sun", position = vec2 600 100}
  ]

mkContainer : { a | name : String, position : Vec2 }  -> Container
mkContainer {name, position} = Container (440000000000 + FNV.hashString name) name position (160, 160)
