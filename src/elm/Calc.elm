module Calc exposing (getCalc)

import Dict exposing (Dict)

import Model exposing (..)
import Util exposing (..)
import Purememo exposing (purememo)


applyLink : Link -> Calc -> Calc
applyLink link ({jackByID, containerByID} as calc) =
  let
    jack = seize link.jackID jackByID
    doLink container =
      { container | amount = container.amount + jack.flow }
  in
    { calc | containerByID = Dict.update link.containerID (Maybe.map doLink) containerByID }


updateFlows : List Jack -> Calc -> Calc
updateFlows jacks ({jackByID} as calc) =
  let
    setFlow : Jack -> JackCalc -> JackCalc
    setFlow jack jackCalc =
      { jackCalc | flow = getJackFlow jack }

    -- updateJack : Jack -> JackDict -> JackDict
    updateJack jack jackByID =
      Dict.update jack.id (Maybe.map (setFlow jack)) jackByID
  in
    { calc | jackByID = List.foldl updateJack jackByID jacks }


updateContainers : Calc -> Calc
updateContainers ({linkByID} as calc) =
  let
    links = Dict.values linkByID
  in
    List.foldl applyLink calc links

updateEpoch : Calc -> Calc
updateEpoch calc =
  { calc | epoch = calc.epoch + 1 }

stepEpoch : Model -> Calc -> Calc
stepEpoch ({jackByID} as model) calc =
  let
    jacks = Dict.values jackByID
    inputs = List.filter (\j -> j.direction == Input) jacks
    outputs = List.filter (\j -> j.direction == Output) jacks
  in
    calc
      |> updateEpoch
      |> updateFlows inputs
      |> updateFlows outputs
      |> updateContainers

getInitialCalc : Model -> Calc
getInitialCalc model =
  let
    jackByID = model.jackByID |> Dict.map (\i j -> { flow = 0})
    containerByID = model.containerByID |> Dict.map (\i c -> { amount = 0})
    linkByID = model.linkByID
  in
    { jackByID = jackByID
    , containerByID = containerByID
    , linkByID = linkByID
    , epoch = 0
    }

getCalc : Model -> (Calc, CalcCache)
getCalc model =
  let
    initial = getInitialCalc model
    step = purememo .epoch (stepEpoch model)
    (calc, cache) = Purememo.repeat step model.calcCache model.epoch initial
  in
    (calc, cache)

reduceCalc : (Calc -> Calc) -> Calc -> Int -> Calc
reduceCalc step calc epoch =
  if epoch == 0 then
    calc
  else
    step (reduceCalc step calc (epoch - 1))

--f : Model -> Model
--f ({containerByID} as model) =
--  let
--    {getJack, getContainer, getProcess} = getGetters model
--    suns = containerByID |> Dict.values |> List.filter (\c -> c.name == "Sun")
--    graph = makeGraph model
--    path = traverse graph suns
--    step ctx =
--      case ctx of
--        ProcessNode processID ->
--          let
--            jacks = getJacksByProcessID model processID
--        ContainerNode containerID ->
--    _ = path |> List.map step
--  in
--    model
