module Calc exposing (..)

import Dict exposing (Dict)
import Graph exposing (..)

import Model exposing (..)
import Util exposing (..)

type alias ModelGraph = Graph NodeValue EdgeValue
type alias ModelNodeContext = Graph.NodeContext NodeValue EdgeValue

type NodeValue
  = ProcessNode ProcessID
  | ContainerNode ContainerID

type alias EdgeValue = FlowID


getNodeValueID model nv =
  let
    {getJack, getContainer} = getGetters model
  in case nv of
    ProcessNode id -> id
    ContainerNode id -> id

makeGraph : Model -> ModelGraph
makeGraph ({processByID, containerByID, jackByID, flowByID} as model) =
  let
    processes = processByID |> Dict.values
    containers = containerByID |> Dict.values
    flows = flowByID |> Dict.values
    makeNode n = Graph.Node (getNodeValueID model n) n
    makeEdge ({containerID, jackID} as flow) =
      let
        jack = seize jackID jackByID
      in case jack.direction of
        Model.Input -> Graph.Edge containerID jack.processID flow.id
        Model.Output -> Graph.Edge jack.processID containerID flow.id
    nodeValues = (++)
      (List.map (ProcessNode << .id) processes)
      (List.map (ContainerNode << .id) containers)
    nodes = List.map makeNode nodeValues
    edges = List.map makeEdge flows
  in
    Graph.fromNodesAndEdges nodes edges


traverse : ModelGraph -> List { a | id : Model.ID } -> List ModelNodeContext
traverse graph roots =
  let
    --visitor ctxs dist acc =
    visitor = Graph.ignorePath (::)
    (graphPath, _) =
      Graph.guidedBfs
        Graph.alongOutgoingEdges
        visitor
        (List.map .id roots)
        []
        graph
  in
    graphPath |> List.reverse


applyFlow : Flow -> Model -> Model
applyFlow flow model =
  let
    {getJack, getContainer, getProcess} = getGetters model
    jack = getJack flow.jackID
    doFlow container =
      { container | amount = container.amount + 1} -- TODO
  in
    { model | containerByID = Dict.update flow.containerID (Maybe.map doFlow) model.containerByID }


updateContainers : Model -> Model
updateContainers ({containerByID, flowByID} as model) =
  let
    flows = Dict.values flowByID
  in
    List.foldl applyFlow model flows

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
