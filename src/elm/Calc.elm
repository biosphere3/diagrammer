module Calc exposing (..)

import Dict exposing (Dict)
import Graph exposing (..)

import Model exposing (..)


type alias ModelGraph = Graph NodeValue EdgeValue

type NodeValue
  = JackNode Jack
  | ContainerNode Container

type alias EdgeValue = Flow


getNodeValueID nv =
  case nv of
    JackNode jack -> jack.id
    ContainerNode container -> container.id

makeGraph : Model -> ModelGraph
makeGraph {jackByID, containerByID, flowByID} =
  let
    jacks = jackByID |> Dict.values
    containers = containerByID |> Dict.values
    flows = flowByID |> Dict.values

    nodeValues = (List.map JackNode jacks) ++ (List.map ContainerNode containers)
    nodes = List.map (\n -> Graph.Node (getNodeValueID n) n) nodeValues
    edges = List.map (\e -> Graph.Edge e.containerID e.jackID e) flows  -- TODO: direction
  in
    Graph.fromNodesAndEdges nodes edges


traverse : ModelGraph -> List { a | id : Model.ID } -> List NodeValue
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
    graphPath |> List.reverse |> List.map (.node >> .label)

handleNode : NodeValue -> String
handleNode node =
  case node of
    JackNode jack -> "jack: " ++ jack.name
    ContainerNode container -> "container: " ++ container.name
