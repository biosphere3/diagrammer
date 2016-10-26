module State exposing (..)

import Focus exposing (..)
import Math.Vector2 exposing (..)
import Mouse
import Time exposing (..)
import Calc
import Dict exposing (..)
import Foci exposing (..)
import Model exposing (..)
import String
import Util exposing (..)


textOffsetRate =
    1.0


simulationTickInterval =
    100 * millisecond


hardcodedContainerCapacity =
    1000


type Msg
    = DragStart Draggable Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position
    | DragEndTargetJack Jack
    | DragEndTargetContainer Container
    | RemoveLink Link
    | RemoveContainer Container
    | MouseWheelTurn Mouse.Position ( Int, Int ) Float
    | SetEpoch Int
    | SetPlaying Bool
    | SetJacksVisible Bool
    | TickAnimation Time
    | TickSimulation Time
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( manageCache msg <| updateHelp msg <| model
    , Cmd.none
    )


{-| Populate the calcCache appropriately based on the Msg
Note that next time getCalc is called, it will just be a Dict lookup
-}
manageCache : Msg -> Model -> Model
manageCache msg model =
    let
        calc model =
            { model
                | calcCache = snd <| Calc.getCalc model
            }

        clear model =
            { model
                | calcCache = Dict.empty
            }

        recalc =
            calc << clear
    in
        case msg of
            DragStart _ _ ->
                model

            DragAt _ ->
                model

            MouseWheelTurn _ _ _ ->
                model

            TickAnimation _ ->
                model

            TickSimulation _ ->
                model

            SetPlaying _ ->
                model

            SetEpoch _ ->
                calc model

            DragEnd _ ->
                recalc model

            DragEndTargetJack _ ->
                recalc model

            DragEndTargetContainer _ ->
                recalc model

            RemoveLink _ ->
                recalc model

            RemoveContainer _ ->
                recalc model

            _ ->
                recalc model


connectJacks model dragJack jack =
    let
        jacks =
            [ dragJack, jack ]

        newContainer =
            let
                newPos =
                    centroid <| List.map .position jacks

                containerName =
                    dragJack.name
            in
                { id = generateContainerID containerName
                , name = containerName
                , position = newPos
                , radius = 62
                , capacity = hardcodedContainerCapacity
                , initialAmount = 0
                }

        model' =
            { model
                | containerByID = Dict.insert newContainer.id newContainer model.containerByID
            }

        pairs : List ( Container, Jack )
        pairs =
            List.map ((,) newContainer) jacks

        model'' =
            List.foldl
                addLink
                model'
                pairs
    in
        model''


updateHelp : Msg -> Model -> Model
updateHelp msg ({ processByID, jackByID, containerByID, linkByID, drag } as model) =
    let
        { getProcess, getJack, getContainer, getLink } =
            getGetters model
    in
        case msg of
            Noop ->
                model

            DragStart target xy ->
                { model | drag = (Just (Drag xy xy target)) }

            DragAt xy ->
                { model | drag = (Maybe.map (\{ start, target } -> Drag start xy target) model.drag) }

            DragEndTargetJack jack ->
                case drag of
                    Just drag ->
                        case drag.target of
                            DragJack dragJack ->
                                connectJacks model dragJack jack

                            _ ->
                                model

                    Nothing ->
                        model

            DragEndTargetContainer container ->
                case drag of
                    Just drag ->
                        case drag.target of
                            DragJack dragJack ->
                                addLink ( container, dragJack ) model

                            _ ->
                                model

                    Nothing ->
                        model

            DragEnd _ ->
                case drag of
                    Nothing ->
                        Debug.crash "not gonna happen"

                    Just drag ->
                        let
                            model' =
                                { model | drag = Nothing }

                            updateProcessPosition process =
                                { process | position = getProcessPosition model process }

                            updateJackPosition jack =
                                { jack | position = getJackPosition model jack }

                            updateContainerPosition container =
                                { container | position = getContainerPosition model container }
                        in
                            case drag.target of
                                DragScreen ->
                                    Focus.update (globalTransform => translate) (add (dragOffset drag)) model'

                                DragProcess dragProcess ->
                                    let
                                        attachedJacks =
                                            getJacks model dragProcess
                                    in
                                        { model'
                                            | processByID = processByID |> Dict.update dragProcess.id (Maybe.map updateProcessPosition)
                                            , jackByID = jackByID |> updateMulti (List.map .id attachedJacks) (Maybe.map updateJackPosition)
                                        }

                                DragContainer dragContainer ->
                                    { model'
                                        | containerByID = containerByID |> Dict.update dragContainer.id (Maybe.map updateContainerPosition)
                                    }

                                DragJack dragJack ->
                                    model'

            RemoveLink link ->
                removeLink link model

            RemoveContainer container ->
                removeContainer container model

            MouseWheelTurn position ( width, height ) delta ->
                let
                    ratio =
                        Debug.log "delta" <| 1 + delta / 100
                in
                    Focus.update globalTransform (\xf -> { xf | scale = xf.scale * ratio }) model

            SetPlaying playing ->
                { model | playing = playing }

            SetJacksVisible visible ->
                { model | jacksVisible = visible }

            SetEpoch epoch ->
                { model
                    | epoch = epoch
                }

            TickSimulation t ->
                if not model.playing then
                    model
                else
                    let
                        next =
                            updateHelp (SetEpoch (model.epoch + 1)) model
                    in
                        if isValidSimulationState next then
                            next
                        else
                            updateHelp (SetPlaying False) model

            TickAnimation t ->
                let
                    rate jack =
                        case jack.direction of
                            Input ->
                                -textOffsetRate

                            Output ->
                                textOffsetRate

                    u _ link =
                        let
                            offsetRate =
                                Dict.get link.jackID model.jackByID |> Maybe.map rate |> Maybe.withDefault 0
                        in
                            { link | textOffset = link.textOffset + offsetRate }

                    linkByID' =
                        linkByID |> Dict.map u
                in
                    { model | linkByID = linkByID' }



--getHuman model =
--  model.processByID
--  |> List.filter \p -> p.name == "Human"
--  |> List.head
--
--isValidSimulationState model calc =
--  case getHuman model of
--    Nothing -> True
--    Just human ->
--      let
--        inputs = getJacksByProcessID human.id |> List.filter (\j -> j.direction == Input)
--        flows =
--          calc.jackByID
--          |> Dict.filter (\id _ -> List.member id (List.map .id inputs))
--          |> Dict.map (\id jack ->)
--          |> List.filter (\{jackID} -> List.member jackID (List.map .id inputs))


isValidSimulationState model =
    model.epoch <= 100


addLink' : ( Container, Jack ) -> Model -> ( Model, Link )
addLink' ( container, jack ) model =
    let
        link =
            { id = container.id + jack.id
            , containerID = container.id
            , jackID = jack.id
            , textOffset = -2000
            }

        linkByID' =
            Dict.insert link.id link model.linkByID

        model' =
            { model | linkByID = linkByID' }
    in
        ( model', link )


addLink : ( Container, Jack ) -> Model -> Model
addLink pair model =
    fst <| addLink' pair model


removeLink : Link -> Model -> Model
removeLink link model =
    { model
        | linkByID = model.linkByID |> Dict.filter (\id _ -> not (id == link.id))
    }


removeContainer : Container -> Model -> Model
removeContainer container model =
    let
        linkByID' =
            model.linkByID
                |> Dict.filter (\id link -> link.containerID /= container.id)
    in
        { model
            | linkByID = linkByID'
            , containerByID = model.containerByID |> Dict.filter (\id _ -> not (id == container.id))
        }


linksEqual : Link -> Link -> Bool
linksEqual a b =
    ( a.jackID, a.containerID ) == ( b.jackID, b.containerID )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragging =
            case model.drag of
                Nothing ->
                    Sub.batch
                        [ Mouse.downs (DragStart DragScreen) ]

                Just _ ->
                    Sub.batch
                        [ Mouse.moves DragAt
                        , Mouse.ups DragEnd
                        ]

        ticking =
            every (33 * millisecond) TickAnimation

        simTick =
            every simulationTickInterval TickSimulation
    in
        Sub.batch
            [ dragging
            , simTick
            ]


nextID : Dict comparable { a | id : ID } -> ID
nextID dict =
    dict
        |> Dict.values
        |> List.map .id
        |> List.maximum
        |> Maybe.withDefault 0
        |> (+) 1


getJacks : Model -> Process -> List Jack
getJacks { jackByID } process =
    jackByID
        |> Dict.values
        |> List.filter (\x -> x.processID == process.id)


joinJacks : Model -> List Jack -> Model
joinJacks model jacks =
    model



-- add a link between these jacks
