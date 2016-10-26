module View.ViewField exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseEnter, onMouseLeave, onMouseUp, onDoubleClick)
import Json.Decode as Json exposing ((:=), int, float, object4, object2)
import Math.Vector2 exposing (..)
import Mouse
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Constants exposing (colors)
import Maybe exposing (..)
import Util exposing (..)
import Model exposing (..)
import Calc exposing (getCalc)
import JackRouting
import Shape
import State exposing (Msg(..))


(=>) =
    (,)


view : Model -> Html Msg
view model =
    let
        xf =
            getGlobalTransform model

        ( gx, gy ) =
            toTuple <| .translate <| xf

        scale =
            .scale <| xf

        ( calc, _ ) =
            getCalc model
    in
        Svg.svg
            [ width "100%"
            , height "100%"
            , onMouseWheel'
            , Html.Attributes.style
                [ "background-position" => ((toString gx) ++ " " ++ (toString gy))
                ]
            ]
            [ Svg.g
                [ transform <| " scale(" ++ (toString scale) ++ ")" ++ "translate( " ++ (toString gx) ++ "," ++ (toString gy) ++ " )"
                ]
                [ Svg.g [] (model.linkByID |> Dict.values |> List.map (drawLink model calc))
                , Svg.g [] (model.processByID |> Dict.values |> List.map (drawProcess model calc))
                , Svg.g [] (model.containerByID |> Dict.values |> List.map (drawContainer model calc))
                ]
            ]


drawProcess : Model -> Calc -> Process -> Svg Msg
drawProcess model calc process =
    let
        realPosition =
            toRecord <| getProcessPosition model process

        ( w, h ) =
            process.rect

        imageURL =
            process.imageURL |> withDefault ("http://placekitten.com/" ++ toString w ++ "/" ++ toString h)

        jacks =
            getJacksByProcessID model process.id

        looseJacks =
            jacks |> List.filter (not << isConnected model)

        attrs =
            [ onMouseDown' <| DragProcess process
            , rx "10"
            , ry "10"
            , x (toString <| -w / 2)
            , y (toString <| -h / 2)
            , width (toString w)
            , height (toString h)
            , xlinkHref <| imageURL
            , Html.Attributes.style
                [ "cursor" => "move"
                ]
            ]

        headAttrs =
            [ width (toString w)
            , height "40"
            ]

        textAttrs =
            headAttrs
                ++ [ x (toString <| 0)
                   , y (toString <| 0 - 90)
                   , fontSize "20px"
                   , textAnchor "middle"
                   ]

        boxAttrs =
            headAttrs
                ++ [ x (toString <| 0 - 80)
                   , y (toString <| 0 - 120)
                   , fill "white"
                   , stroke "#ddd"
                   , strokeWidth "1px"
                   ]

        body =
            image attrs []

        textBox =
            rect boxAttrs []

        textContent =
            text' textAttrs [ (text process.name) ]

        head =
            g [] [ textBox, textContent ]

        drawnJacks =
            if not model.jacksVisible then
                []
            else
                (looseJacks |> List.map (drawJack model calc))
    in
        g []
            [ g [ transform <| fn2 "transform" realPosition.x realPosition.y ]
                [ head
                , body
                ]
            , g [] drawnJacks
            ]


drawJack : Model -> Calc -> Jack -> Svg Msg
drawJack model calc jack =
    let
        jackCoords =
            toRecord <| getJackPosition model jack

        x0 =
            toString <| jackCoords.x

        y0 =
            toString <| jackCoords.y

        ( w, h ) =
            jack.rect

        isDragging =
            isDragSubjectID model jack.id

        -- would rather not compute this every time
        color =
            getStateColor jack.matterState

        outline =
            Svg.path
                ([ onMouseDown' <| DragJack jack
                 , onMouseUp <| DragEndTargetJack jack
                 , d <| Shape.chevron Shape.jackDimensions
                 , Html.Attributes.style
                    [ "cursor" => "move"
                    , "fill" => color
                    , "stroke" => "#ddd"
                    , "strokeWidth" => "1"
                    , "pointer-events"
                        => if isDragging then
                            "none"
                           else
                            "auto"
                      -- don't catch mouseUp while dragging
                    ]
                 ]
                )
                []

        contentName =
            text'
                [ x <| toString (w / 2 - 10)
                , y <| toString (-h / 5)
                , alignmentBaseline "middle"
                , textAnchor "middle"
                , fill "white"
                , fontSize "20px"
                ]
                [ text <| jack.name ]

        contentQuantity =
            text'
                [ x <| toString (w / 2 - 10)
                , y <| toString (h / 5)
                , alignmentBaseline "middle"
                , textAnchor "middle"
                , fill "white"
                ]
                [ text <| toString jack.rate ++ " " ++ jack.units ]
    in
        g [ transform <| "translate(" ++ x0 ++ "," ++ y0 ++ ")" ]
            [ outline, contentName, contentQuantity ]


isDragSubjectID model id =
    model.drag
        |> Maybe.map
            (\drag ->
                case drag.target of
                    DragJack dragJack ->
                        dragJack.id == id

                    _ ->
                        False
            )
        |> Maybe.withDefault False


drawContainer : Model -> Calc -> Container -> Svg Msg
drawContainer model calc container =
    let
        realPosition =
            toRecord <| getContainerPosition model container

        radius =
            container.radius

        stringOrInf amount =
            if isInfinite amount then
                "∞"
            else
                toString (round amount)

        -- TODO: don't round, just limit sigfigs
        amount =
            Dict.get container.id calc.containerByID |> Maybe.map .amount

        amountDisplay =
            amount
                |> Maybe.map stringOrInf
                |> Maybe.withDefault "???"

        capacityDisplay =
            (stringOrInf container.capacity)

        isSun =
            container.id == (.id <| getSun model)

        circleColor =
            if isSun then
                "white"
            else
                case amount of
                    Just val ->
                        if val < 0 then
                            colors.death
                        else
                            "white"

                    Nothing ->
                        "white"

        clipID =
            "container-clip-" ++ (toString container.id)

        circleID =
            "container-circle-" ++ (toString container.id)

        circleAttrs =
            [ id circleID
            , cx "0"
            , cy "0"
            , r (toString <| radius)
            , fill circleColor
            , stroke <|
                if isSun then
                    colors.light
                else
                    "black"
            , strokeWidth <|
                if isSun then
                    "60"
                else
                    "3"
            ]

        fillRatio =
            if isInfinite container.capacity then
                0
            else
                amount |> Maybe.map (\a -> a / container.capacity) |> withDefault 0

        fillHeight =
            radius * 2 * fillRatio

        rectAttrs =
            [ x <| toString (-radius)
            , y <| toString (radius - fillHeight)
            , width "500"
            , height <| toString fillHeight
            , fill "#ddd"
            , Svg.Attributes.clipPath <| "url(#" ++ clipID ++ ")"
            ]

        textAttrs =
            [ fontSize "20px"
            , x "0"
            , fill "black"
            , textAnchor "middle"
            , alignmentBaseline "middle"
            ]

        nameAttrs =
            textAttrs ++ [ y "-10", fontSize "20px" ]

        amountAttrs =
            textAttrs ++ [ y "15", fontSize "24px" ]

        capacityAttrs =
            textAttrs ++ [ y "38", fontSize "16px", fill "#666" ]

        dividerAttrs =
            [ x1 "-20", x2 "20", y1 "27", y2 "27", stroke "#666" ]
    in
        g
            [ transform <| fn2 "translate" realPosition.x realPosition.y
            , onDoubleClick <|
                if isSun then
                    Noop
                else
                    (RemoveContainer container)
            , onMouseDown' <| DragContainer container
            , onMouseUp <| DragEndTargetContainer container
            , Html.Attributes.style
                [ "cursor" => "move"
                , "pointer-events"
                    => if (isDragSubjectID model container.id) then
                        "none"
                       else
                        "auto"
                ]
            ]
            [ Svg.clipPath [ id clipID ] [ use [ xlinkHref <| "#" ++ circleID ] [] ]
            , circle circleAttrs []
            , rect rectAttrs []
            , text' nameAttrs [ text container.name ]
            , text' amountAttrs [ text amountDisplay ]
            , line dividerAttrs []
            , text' capacityAttrs [ text capacityDisplay ]
            ]


drawLink : Model -> Calc -> Link -> Svg Msg
drawLink ({ containerByID, jackByID } as model) calc link =
    let
        container =
            seize link.containerID containerByID

        jack =
            seize link.jackID jackByID

        process =
            getJackProcess model jack

        containerCoords =
            toRecord <| getContainerPosition model container

        processCoords =
            toRecord <| getProcessPosition model process

        cx =
            toString <| containerCoords.x

        cy =
            toString <| containerCoords.y

        jx =
            toString <| processCoords.x

        jy =
            toString <| processCoords.y

        flowDisplay =
            Dict.get jack.id calc.jackByID
                |> map (toString << .flow)
                |> withDefault "???"

        stripeColor =
            getStateColor jack.matterState

        textColor =
            getStateContrastColor jack.matterState

        arrowText =
            case jack.direction of
                Input ->
                    "  \x2003❮ ❮ ❮  \x2003"

                Output ->
                    " \x2003❯ ❯ ❯  \x2003"

        linkClass =
            case jack.direction of
                Input ->
                    "input"

                Output ->
                    "output"

        domID =
            "link-" ++ (toString link.id)

        dval =
            JackRouting.linkPath model process container

        displayText =
            (String.toUpper jack.name) ++ " \x2003" ++ flowDisplay ++ " " ++ jack.units

        linkStripe =
            Svg.path
                [ d dval
                , fill "none"
                , stroke stripeColor
                , strokeWidth "25"
                , id domID
                , class "link-stripe"
                ]
                []

        linkLine =
            Svg.path
                [ d dval
                , fill "none"
                , stroke textColor
                , strokeWidth "5"
                , strokeDasharray "10,20"
                , id domID
                , class "link-line"
                ]
                []

        linkText =
            text'
                [ alignmentBaseline "bottom"
                , fill textColor
                , fontSize "14px"
                , fontFamily "Helvetica Neue, sans-serif"
                , dy "5"
                ]
                [ textPath
                    [ xlinkHref <| "#" ++ domID
                    ]
                    [ text <| String.join arrowText <| repeat 100 displayText
                    ]
                ]
    in
        g
            [ class <| "link " ++ linkClass
            , onDoubleClick (RemoveLink link)
            ]
            [ linkStripe
            , linkLine
            , linkText
            ]


getStateColor : MatterState -> String
getStateColor state =
    case state of
        SolidState ->
            colors.solid

        LiquidState ->
            colors.liquid

        GasState ->
            colors.gas

        EnergyState ->
            colors.energy

        LightState ->
            colors.light

        UnspecifiedState ->
            colors.unspecified


getStateContrastColor : MatterState -> String
getStateContrastColor state =
    case state of
        LightState ->
            "black"

        _ ->
            "white"


isConnected : Model -> Jack -> Bool
isConnected model jack =
    let
        link =
            model.linkByID |> Dict.values |> List.filter (\{ jackID } -> jack.id == jackID) |> List.head
    in
        case link of
            Just _ ->
                True

            Nothing ->
                False


onMouseDown' : Draggable -> Attribute Msg
onMouseDown' target =
    on "mousedown" (Json.map (DragStart target) Mouse.position)


onMouseWheel' : Attribute Msg
onMouseWheel' =
    let
        makeMouseWheelMsg : Int -> Int -> Float -> ( Int, Int ) -> Msg
        makeMouseWheelMsg clientX clientY deltaY dimensions =
            MouseWheelTurn (Mouse.Position clientX clientY) dimensions deltaY

        targetDecoder =
            object2 (,) ("width" := int) ("height" := int)

        decoder : Json.Decoder Msg
        decoder =
            object4 makeMouseWheelMsg ("clientX" := int) ("clientY" := int) ("deltaY" := float) ("target" := targetDecoder)
    in
        on "wheel" decoder
