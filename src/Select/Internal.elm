module Select.Internal exposing
    ( Customizations
    , HtmlDetails
    , Msg
    , OptionStatus
    , State
    , UpdateConfig
    , ViewConfig
    , closeDropdown
    , customViewConfig
    , defaultCustomizations
    , defaultKeyPress
    , initialState
    , openDropdown
    , update
    , updateConfig
    , view
    , viewConfig
    )

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Attribute, Html, button, div, input, li, text, ul)
import Html.Attributes as Attrs
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import List
import Select.Keyboard exposing (KeyPressed(..), NavigationStrategy(..), toKeyPressed)
import Select.ScrollUtils exposing (scrollToElement)
import Task



-- MODEL


type Visibility
    = Visible
    | Hidden


type Status
    = Open Visibility
    | Closed


type alias State =
    { status : Status
    , focusedIndex : Maybe Int
    , rootElementId : String
    }


initialState :
    { open : Bool
    , id : String
    }
    -> State
initialState { open, id } =
    { status = toStatus open
    , focusedIndex = Nothing
    , rootElementId = id
    }


toStatus : Bool -> Status
toStatus open =
    if open then
        Open Visible

    else
        Closed



-- UPDATE


type Msg
    = Close
    | OpenSelect
    | OptionClick String
    | ScrolledIntoSelected
    | KeyPress KeyPressed
    | ToggleClick
    | NoOp


type alias UpdateConfig option msg =
    { toId : option -> String
    , navigation : NavigationStrategy
    , onMouseClick : String -> Maybe msg
    , onKeyPress : KeyPressed -> Maybe String -> Maybe msg
    }


updateConfig :
    { toId : option -> String
    , navigation : NavigationStrategy
    , onMouseClick : Maybe (String -> Maybe msg)
    , onKeyPress : Maybe (KeyPressed -> Maybe String -> Maybe msg)
    }
    -> UpdateConfig option msg
updateConfig { toId, navigation, onMouseClick, onKeyPress } =
    { toId = toId
    , navigation = navigation
    , onMouseClick = onMouseClick |> Maybe.withDefault (\_ -> Nothing)
    , onKeyPress = onKeyPress |> Maybe.withDefault (\_ _ -> Nothing)
    }


defaultKeyPress : (String -> Maybe msg) -> KeyPressed -> Maybe String -> Maybe msg
defaultKeyPress msg key id =
    Maybe.map
        (\justId ->
            if key == Enter || key == Space then
                msg justId

            else
                Nothing
        )
        id
        |> Maybe.withDefault Nothing


update :
    UpdateConfig option msg
    -> Msg
    -> State
    -> Array option
    -> (option -> Bool)
    -> ( State, Cmd Msg, Maybe msg )
update config msg state options isSelected =
    case msg of
        Close ->
            closeDropdown state

        OpenSelect ->
            openDropdown config state options isSelected False

        OptionClick optionId ->
            ( state, Cmd.none, config.onMouseClick optionId )

        ToggleClick ->
            case state.status of
                Open _ ->
                    closeDropdown state

                Closed ->
                    openDropdown config state options isSelected False

        ScrolledIntoSelected ->
            case state.status of
                Open _ ->
                    ( { state | status = Open Visible }, Cmd.none, Nothing )

                Closed ->
                    ( state, Cmd.none, Nothing )

        KeyPress key ->
            case state.status of
                Open _ ->
                    handleKeyWhenOpen config state options isSelected key

                Closed ->
                    handleKeyWhenClosed config state options isSelected key

        NoOp ->
            ( state, Cmd.none, Nothing )


openDropdown :
    UpdateConfig option msg
    -> State
    -> Array option
    -> (option -> Bool)
    -> Bool
    -> ( State, Cmd Msg, Maybe msg )
openDropdown { toId } state options isSelected shouldFocusSelected =
    let
        selectedId =
            options |> Array.filter isSelected |> Array.get 0 |> Maybe.map toId
    in
    case selectedId of
        Nothing ->
            ( { state | status = Open Visible, focusedIndex = Nothing }, Cmd.none, Nothing )

        Just optionId ->
            ( { state
                | status = Open Hidden
                , focusedIndex =
                    if shouldFocusSelected then
                        findFirstIndex (\option -> toId option == optionId) options

                    else
                        Nothing
              }
            , scrollToOption state.rootElementId optionId ScrolledIntoSelected
            , Nothing
            )


closeDropdown : State -> ( State, Cmd Msg, Maybe msg )
closeDropdown state =
    ( { state | status = Closed, focusedIndex = Nothing }, Cmd.none, Nothing )


handleKeyWhenClosed :
    UpdateConfig option msg
    -> State
    -> Array option
    -> (option -> Bool)
    -> KeyPressed
    -> ( State, Cmd Msg, Maybe msg )
handleKeyWhenClosed config state options isSelected key =
    if key == Up || key == Down || key == Enter || key == Space then
        openDropdown config state options isSelected True

    else
        ( state, Cmd.none, Nothing )


handleKeyWhenOpen :
    UpdateConfig option msg
    -> State
    -> Array option
    -> (option -> Bool)
    -> KeyPressed
    -> ( State, Cmd Msg, Maybe msg )
handleKeyWhenOpen config state options isSelected key =
    case key of
        Up ->
            let
                index =
                    handleKeyUp config state (Array.length options)
            in
            ( { state | focusedIndex = index }
            , scrollToOptionByIndex config state.rootElementId options index
            , Nothing
            )

        Down ->
            let
                index =
                    handleKeyDown config state (Array.length options)
            in
            ( { state | focusedIndex = index }
            , scrollToOptionByIndex config state.rootElementId options index
            , Nothing
            )

        Escape ->
            closeDropdown state

        _ ->
            ( state, Cmd.none, config.onKeyPress key (optionIdByIndex config.toId options state.focusedIndex) )


handleKeyUp : UpdateConfig option msg -> State -> Int -> Maybe Int
handleKeyUp config state total =
    case state.focusedIndex |> Maybe.map (\ind -> ind - 1) of
        Nothing ->
            case config.navigation of
                Circular ->
                    Just (total - 1)

                StayOnBoundary ->
                    Nothing

        Just ind ->
            if ind < 0 then
                case config.navigation of
                    Circular ->
                        Just (total - 1)

                    StayOnBoundary ->
                        Just 0

            else
                Just ind


handleKeyDown : UpdateConfig option msg -> State -> Int -> Maybe Int
handleKeyDown config state total =
    case state.focusedIndex |> Maybe.map (\ind -> ind + 1) of
        Nothing ->
            Just 0

        Just ind ->
            if ind >= total - 1 then
                case config.navigation of
                    Circular ->
                        Just 0

                    StayOnBoundary ->
                        Just (total - 1)

            else
                Just ind


scrollToOptionByIndex : UpdateConfig option msg -> String -> Array option -> Maybe Int -> Cmd Msg
scrollToOptionByIndex { toId } rootElementId options index =
    options
        |> Array.get (Maybe.withDefault -1 index)
        |> Maybe.map (\option -> scrollToOption rootElementId (toId option) NoOp)
        |> Maybe.withDefault Cmd.none


optionIdByIndex : (option -> String) -> Array option -> Maybe Int -> Maybe String
optionIdByIndex toId options index =
    index
        |> Maybe.map (\ind -> Array.get ind options)
        |> Maybe.withDefault Nothing
        |> Maybe.map toId


scrollToOption : String -> String -> Msg -> Cmd Msg
scrollToOption rootElementId optionId msg =
    scrollToElement
        { childId = makeOptionElementId rootElementId optionId
        , containerId = makeContainerElementId rootElementId
        }
        |> Task.attempt (\_ -> msg)



-- VIEW


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias ViewConfig option msg =
    { toId : option -> String
    , toMsg : Msg -> msg
    , classNamespace : Maybe String
    , customizations : Customizations option msg
    }


type alias Customizations option msg =
    { selectAttributes : List (Attribute msg)
    , containerAttributes : List (Attribute msg)
    , listAttributes : List (Attribute msg)
    , viewButton : List option -> HtmlDetails msg
    , viewOption : option -> OptionStatus -> HtmlDetails msg
    }


type alias OptionStatus =
    { selected : Bool
    , focused : Bool
    }


textDetails : String -> HtmlDetails msg
textDetails str =
    HtmlDetails [] [ Html.text str ]


viewConfig :
    { toId : option -> String
    , placeholder : String
    , toLabel : option -> String
    , toMsg : Msg -> msg
    , classNamespace : Maybe String
    }
    -> ViewConfig option msg
viewConfig config =
    { toId = config.toId
    , toMsg = config.toMsg
    , classNamespace = config.classNamespace
    , customizations =
        defaultCustomizations
            { placeholder = config.placeholder
            , toLabel = config.toLabel
            }
    }


customViewConfig :
    { toId : option -> String
    , toMsg : Msg -> msg
    , classNamespace : Maybe String
    , customizations : Customizations option msg
    }
    -> ViewConfig option msg
customViewConfig config =
    { toId = config.toId
    , toMsg = config.toMsg
    , classNamespace = config.classNamespace
    , customizations = config.customizations
    }


defaultCustomizations :
    { placeholder : String
    , toLabel : option -> String
    }
    -> Customizations option msg
defaultCustomizations { placeholder, toLabel } =
    { selectAttributes = []
    , containerAttributes = []
    , listAttributes = []
    , viewButton = textDetails << toButtonText placeholder toLabel
    , viewOption = \option _ -> toLabel option |> textDetails
    }


toButtonText : String -> (option -> String) -> List option -> String
toButtonText placeholder toLabel selectedOptions =
    case selectedOptions of
        [] ->
            placeholder

        [ single ] ->
            toLabel single

        options ->
            options
                |> List.map toLabel
                |> String.join ", "


view : ViewConfig option msg -> State -> Array option -> (option -> Bool) -> Html msg
view ({ toMsg, toId, customizations } as config) state options isSelected =
    let
        buttonDetails =
            customizations.viewButton (options |> Array.filter isSelected |> Array.toList)

        classes =
            mergeClass config
    in
    div
        ([ Attrs.id state.rootElementId
         , on "focusout" (onFocusOut toMsg state.rootElementId)
         , preventDefaultOn "keydown" (keyDown state.status (KeyPress >> toMsg))
         ]
            ++ classes ""
            ++ customizations.selectAttributes
        )
        [ button
            ([ onClick (toMsg ToggleClick) ]
                ++ classes "-button"
                ++ buttonDetails.attributes
            )
            buttonDetails.children
        , viewContainer config state options isSelected
        ]


viewContainer : ViewConfig option msg -> State -> Array option -> (option -> Bool) -> Html msg
viewContainer ({ customizations } as config) state options isSelected =
    case state.status of
        Open visibility ->
            let
                mandatoryAttrs =
                    [ Attrs.id (makeContainerElementId state.rootElementId) ]
                        ++ makeContainerStyle visibility
            in
            div
                (mandatoryAttrs ++ mergeClass config "-container" ++ customizations.containerAttributes)
                [ viewList config state options isSelected ]

        Closed ->
            text ""


viewList : ViewConfig option msg -> State -> Array option -> (option -> Bool) -> Html msg
viewList config state options isSelected =
    ul
        (mergeClass config "-list"
            ++ [ Attrs.tabindex -1
               , Attrs.attribute "role" "listbox"
               ]
        )
        (options
            |> Array.toIndexedList
            |> List.map (viewOptionByIndex config state isSelected)
        )


viewOptionByIndex : ViewConfig option msg -> State -> (option -> Bool) -> ( Int, option ) -> Html msg
viewOptionByIndex config state isSelected ( index, option ) =
    viewOption config
        state
        { selected = isSelected option
        , focused = index == Maybe.withDefault -1 state.focusedIndex
        }
        option


viewOption : ViewConfig option msg -> State -> OptionStatus -> option -> Html msg
viewOption ({ toId, toMsg, customizations } as config) { rootElementId } { selected, focused } option =
    let
        optionDetails =
            customizations.viewOption option { selected = selected, focused = focused }

        mandatoryAttrs =
            [ Attrs.id (makeOptionElementId rootElementId (toId option))
            , onClick (OptionClick (toId option) |> toMsg)
            , Attrs.attribute "role" "option"
            ]
                ++ (if focused then
                        [ Attrs.attribute "aria-selected" "true" ]

                    else
                        []
                   )

        classList =
            mergeClassList config
                [ ( "-option", True )
                , ( "-option--selected", selected )
                , ( "-option--focused", focused )
                ]
    in
    li (optionDetails.attributes ++ classList ++ mandatoryAttrs)
        optionDetails.children



-- VIEW HELPERS


makeContainerStyle : Visibility -> List (Html.Attribute msg)
makeContainerStyle visibility =
    case visibility of
        Visible ->
            [ Attrs.style "opacity" "1", Attrs.style "scroll-behavior" "smooth" ]

        Hidden ->
            [ Attrs.style "opacity" "0" ]


makeOptionElementId id optionId =
    id ++ "-" ++ optionId


makeContainerElementId id =
    id ++ "-options-container"


defaultNamespace =
    "elm-select"


mergeClass : ViewConfig option msg -> String -> List (Attribute msg)
mergeClass { classNamespace } class =
    [ makeClass defaultNamespace class ]
        ++ (classNamespace
                |> Maybe.map (\ns -> [ makeClass ns class ])
                |> Maybe.withDefault []
           )


mergeClassList : ViewConfig option msg -> List ( String, Bool ) -> List (Attribute msg)
mergeClassList { classNamespace } class =
    [ makeClassList defaultNamespace class ]
        ++ (classNamespace
                |> Maybe.map (\ns -> [ makeClassList ns class ])
                |> Maybe.withDefault []
           )


makeClass : String -> String -> Attribute msg
makeClass classNamespace class =
    Attrs.class (classNamespace ++ class)


makeClassList : String -> List ( String, Bool ) -> Attribute msg
makeClassList classNamespace cs =
    List.map (\( class, cond ) -> ( classNamespace ++ class, cond )) cs
        |> Attrs.classList



-- EVENTS


onFocusOut : (Msg -> msg) -> String -> Decode.Decoder msg
onFocusOut toMsg id =
    outsideTarget "relatedTarget" id
        |> Decode.map toMsg


outsideTarget : String -> String -> Decode.Decoder Msg
outsideTarget targetName rootElementId =
    Decode.field targetName (isOutsideDropdown rootElementId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed Close

                else
                    Decode.fail "inside select"
            )


isOutsideDropdown : String -> Decode.Decoder Bool
isOutsideDropdown rootElementId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if rootElementId == id then
                        Decode.succeed False

                    else
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDropdown rootElementId |> Decode.field "parentNode")
        , Decode.succeed True
        ]



-- KEYBOARD NAVIGATION


keyDown : Status -> (KeyPressed -> msg) -> Decode.Decoder ( msg, Bool )
keyDown status toMsg =
    Decode.map
        (\key -> ( toMsg key, shouldPreventDefault status key ))
        (Decode.field "key" Decode.string
            |> Decode.map toKeyPressed
        )


shouldPreventDefault : Status -> KeyPressed -> Bool
shouldPreventDefault status key =
    -- prevent default on Up and Down which will cause the whole screen to scroll,
    -- and on Enter and Space which will cause the select to close
    key == Up || key == Down || key == Space || key == Enter



-- HELPERS


findFirstIndex : (option -> Bool) -> Array option -> Maybe Int
findFirstIndex predicate array =
    Array.foldl
        (\item ->
            \( index, matchInd ) ->
                case ( matchInd, predicate item ) of
                    ( Just ind, _ ) ->
                        ( index, Just ind )

                    ( Nothing, True ) ->
                        ( index, Just index )

                    ( Nothing, False ) ->
                        ( index + 1, Nothing )
        )
        ( 0, Nothing )
        array
        |> Tuple.second
