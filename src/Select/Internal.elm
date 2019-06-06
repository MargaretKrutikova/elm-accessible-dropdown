module Select.Internal exposing
    ( Msg
    , State
    , UpdateConfig
    , ViewConfig
    , closeDropdown
    , defaultKeyPress
    , initialState
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
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, src, style, tabindex, value)
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
    , idAttribute : String
    }


initialState :
    { open : Bool
    , id : String
    }
    -> State
initialState { open, id } =
    { status = toStatus open
    , focusedIndex = Nothing
    , idAttribute = id
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
            openDropdown config state options isSelected

        OptionClick optionId ->
            ( state, Cmd.none, config.onMouseClick optionId )

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
    -> ( State, Cmd Msg, Maybe msg )
openDropdown { toId } state options isSelected =
    let
        selectedId =
            options |> Array.filter isSelected |> Array.get 0 |> Maybe.map toId
    in
    case selectedId of
        Nothing ->
            ( { state | status = Open Visible, focusedIndex = Nothing }, Cmd.none, Nothing )

        Just optionId ->
            ( { state | status = Open Hidden }
            , scrollToOption state.idAttribute optionId ScrolledIntoSelected
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
    if key == Up || key == Down then
        openDropdown config state options isSelected

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
            , scrollToOptionByIndex config state.idAttribute options index
            , Nothing
            )

        Down ->
            let
                index =
                    handleKeyDown config state (Array.length options)
            in
            ( { state | focusedIndex = index }
            , scrollToOptionByIndex config state.idAttribute options index
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
scrollToOptionByIndex { toId } idAttribute options index =
    options
        |> Array.get (Maybe.withDefault -1 index)
        |> Maybe.map (\option -> scrollToOption idAttribute (toId option) NoOp)
        |> Maybe.withDefault Cmd.none


optionIdByIndex toId options index =
    index
        |> Maybe.map (\ind -> Array.get ind options)
        |> Maybe.withDefault Nothing
        |> Maybe.map toId


scrollToOption : String -> String -> Msg -> Cmd Msg
scrollToOption idAttribute optionId msg =
    scrollToElement
        { childId = getOptionElementId idAttribute optionId
        , containerId = getOptionsContainerId idAttribute
        }
        |> Task.attempt (\_ -> msg)



-- VIEW


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias ViewConfig option msg =
    { toId : option -> String
    , placeholder : String
    , toLabel : option -> String
    , optionDetails : option -> HtmlDetails msg
    , toMsg : Msg -> msg
    }


textOption : String -> HtmlDetails msg
textOption str =
    HtmlDetails [] [ Html.text str ]


viewConfig :
    { toId : option -> String
    , placeholder : Maybe String
    , toLabel : option -> String
    , toMsg : Msg -> msg
    }
    -> ViewConfig option msg
viewConfig config =
    { toId = config.toId
    , placeholder = config.placeholder |> Maybe.withDefault ""
    , optionDetails = textOption << config.toLabel
    , toLabel = config.toLabel
    , toMsg = config.toMsg
    }



--- Add custom attributes to view config


toggleOpen : State -> Msg
toggleOpen state =
    case state.status of
        Open _ ->
            Close

        Closed ->
            OpenSelect


view : ViewConfig option msg -> Array option -> (option -> Bool) -> State -> Html msg
view ({ toMsg, toId } as config) options isSelected ({ idAttribute } as state) =
    div
        [ id idAttribute
        , class "dropdown"
        , on "focusout" (onFocusOut toMsg idAttribute)
        , preventDefaultOn "keydown"
            (Decode.map
                (\key ->
                    ( KeyPress key |> toMsg, shouldPreventDefault state.status key )
                )
                keyDecoder
            )
        ]
        -- [ input [ class "dropdown-button", onClick (toMsg <| toggleOpen state), value query, onInput config.onInput ]
        --     []
        [ button
            [ class "dropdown-button", onClick (toMsg <| toggleOpen state) ]
            [ text (getButtonText config options isSelected) ]
        , case state.status of
            Open visibility ->
                div
                    ([ class "dropdown-options-container"
                     , id (getOptionsContainerId idAttribute)
                     ]
                        ++ getVisibilityStyle visibility
                    )
                    [ viewList config state options isSelected ]

            Closed ->
                text ""
        ]


viewList : ViewConfig option msg -> State -> Array option -> (option -> Bool) -> Html msg
viewList config state options isSelected =
    ul [ class "dropdown-options", tabindex -1, attribute "role" "listbox" ]
        (options
            |> Array.toIndexedList
            |> List.map
                (\( index, option ) ->
                    viewOption config
                        state
                        (isSelected option)
                        (index == Maybe.withDefault -1 state.focusedIndex)
                        option
                )
        )


viewOption : ViewConfig option msg -> State -> Bool -> Bool -> option -> Html msg
viewOption ({ toId, optionDetails, toMsg } as config) { idAttribute } selected focused option =
    li
        ([ attribute "role" "option"
         , id (getOptionElementId idAttribute (toId option))
         , onClick (OptionClick (toId option) |> toMsg)
         , class "dropdown-option"
         , classList
            [ ( "dropdown-option--selected", selected )
            , ( "dropdown-option--focused", focused )
            ]
         ]
            ++ (option |> optionDetails |> .attributes)
            ++ (if focused then
                    [ attribute "aria-selected" "true" ]

                else
                    []
               )
        )
        (option
            |> optionDetails
            |> .children
        )


getVisibilityStyle : Visibility -> List (Html.Attribute msg)
getVisibilityStyle visibility =
    case visibility of
        Visible ->
            [ style "opacity" "1", style "scroll-behavior" "smooth" ]

        Hidden ->
            [ style "opacity" "0" ]


getButtonText : ViewConfig option msg -> Array option -> (option -> Bool) -> String
getButtonText config options isSelected =
    options
        |> Array.filter isSelected
        |> Array.get 0
        |> Maybe.map config.toLabel
        |> Maybe.withDefault config.placeholder


getOptionElementId idAttribute optionId =
    idAttribute ++ "-" ++ optionId


getOptionsContainerId idAttribute =
    idAttribute ++ "-options-container"



-- EVENTS


onFocusOut : (Msg -> msg) -> String -> Decode.Decoder msg
onFocusOut toMsg id =
    outsideTarget "relatedTarget" id
        |> Decode.map toMsg


outsideTarget : String -> String -> Decode.Decoder Msg
outsideTarget targetName dropdownId =
    Decode.field targetName (isOutsideDropdown dropdownId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed Close

                else
                    Decode.fail "inside dropdown"
            )


isOutsideDropdown : String -> Decode.Decoder Bool
isOutsideDropdown dropdownId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if dropdownId == id then
                        Decode.succeed False

                    else
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")
        , Decode.succeed True
        ]



-- KEYBOARD NAVIGATION


keyDecoder : Decode.Decoder KeyPressed
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toKeyPressed


shouldPreventDefault status key =
    case status of
        Open _ ->
            key == Enter || key == Space

        Closed ->
            False



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
