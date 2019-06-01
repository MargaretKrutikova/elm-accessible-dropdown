module Select exposing (Msg, OutMsg(..), State, UpdateConfig, ViewConfig, customInitialState, initialState, update, updateConfig, view)

import Array exposing (..)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, src, style, tabindex, value)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode as Decode
import List exposing (..)
import Task



-- MODEL


type DropdownVisibility
    = Visible
    | Hidden


type Status
    = Open DropdownVisibility
    | Closed


type alias State =
    { status : Status
    , dropdownId : String -- TODO: consider DropdownId
    , focusedIndex : Int
    }


initialState : String -> State
initialState dropdownId =
    customInitialState { open = False, dropdownId = dropdownId }


customInitialState :
    { open : Bool
    , dropdownId : String
    }
    -> State
customInitialState { open, dropdownId } =
    { status = initialStatus open
    , dropdownId = dropdownId
    , focusedIndex = -1
    }


initialStatus : Bool -> Status
initialStatus open =
    if open then
        Open Visible

    else
        Closed


type KeyPressed
    = Up
    | Down
    | Enter
    | Escape
    | Other



-- UPDATE


type OutMsg option
    = SelectedOption option


type Msg option
    = Toggle
    | Close
    | SelectOption option
    | KeyDown KeyPressed
    | ScrolledIntoSelected


type UpdateConfig msg option
    = UpdateConfig
        { closeOnSelect : Bool
        , options : Array option
        , selectedId : String
        , toId : option -> String
        }


updateConfig :
    { closeOnSelect : Bool
    , options : Array option
    , selectedId : String
    , toId : option -> String
    }
    -> UpdateConfig msg option
updateConfig { closeOnSelect, options, selectedId, toId } =
    UpdateConfig
        { closeOnSelect = closeOnSelect
        , options = options
        , selectedId = selectedId
        , toId = toId
        }


update : UpdateConfig msg option -> Msg option -> State -> ( State, Cmd (Msg option), Maybe (OutMsg option) )
update config msg state =
    case msg of
        Toggle ->
            case state.status of
                Open _ ->
                    ( { state | status = Closed }, Cmd.none, Nothing )

                Closed ->
                    openDropdown config state

        Close ->
            ( { state | status = Closed }, Cmd.none, Nothing )

        SelectOption option ->
            onSelectOption config state option

        ScrolledIntoSelected ->
            case state.status of
                Open _ ->
                    ( { state | status = Open Visible }, Cmd.none, Nothing )

                Closed ->
                    ( state, Cmd.none, Nothing )

        KeyDown key ->
            case state.status of
                Closed ->
                    handleKeyWhenClosed key config state

                Open Visible ->
                    handleKeyWhenOpen key config state

                Open Hidden ->
                    ( state, Cmd.none, Nothing )


openDropdown : UpdateConfig msg option -> State -> ( State, Cmd (Msg option), Maybe (OutMsg option) )
openDropdown config state =
    ( { state | status = Open Hidden, focusedIndex = getSelectedOptionIndex config }
    , scrollToSelectedOption config state
    , Nothing
    )


onSelectOption : UpdateConfig msg option -> State -> option -> ( State, Cmd (Msg option), Maybe (OutMsg option) )
onSelectOption (UpdateConfig { closeOnSelect }) state option =
    let
        updatedState =
            if closeOnSelect then
                { state | status = Closed }

            else
                state
    in
    ( updatedState, Cmd.none, Just (SelectedOption option) )


handleKeyWhenClosed : KeyPressed -> UpdateConfig msg option -> State -> ( State, Cmd (Msg option), Maybe (OutMsg option) )
handleKeyWhenClosed key config state =
    case key of
        Up ->
            openDropdown config state

        Down ->
            openDropdown config state

        _ ->
            ( state, Cmd.none, Nothing )


handleKeyWhenOpen : KeyPressed -> UpdateConfig msg option -> State -> ( State, Cmd (Msg option), Maybe (OutMsg option) )
handleKeyWhenOpen key ((UpdateConfig { options }) as config) state =
    case key of
        Enter ->
            case options |> Array.get state.focusedIndex of
                Just option ->
                    onSelectOption config state option

                Nothing ->
                    ( state, Cmd.none, Nothing )

        Up ->
            ( { state | focusedIndex = moveIndexUp options state.focusedIndex }, Cmd.none, Nothing )

        Down ->
            ( { state | focusedIndex = moveIndexDown options state.focusedIndex }, Cmd.none, Nothing )

        Escape ->
            ( { state | status = Closed }, Cmd.none, Nothing )

        _ ->
            ( state, Cmd.none, Nothing )


moveIndexDown : Array option -> Int -> Int
moveIndexDown options currentIndex =
    if currentIndex >= (options |> Array.length) - 1 then
        0

    else
        currentIndex + 1


moveIndexUp : Array option -> Int -> Int
moveIndexUp options currentIndex =
    if currentIndex < -1 then
        (options |> Array.length) - 1

    else
        currentIndex - 1


getSelectedOptionIndex : UpdateConfig msg option -> Int
getSelectedOptionIndex (UpdateConfig { options, selectedId, toId }) =
    options
        |> Array.toIndexedList
        |> List.filter (\( index, option ) -> selectedId == toId option)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1



-- VIEW


type alias ViewConfig option =
    { toId : option -> String
    , selectedId : String
    , toLabel : option -> String
    , options : Array option
    , placeholder : String
    }


view : ViewConfig option -> State -> Html (Msg option)
view config state =
    div
        [ id state.dropdownId
        , class "dropdown"
        , on "focusout" (onFocusOut state)
        , keyDecoder state |> preventDefaultOn "keydown"
        ]
        [ button
            [ class "dropdown-button", onClick Toggle ]
            [ text <| getButtonText config ]
        , case state.status of
            Open visibility ->
                div
                    ([ class "dropdown-options-container"
                     , id (getOptionsContainerId state)
                     ]
                        ++ getVisibilityStyle visibility
                    )
                    [ ul [ class "dropdown-options", tabindex -1, attribute "role" "listbox" ]
                        (config.options
                            |> Array.indexedMap Tuple.pair
                            |> Array.map (viewOption config state)
                            |> Array.toList
                        )
                    ]

            Closed ->
                text ""
        ]


viewOption : ViewConfig option -> State -> ( Int, option ) -> Html (Msg option)
viewOption { selectedId, toId, toLabel } state ( index, option ) =
    li
        ([ attribute "role" "option"
         , id (toId option |> getOptionElementId state)
         , onClick (SelectOption option)
         , class "dropdown-option"
         , classList
            [ ( "dropdown-option--selected", toId option == selectedId )
            , ( "dropdown-option--focused", state.focusedIndex == index )
            ]
         ]
            ++ (if state.focusedIndex == index then
                    [ attribute "aria-selected" "true" ]

                else
                    []
               )
        )
        [ text <| toLabel option ]


getVisibilityStyle : DropdownVisibility -> List (Html.Attribute (Msg option))
getVisibilityStyle visibility =
    case visibility of
        Visible ->
            [ style "opacity" "1" ]

        Hidden ->
            [ style "opacity" "0" ]


getSelectedOption : ViewConfig option -> Maybe option
getSelectedOption { options, selectedId, toId } =
    options
        |> Array.filter (\option -> toId option == selectedId)
        |> Array.get 0


getButtonText : ViewConfig option -> String
getButtonText config =
    getSelectedOption config
        |> Maybe.map config.toLabel
        |> Maybe.withDefault config.placeholder


getOptionElementId state optionId =
    state.dropdownId ++ "-" ++ optionId


getOptionsContainerId state =
    state.dropdownId ++ "-options-container"



-- EVENTS


onFocusOut : State -> Decode.Decoder (Msg option)
onFocusOut state =
    outsideTarget "relatedTarget" state.dropdownId


outsideTarget : String -> String -> Decode.Decoder (Msg option)
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
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]



-- scroll to selected


scrollToSelectedOption : UpdateConfig msg option -> State -> Cmd (Msg option)
scrollToSelectedOption (UpdateConfig { selectedId }) state =
    Task.map2
        getOptionScrollPosition
        (Dom.getElement (getOptionElementId state selectedId))
        (Dom.getElement (getOptionsContainerId state))
        |> Task.andThen
            (\top ->
                Dom.setViewportOf (getOptionsContainerId state) 0 top
            )
        |> Task.attempt
            (\_ -> ScrolledIntoSelected)


getOptionScrollPosition : Dom.Element -> Dom.Element -> Float
getOptionScrollPosition { element } containerElement =
    element.y - containerElement.element.y - element.height


keyDecoder : State -> Decode.Decoder ( Msg option, Bool )
keyDecoder state =
    Decode.field "key" Decode.string
        |> Decode.map toKeyPressed
        |> Decode.map (\key -> ( KeyDown key, shouldPreventDefault state key ))


shouldPreventDefault state key =
    case state.status of
        Closed ->
            False

        Open _ ->
            key == Enter


toKeyPressed : String -> KeyPressed
toKeyPressed key =
    case key of
        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        "Escape" ->
            Escape

        "Enter" ->
            Enter

        _ ->
            Other
