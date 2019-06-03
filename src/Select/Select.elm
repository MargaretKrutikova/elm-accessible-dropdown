module Select.Select exposing
    ( Msg
    , SelectId(..)
    , State
    , UpdateConfig
    , ViewConfig
    , initialState
    , update
    , updateConfig
    , view
    )

import Array exposing (..)
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Attribute, Html, button, div, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, src, style, tabindex, value)
import Html.Events exposing (on, onClick, onInput, preventDefaultOn)
import Json.Decode as Decode
import List exposing (..)
import Select.KeyboardNavigator exposing (..)
import Select.ScrollUtils exposing (..)
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
    , focusedIndex : Maybe Int
    }


initialState :
    { open : Bool
    }
    -> State
initialState { open } =
    { status = initialStatus open
    , focusedIndex = Nothing
    }


initialStatus : Bool -> Status
initialStatus open =
    if open then
        Open Visible

    else
        Closed



-- UPDATE


type SelectId
    = SelectId String


type Msg option
    = Close
    | OpenSelect SelectId
    | SelectOption option
    | ScrolledIntoSelected
    | KeyPress ( SelectId, KeyPressed )
    | NoOp


type alias UpdateConfig option msg =
    { toId : option -> String
    , closeOnSelect : Bool
    , onSelect : option -> msg
    }


updateConfig :
    { toId : option -> String
    , closeOnSelect : Bool
    , onSelect : option -> msg
    }
    -> UpdateConfig option msg
updateConfig { toId, closeOnSelect, onSelect } =
    { toId = toId
    , closeOnSelect = closeOnSelect
    , onSelect = onSelect
    }


type alias Data option =
    { selectedId : String
    , options : Array option
    }


update : UpdateConfig option msg -> Msg option -> State -> Data option -> ( State, Cmd (Msg option), Maybe msg )
update config msg state data =
    case msg of
        Close ->
            ( { state | status = Closed }, Cmd.none, Nothing )

        OpenSelect selectId ->
            openDropdown config selectId state data

        SelectOption option ->
            onSelectOption config state option

        ScrolledIntoSelected ->
            case state.status of
                Open _ ->
                    ( { state | status = Open Visible }, Cmd.none, Nothing )

                Closed ->
                    ( state, Cmd.none, Nothing )

        KeyPress tuple ->
            case state.status of
                Open _ ->
                    handleKeyWhenOpen tuple config state data

                Closed ->
                    handleKeyWhenClosed tuple config state data

        NoOp ->
            ( state, Cmd.none, Nothing )


openDropdown : UpdateConfig option msg -> SelectId -> State -> Data option -> ( State, Cmd (Msg option), Maybe msg )
openDropdown { toId } selectId state { selectedId, options } =
    ( { state
        | status = Open Hidden
        , focusedIndex =
            options
                |> Array.toIndexedList
                |> List.filter (\( index, option ) -> selectedId == toId option)
                |> List.head
                |> Maybe.map Tuple.first
      }
    , scrollToOption selectId selectedId ScrolledIntoSelected
    , Nothing
    )


handleKeyWhenClosed : ( SelectId, KeyPressed ) -> UpdateConfig option msg -> State -> Data option -> ( State, Cmd (Msg option), Maybe msg )
handleKeyWhenClosed ( selectId, key ) config state data =
    case key of
        Up ->
            openDropdown config selectId state data

        Down ->
            openDropdown config selectId state data

        _ ->
            ( state, Cmd.none, Nothing )


handleKeyWhenOpen : ( SelectId, KeyPressed ) -> UpdateConfig option msg -> State -> Data option -> ( State, Cmd (Msg option), Maybe msg )
handleKeyWhenOpen ( selectId, key ) config state ({ options } as data) =
    case key of
        EnterOrSpace ->
            case Maybe.map (\ind -> Array.get ind options) state.focusedIndex |> Maybe.withDefault Nothing of
                Just option ->
                    onSelectOption config state option

                Nothing ->
                    ( state, Cmd.none, Nothing )

        Up ->
            moveFocusedIndex config selectId state data (moveIndexUp (Array.length options) state.focusedIndex)

        Down ->
            moveFocusedIndex config selectId state data (moveIndexDown (Array.length options) state.focusedIndex)

        Escape ->
            ( { state | status = Closed }, Cmd.none, Nothing )

        Other ->
            ( state, Cmd.none, Nothing )


onSelectOption : UpdateConfig option msg -> State -> option -> ( State, Cmd (Msg option), Maybe msg )
onSelectOption { closeOnSelect, onSelect } state option =
    let
        updatedState =
            if closeOnSelect then
                { state | status = Closed }

            else
                state
    in
    ( updatedState, Cmd.none, Just (onSelect option) )


moveFocusedIndex : UpdateConfig option msg -> SelectId -> State -> Data option -> Int -> ( State, Cmd (Msg option), Maybe msg )
moveFocusedIndex { toId } selectId state { options } index =
    case options |> Array.get index |> Maybe.map toId of
        Nothing ->
            ( { state | focusedIndex = Just index }, Cmd.none, Nothing )

        Just optionId ->
            ( { state | focusedIndex = Just index }, scrollToOption selectId optionId NoOp, Nothing )



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
    , toMsg : Msg option -> msg
    }


textOption : String -> HtmlDetails msg
textOption str =
    HtmlDetails [] [ Html.text str ]


viewConfig :
    { toId : option -> String
    , placeholder : String
    , toLabel : option -> String
    , toMsg : Msg option -> msg
    }
    -> ViewConfig option msg
viewConfig config =
    { toId = config.toId
    , placeholder = config.placeholder
    , optionDetails = textOption << config.toLabel
    , toLabel = config.toLabel
    , toMsg = config.toMsg
    }


toggleOpen : State -> Msg option
toggleOpen state =
    case state.status of
        Open _ ->
            Close

        Closed ->
            OpenSelect (SelectId "dropdown")


view : ViewConfig option msg -> SelectId -> Data option -> State -> Html msg
view ({ toMsg, toId } as config) (SelectId elementId) data state =
    div
        [ id elementId
        , class "dropdown"
        , on "focusout" (onFocusOut toMsg elementId)
        , preventDefaultOn "keydown"
            (Decode.map
                (\key ->
                    ( KeyPress ( SelectId elementId, key ) |> toMsg, shouldPreventDefault state.status key )
                )
                keyDecoder
            )
        ]
        -- [ input [ class "dropdown-button", onClick (toMsg <| toggleOpen state), value query, onInput config.onInput ]
        --     []
        [ button
            [ class "dropdown-button", onClick (toMsg <| toggleOpen state) ]
            [ text (getButtonText config data) ]
        , case state.status of
            Open visibility ->
                viewOptions config visibility state data

            Closed ->
                text ""
        ]


viewOptions : ViewConfig option msg -> DropdownVisibility -> State -> Data option -> Html msg
viewOptions config visibility state data =
    div
        ([ class "dropdown-options-container"
         , id (getOptionsContainerId (SelectId "dropdown"))
         ]
            ++ getVisibilityStyle visibility
        )
        [ ul [ class "dropdown-options", tabindex -1, attribute "role" "listbox" ]
            (data.options
                |> Array.map
                    (viewOption config
                        data.selectedId
                        "dropdown"
                        (getOptionIdByIndex config.toId data.options state.focusedIndex)
                    )
                |> Array.toList
            )
        ]


getOptionIdByIndex toId options index =
    index
        |> Maybe.map (\value -> Array.get value options)
        |> Maybe.withDefault Nothing
        |> Maybe.map toId


equalId id optionalId =
    case optionalId of
        Nothing ->
            False

        Just value ->
            id == value


viewOption : ViewConfig option msg -> String -> String -> Maybe String -> option -> Html msg
viewOption ({ toId, optionDetails, toMsg } as config) selectedId dropdownId focusedId option =
    li
        ([ attribute "role" "option"
         , id (getOptionElementId (SelectId dropdownId) (toId option))
         , onClick (SelectOption option |> toMsg)
         , class "dropdown-option"
         , classList
            [ ( "dropdown-option--selected", toId option == selectedId )
            , ( "dropdown-option--focused", equalId (toId option) focusedId )
            ]
         ]
            ++ (option |> optionDetails |> .attributes)
            ++ (if equalId (toId option) focusedId then
                    [ attribute "aria-selected" "true" ]

                else
                    []
               )
        )
        (option
            |> optionDetails
            |> .children
        )


getVisibilityStyle : DropdownVisibility -> List (Html.Attribute msg)
getVisibilityStyle visibility =
    case visibility of
        Visible ->
            [ style "opacity" "1", style "scroll-behavior" "smooth" ]

        Hidden ->
            [ style "opacity" "0" ]


getButtonText : ViewConfig option msg -> Data option -> String
getButtonText config { options, selectedId } =
    options
        |> Array.filter (\option -> config.toId option == selectedId)
        |> Array.get 0
        |> Maybe.map config.toLabel
        |> Maybe.withDefault config.placeholder


getOptionElementId (SelectId selectId) optionId =
    selectId ++ "-" ++ optionId


getOptionsContainerId (SelectId selectId) =
    selectId ++ "-options-container"



-- EVENTS


onFocusOut : (Msg option -> msg) -> String -> Decode.Decoder msg
onFocusOut toMsg id =
    outsideTarget "relatedTarget" id
        |> Decode.map toMsg


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
                        Decode.succeed False

                    else
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")
        , Decode.succeed True
        ]



-- scroll to selected


scrollToOption : SelectId -> String -> Msg option -> Cmd (Msg option)
scrollToOption selectId optionId msg =
    scrollToElement
        (getOptionElementId selectId optionId |> toChildId)
        (getOptionsContainerId selectId |> toContainerId)
        |> Task.attempt (\_ -> msg)



-- KEYBOARD NAVIGATION


keyDecoder : Decode.Decoder KeyPressed
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toKeyPressed


shouldPreventDefault status key =
    case status of
        Open _ ->
            key == EnterOrSpace

        Closed ->
            False


moveIndexDown total focusedIndex =
    case focusedIndex of
        Nothing ->
            0

        Just index ->
            if index >= total - 1 then
                0

            else
                index + 1


moveIndexUp total focusedIndex =
    case focusedIndex of
        Nothing ->
            total - 1

        Just index ->
            if index <= 0 then
                total - 1

            else
                index - 1
