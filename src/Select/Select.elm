module Select.Select exposing
    ( Msg
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
    , idAttribute : String
    }
    -> State
initialState { open, idAttribute } =
    { status = toStatus open
    , focusedIndex = Nothing
    , idAttribute = idAttribute
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
    | SelectOption String
    | ScrolledIntoSelected
    | KeyPress KeyPressed
    | NoOp


type alias UpdateConfig option msg =
    { toId : option -> String
    , closeOnSelect : Bool
    , onSelect : String -> msg
    }



-- include onMouseClick and onKeyPress instead of onSelect


updateConfig :
    { toId : option -> String
    , closeOnSelect : Bool
    , onSelect : String -> msg
    }
    -> UpdateConfig option msg
updateConfig { toId, closeOnSelect, onSelect } =
    { toId = toId
    , closeOnSelect = closeOnSelect
    , onSelect = onSelect
    }


type alias Data option =
    { selectedOptionId : Maybe String
    , options : Array option
    }


update : UpdateConfig option msg -> Msg -> State -> Data option -> ( State, Cmd Msg, Maybe msg )
update config msg state data =
    case msg of
        Close ->
            ( { state | status = Closed }, Cmd.none, Nothing )

        OpenSelect ->
            openDropdown config state data

        SelectOption option ->
            selectOption config state option

        ScrolledIntoSelected ->
            case state.status of
                Open _ ->
                    ( { state | status = Open Visible }, Cmd.none, Nothing )

                Closed ->
                    ( state, Cmd.none, Nothing )

        KeyPress key ->
            case state.status of
                Open _ ->
                    handleKeyWhenOpen config state data key

                Closed ->
                    handleKeyWhenClosed config state data key

        NoOp ->
            ( state, Cmd.none, Nothing )


openDropdown : UpdateConfig option msg -> State -> Data option -> ( State, Cmd Msg, Maybe msg )
openDropdown { toId } state { selectedOptionId, options } =
    case selectedOptionId of
        Nothing ->
            ( { state | status = Open Hidden }, Cmd.none, Nothing )

        Just optionId ->
            ( { state
                | status = Open Hidden
                , focusedIndex = findIndex (\option -> toId option == optionId) options
              }
            , scrollToOption state.idAttribute optionId ScrolledIntoSelected
            , Nothing
            )


selectOption : UpdateConfig option msg -> State -> String -> ( State, Cmd Msg, Maybe msg )
selectOption { closeOnSelect, onSelect } state optionId =
    let
        updatedState =
            if closeOnSelect then
                { state | status = Closed }

            else
                state
    in
    ( updatedState, Cmd.none, Just (onSelect optionId) )


selectFocusedOption config state { options } =
    case
        state.focusedIndex
            |> Maybe.map (\ind -> Array.get ind options)
            |> Maybe.withDefault Nothing
            |> Maybe.map config.toId
    of
        Just option ->
            selectOption config state option

        Nothing ->
            ( state, Cmd.none, Nothing )


handleKeyWhenClosed : UpdateConfig option msg -> State -> Data option -> KeyPressed -> ( State, Cmd Msg, Maybe msg )
handleKeyWhenClosed config state data key =
    -- TODO: include custom config
    if key == Up || key == Down then
        openDropdown config state data

    else
        ( state, Cmd.none, Nothing )


handleKeyWhenOpen : UpdateConfig option msg -> State -> Data option -> KeyPressed -> ( State, Cmd Msg, Maybe msg )
handleKeyWhenOpen config state ({ options } as data) key =
    -- TODO: include in config? keyToSelect
    if key == Enter || key == Space then
        selectFocusedOption config state data

    else if key == Up || key == Down then
        let
            index =
                navigateWithKey state (Array.length options) key
        in
        ( { state | focusedIndex = index }
        , scrollToOptionByIndex config state.idAttribute data index
        , Nothing
        )

    else if key == Escape then
        ( { state | status = Closed }, Cmd.none, Nothing )

    else
        ( state, Cmd.none, Nothing )


scrollToOptionByIndex { toId } selectElementId { options } index =
    options
        |> Array.get (Maybe.withDefault -1 index)
        |> Maybe.map (\option -> scrollToOption selectElementId (toId option) NoOp)
        |> Maybe.withDefault Cmd.none


navigateWithKey : State -> Int -> KeyPressed -> Maybe Int
navigateWithKey { focusedIndex } total key =
    case key of
        Up ->
            focusedIndex
                |> Maybe.map (moveIndexUp total)
                |> Maybe.withDefault (total - 1)
                |> Just

        Down ->
            focusedIndex
                |> Maybe.map (moveIndexDown total)
                |> Maybe.withDefault 0
                |> Just

        _ ->
            focusedIndex


moveIndexDown total index =
    if index >= total - 1 then
        0

    else
        index + 1


moveIndexUp total index =
    if index <= 0 then
        total - 1

    else
        index - 1



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
    , placeholder : String
    , toLabel : option -> String
    , toMsg : Msg -> msg
    }
    -> ViewConfig option msg
viewConfig config =
    { toId = config.toId
    , placeholder = config.placeholder
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


view : ViewConfig option msg -> Data option -> State -> Html msg
view ({ toMsg, toId } as config) data ({ idAttribute } as state) =
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
            [ text (getButtonText config data) ]
        , case state.status of
            Open visibility ->
                div
                    ([ class "dropdown-options-container"
                     , id (getOptionsContainerId idAttribute)
                     ]
                        ++ getVisibilityStyle visibility
                    )
                    [ viewList config state data ]

            Closed ->
                text ""
        ]


viewList : ViewConfig option msg -> State -> Data option -> Html msg
viewList config state data =
    ul [ class "dropdown-options", tabindex -1, attribute "role" "listbox" ]
        (data.options
            |> Array.toIndexedList
            |> List.map
                (\( index, option ) ->
                    viewOption config state (isSelected config data option) (index == Maybe.withDefault -1 state.focusedIndex) option
                )
        )


isSelected config data option =
    case data.selectedOptionId of
        Nothing ->
            False

        Just id ->
            config.toId option == id


viewOption : ViewConfig option msg -> State -> Bool -> Bool -> option -> Html msg
viewOption ({ toId, optionDetails, toMsg } as config) { idAttribute } selected focused option =
    li
        ([ attribute "role" "option"
         , id (getOptionElementId idAttribute (toId option))
         , onClick (toId option |> SelectOption |> toMsg)
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


getButtonText : ViewConfig option msg -> Data option -> String
getButtonText config data =
    data.options
        |> Array.filter (isSelected config data)
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



-- scroll to selected


scrollToOption : String -> String -> Msg -> Cmd Msg
scrollToOption selectElementId optionId msg =
    scrollToElement
        (getOptionElementId selectElementId optionId |> toChildId)
        (getOptionsContainerId selectElementId |> toContainerId)
        |> Task.attempt (\_ -> msg)



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


findIndex : (option -> Bool) -> Array option -> Maybe Int
findIndex predicate array =
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
