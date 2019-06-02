module Select.Select exposing
    ( Msg
    , State
    , UpdateConfig
    , ViewConfig
    , customInitialState
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
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode as Decode
import List exposing (..)
import Select.KeyboardNavigator exposing (..)
import Select.ScrollUtils exposing (..)
import Select.Types exposing (NavigationIndex(..), getByIndex, isEqual)
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
    , focusedIndex : NavigationIndex
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
    , focusedIndex = NavigationIndex -1
    }


initialStatus : Bool -> Status
initialStatus open =
    if open then
        Open Visible

    else
        Closed



-- UPDATE


type Msg option
    = Toggle
    | Close
    | OpenSelect
    | ScrolledIntoSelected
    | Focus NavigationIndex
    | NoOp


type UpdateConfig msg option
    = UpdateConfig
        { options : Array option
        , selectedId : String
        , toId : option -> String
        }


updateConfig :
    { options : Array option
    , selectedId : String
    , toId : option -> String
    }
    -> UpdateConfig msg option
updateConfig { options, selectedId, toId } =
    UpdateConfig
        { options = options
        , selectedId = selectedId
        , toId = toId
        }


update : UpdateConfig msg option -> Msg option -> State -> ( State, Cmd (Msg option) )
update ((UpdateConfig { selectedId, options }) as config) msg state =
    case msg of
        Toggle ->
            case state.status of
                Open _ ->
                    ( { state | status = Closed }, Cmd.none )

                Closed ->
                    openDropdown config state

        Close ->
            ( { state | status = Closed }, Cmd.none )

        OpenSelect ->
            openDropdown config state

        ScrolledIntoSelected ->
            case state.status of
                Open _ ->
                    ( { state | status = Open Visible }, Cmd.none )

                Closed ->
                    ( state, Cmd.none )

        Focus index ->
            ( { state | focusedIndex = index }
            , scrollToOptionByIndex config state index
            )

        NoOp ->
            ( state, Cmd.none )


openDropdown : UpdateConfig msg option -> State -> ( State, Cmd (Msg option) )
openDropdown ((UpdateConfig { selectedId }) as config) state =
    ( { state
        | status = Open Hidden
        , focusedIndex = getSelectedOptionIndex config |> NavigationIndex
      }
    , scrollToOption selectedId state
    )


getSelectedOptionIndex : UpdateConfig msg option -> Int
getSelectedOptionIndex (UpdateConfig { options, selectedId, toId }) =
    options
        |> Array.toIndexedList
        |> List.filter (\( index, option ) -> selectedId == toId option)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault -1



-- VIEW


type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


type alias ViewConfig option msg =
    { toId : option -> String
    , selectedId : String
    , options : Array option
    , placeholder : String
    , toLabel : option -> String
    , optionDetails : option -> HtmlDetails msg
    , toMsg : Msg option -> msg
    , closeOnSelect : Bool
    , onSelect : ( option, State ) -> msg
    }


textOption : String -> HtmlDetails msg
textOption str =
    HtmlDetails [] [ Html.text str ]


viewConfig :
    { toId : option -> String
    , selectedId : String
    , options : Array option
    , placeholder : String
    , toLabel : option -> String
    , toMsg : Msg option -> msg
    , onSelect : ( option, State ) -> msg
    , closeOnSelect : Bool
    }
    -> ViewConfig option msg
viewConfig config =
    { toId = config.toId
    , selectedId = config.selectedId
    , options = config.options
    , placeholder = config.placeholder
    , optionDetails = textOption << config.toLabel
    , toLabel = config.toLabel
    , toMsg = config.toMsg
    , onSelect = config.onSelect
    , closeOnSelect = config.closeOnSelect
    }


navigatorConfig : ViewConfig option msg -> State -> NavigatorConfig msg
navigatorConfig ({ toMsg, options } as config) state =
    { onFocus = \index -> Focus index |> toMsg
    , onOpen = OpenSelect |> toMsg
    , onClose = Close |> toMsg
    , onPress =
        \(NavigationIndex index) ->
            case options |> Array.get index of
                Just option ->
                    selectOption config state option

                Nothing ->
                    NoOp |> toMsg
    , onNoOp = NoOp |> toMsg
    }


navigatorData : ViewConfig option msg -> State -> NavigatorData
navigatorData { options } state =
    { open = state.status /= Closed
    , focusedIndex = state.focusedIndex
    , total = Array.length options
    }


view : ViewConfig option msg -> State -> Html msg
view ({ toMsg, onSelect, closeOnSelect, options } as config) state =
    div
        [ id state.dropdownId
        , class "dropdown"
        , on "focusout" (onFocusOut toMsg state)
        , keyNavigator
            (navigatorConfig config state)
            (navigatorData config state)
            |> preventDefaultOn "keydown"
        ]
        [ button
            [ class "dropdown-button", onClick (toMsg Toggle) ]
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


selectOption : ViewConfig option msg -> State -> option -> msg
selectOption { onSelect, closeOnSelect } state option =
    case closeOnSelect of
        True ->
            onSelect ( option, { state | status = Closed } )

        False ->
            onSelect ( option, state )


viewOption : ViewConfig option msg -> State -> ( Int, option ) -> Html msg
viewOption ({ selectedId, toId, optionDetails, toMsg } as config) state ( index, option ) =
    li
        ([ attribute "role" "option"
         , id (toId option |> getOptionElementId state)
         , onClick (option |> selectOption config state)
         , class "dropdown-option"
         , classList
            [ ( "dropdown-option--selected", toId option == selectedId )
            , ( "dropdown-option--focused", isEqual state.focusedIndex index )
            ]
         ]
            ++ (option |> optionDetails |> .attributes)
            ++ (if isEqual state.focusedIndex index then
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


getSelectedOption : ViewConfig option msg -> Maybe option
getSelectedOption { options, selectedId, toId } =
    options
        |> Array.filter (\option -> toId option == selectedId)
        |> Array.get 0


getButtonText : ViewConfig option msg -> String
getButtonText config =
    getSelectedOption config
        |> Maybe.map config.toLabel
        |> Maybe.withDefault config.placeholder


getOptionElementId state optionId =
    state.dropdownId ++ "-" ++ optionId


getOptionsContainerId state =
    state.dropdownId ++ "-options-container"



-- EVENTS


onFocusOut : (Msg option -> msg) -> State -> Decode.Decoder msg
onFocusOut toMsg state =
    outsideTarget "relatedTarget" state.dropdownId
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


scrollToOption : String -> State -> Cmd (Msg option)
scrollToOption optionId state =
    scrollToElement
        (getOptionElementId state optionId |> toChildId)
        (getOptionsContainerId state |> toContainerId)
        |> Task.attempt (\_ -> ScrolledIntoSelected)


scrollToOptionByIndex : UpdateConfig msg option -> State -> NavigationIndex -> Cmd (Msg option)
scrollToOptionByIndex (UpdateConfig { options, toId }) state index =
    case options |> getByIndex index |> Maybe.map toId of
        Nothing ->
            Cmd.none

        Just optionId ->
            scrollToNavigationIndex
                (getOptionElementId state optionId |> toChildId)
                (getOptionsContainerId state |> toContainerId)
                (Array.length options)
                index
                |> Task.attempt (\_ -> NoOp)
