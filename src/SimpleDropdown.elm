module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, button, div, h2, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, style, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import List exposing (..)
import Task


options : List Option
options =
    [ { id = "Np", label = "Neptunium" }
    , { id = "Pu", label = "Plutonium" }
    , { id = "Am", label = "Americium" }
    , { id = "Cm", label = "Curium" }
    , { id = "Bk", label = "Berkelium" }
    , { id = "Cf", label = "Californium" }
    , { id = "Fm", label = "Fermium" }
    , { id = "Md", label = "Mendelevium" }
    , { id = "No", label = "Nobelium" }
    , { id = "Lr", label = "Lawrencium" }
    , { id = "Rf", label = "Rutherfordium" }
    , { id = "Db", label = "Dubnium" }
    , { id = "Sg", label = "Seaborgium" }
    , { id = "Bh", label = "Bohrium" }
    , { id = "Hs", label = "Hassium" }
    ]



-- MODEL


type alias Option =
    { id : String
    , label : String
    }


type Visibility
    = Visible
    | Hidden


type State
    = Open Visibility
    | Closed


type alias Model =
    { state : State
    , dropdownId : String
    , selectedId : String
    }


initialModel : Model
initialModel =
    { state = Closed
    , dropdownId = "elm-dropdown"
    , selectedId = ""
    }



-- UPDATE


type Msg
    = Toggle
    | Close
    | SelectOption String
    | ScrolledIntoSelected


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            case model.state of
                Open _ ->
                    ( { model | state = Closed }, Cmd.none )

                Closed ->
                    openDropdown model

        Close ->
            ( { model | state = Closed }, Cmd.none )

        ScrolledIntoSelected ->
            ( setVisibleIfOpen model, Cmd.none )

        SelectOption id ->
            ( { model | selectedId = id }, Cmd.none )


openDropdown : Model -> ( Model, Cmd Msg )
openDropdown model =
    ( { model | state = Open Hidden }, scrollToSelectedOption model )


setVisibleIfOpen : Model -> Model
setVisibleIfOpen model =
    case model.state of
        Open _ ->
            { model | state = Open Visible }

        Closed ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ h2 [] [ text "Simple dropdown" ]
        , viewDropdown model
        ]


viewDropdown : Model -> Html Msg
viewDropdown model =
    div
        [ id model.dropdownId, class "elm-dropdown" ]
        [ button
            [ class "elm-dropdown-button", onClick Toggle ]
            [ text
                (model
                    |> getSelectedOption
                    |> Maybe.withDefault getDefaultOption
                    |> .label
                )
            ]
        , case model.state of
            Open visibility ->
                div
                    ([ class "elm-dropdown-options-container", id (getOptionsContainerId model) ]
                        ++ getStyle visibility
                    )
                    [ ul [ class "elm-dropdown-options" ]
                        (getDefaultOption :: options |> List.map (viewOption model))
                    ]

            Closed ->
                text ""
        ]


viewOption : Model -> Option -> Html Msg
viewOption model option =
    li
        [ attribute "role" "option"
        , id (getOptionElementId model option.id)
        , onClick (SelectOption option.id)
        , class "elm-dropdown-option"
        , classList
            [ ( "elm-dropdown-option--selected", model.selectedId == option.id ) ]
        ]
        [ text option.label ]


getStyle : Visibility -> List (Html.Attribute Msg)
getStyle visibility =
    case visibility of
        Visible ->
            [ style "opacity" "1" ]

        Hidden ->
            [ style "opacity" "0" ]


getSelectedOption : Model -> Maybe Option
getSelectedOption model =
    options
        |> List.filter (\option -> option.id == model.selectedId)
        |> List.head


getOptionElementId model optionId =
    model.dropdownId ++ "-" ++ optionId


getOptionsContainerId model =
    model.dropdownId ++ "-options-container"


getDefaultOption =
    { id = "", label = "Choose element" }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                case model.state of
                    Open _ ->
                        Browser.Events.onMouseDown (outsideTarget "elm-dropdown")

                    Closed ->
                        Sub.none
        }


outsideTarget : String -> Decode.Decoder Msg
outsideTarget dropdownId =
    Decode.field "target" (isOutsideDropdown "elm-dropdown")
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


scrollToSelectedOption : Model -> Cmd Msg
scrollToSelectedOption model =
    Task.map2
        getOptionScrollPosition
        (Dom.getElement (getOptionElementId model model.selectedId))
        (Dom.getElement (getOptionsContainerId model))
        |> Task.andThen
            (\top ->
                Dom.setViewportOf (getOptionsContainerId model) 0 top
            )
        |> Task.attempt
            (\_ -> ScrolledIntoSelected)


getOptionScrollPosition : Dom.Element -> Dom.Element -> Float
getOptionScrollPosition { element } containerElement =
    element.y - containerElement.element.y - element.height
