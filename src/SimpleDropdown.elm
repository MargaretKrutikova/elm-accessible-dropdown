module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import List exposing (..)


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


type alias Model =
    { open : Bool
    , id : String
    , selectedId : Maybe String
    }


initialModel : Model
initialModel =
    { open = False
    , id = "dropdown"
    , selectedId = Nothing
    }



-- UPDATE


type Msg
    = Toggle
    | Open
    | Close
    | SelectOption String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            ( { model | open = not model.open }, Cmd.none )

        Open ->
            ( { model | open = True }, Cmd.none )

        Close ->
            ( { model | open = False }, Cmd.none )

        SelectOption id ->
            ( { model | selectedId = Just id, open = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ viewDropdown model ]


viewDropdown : Model -> Html Msg
viewDropdown model =
    div
        [ id model.id, class "dropdown" ]
        [ button
            [ class "dropdown-button", onClick Toggle ]
            [ text
                (model
                    |> getSelectedOption
                    |> Maybe.map .label
                    |> Maybe.withDefault "Choose element"
                )
            ]
        , if model.open then
            ul [ class "dropdown-options" ]
                ({ id = "", label = "Choose element" }
                    :: options
                    |> List.map (viewOption model)
                )

          else
            text ""
        ]


viewOption : Model -> Option -> Html Msg
viewOption model option =
    li
        [ attribute "role" "option"
        , onClick (SelectOption option.id)
        , class "dropdown-option"
        , classList
            [ ( "dropdown-option--selected", isSelected model option ) ]
        ]
        [ text option.label ]


isSelected : Model -> Option -> Bool
isSelected model option =
    model.selectedId
        |> Maybe.map (\value -> option.id == value)
        |> Maybe.withDefault False


getSelectedOption : Model -> Maybe Option
getSelectedOption model =
    model.selectedId
        |> Maybe.map
            (\value ->
                options
                    |> List.filter (\option -> option.id == value)
                    |> List.head
            )
        |> Maybe.withDefault Nothing



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                if model.open then
                    Browser.Events.onMouseDown (outsideTarget "dropdown")

                else
                    Sub.none
        }


outsideTarget : String -> Decode.Decoder Msg
outsideTarget dropdownId =
    Decode.field "target" (isOutsideDropdown "dropdown")
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
