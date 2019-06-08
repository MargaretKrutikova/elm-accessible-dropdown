module Main exposing (main)

import Array exposing (..)
import Browser
import Browser.Events
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, src, tabindex, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import List exposing (..)


getOptions : Array Option
getOptions =
    fromList
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
    , focusedIndex : Int
    , options : Array Option
    }


initialModel : Model
initialModel =
    { open = False
    , id = "dropdown"
    , selectedId = Nothing
    , focusedIndex = -1
    , options = getOptions
    }



-- UPDATE


type Msg
    = Toggle
    | Open
    | Close
    | SelectOption String
    | KeyDown String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            ( { model | open = not model.open }, Cmd.none )

        Open ->
            ( openDropdown model, Cmd.none )

        Close ->
            ( { model | open = False }, Cmd.none )

        SelectOption id ->
            ( { model | selectedId = Just id, open = False }, Cmd.none )

        KeyDown key ->
            if not model.open then
                ( handleKeyWhenClosed key model, Cmd.none )

            else
                ( handleKeyWhenOpen key model, Cmd.none )


handleKeyWhenClosed : String -> Model -> Model
handleKeyWhenClosed key model =
    case key of
        "ArrowUp" ->
            openDropdown model

        "ArrowDown" ->
            openDropdown model

        _ ->
            model


handleKeyWhenOpen : String -> Model -> Model
handleKeyWhenOpen key model =
    case key of
        "Enter" ->
            { model
                | selectedId =
                    model.options
                        |> Array.get model.focusedIndex
                        |> Maybe.map (\option -> option.id)
            }

        "ArrowUp" ->
            if model.focusedIndex <= -1 then
                { model | focusedIndex = (model.options |> Array.length) - 1 }

            else
                { model | focusedIndex = model.focusedIndex - 1 }

        "ArrowDown" ->
            if model.focusedIndex >= (model.options |> Array.length) - 1 then
                { model | focusedIndex = -1 }

            else
                { model | focusedIndex = model.focusedIndex + 1 }

        "Escape" ->
            { model | open = False }

        _ ->
            model


getSelectedOptionIndex : Model -> Maybe Int
getSelectedOptionIndex model =
    model.selectedId
        |> Maybe.map
            (\value ->
                model.options
                    |> Array.indexedMap Tuple.pair
                    |> Array.filter (\( index, option ) -> option.id == value)
                    |> Array.get 0
            )
        |> Maybe.withDefault Nothing
        |> Maybe.map (\( index, option ) -> index)


openDropdown : Model -> Model
openDropdown model =
    { model | open = True, focusedIndex = getSelectedOptionIndex model |> Maybe.withDefault -1 }



-- VIEW


type alias DropdownConfig =
    { placeholder : String
    }


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ viewDropdown model { placeholder = "Choose delivery option" }
        , input [] []
        , button [] [ text "just a button" ]
        ]


viewDropdown : Model -> DropdownConfig -> Html Msg
viewDropdown model { placeholder } =
    div
        [ id model.id
        , class "dropdown"
        , on "focusout" (onFocusOut model)
        , on "keydown" (onKeyDown KeyDown) -- TODO: scroll into view
        ]
        [ button
            [ class "dropdown-button"
            , onClick Toggle
            ]
            [ text
                (model
                    |> getSelectedOption model.options
                    |> Maybe.map .label
                    |> Maybe.withDefault placeholder
                )
            ]
        , if model.open then
            ul [ class "dropdown-options", tabindex -1, attribute "role" "listbox" ]
                (getAllOptions placeholder |> List.map (viewOption model))

          else
            text ""
        ]


viewOption : Model -> ( Int, Option ) -> Html Msg
viewOption model ( index, option ) =
    li
        ([ attribute "role" "option"
         , onClick (SelectOption option.id)
         , class "dropdown-option"
         , classList
            [ ( "dropdown-option--selected", isSelected model option )
            , ( "dropdown-option--focused", model.focusedIndex == index )
            ]
         ]
            ++ (if model.focusedIndex == index then
                    [ attribute "aria-selected" "true" ]

                else
                    []
               )
        )
        [ text option.label ]


getAllOptions : String -> List ( Int, Option )
getAllOptions placeholder =
    ( -1, { id = "", label = placeholder } )
        :: (getOptions
                |> Array.indexedMap Tuple.pair
                |> Array.toList
           )


isSelected : Model -> Option -> Bool
isSelected model option =
    model.selectedId |> Maybe.map (\value -> option.id == value) |> Maybe.withDefault False


getSelectedOption : Array Option -> Model -> Maybe Option
getSelectedOption options model =
    model.selectedId
        |> Maybe.map (\value -> options |> Array.filter (\option -> option.id == value) |> Array.get 0)
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
                    Browser.Events.onMouseDown (outsideTarget "target" model.id)

                else
                    Sub.none
        }



-- EVENTS


onFocusOut : Model -> Decode.Decoder Msg
onFocusOut model =
    outsideTarget "relatedTarget" model.id


onKeyDown : (String -> msg) -> Decode.Decoder msg
onKeyDown toMsg =
    Decode.at [ "key" ] Decode.string
        |> Decode.andThen (\key -> Decode.succeed (toMsg key))


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
