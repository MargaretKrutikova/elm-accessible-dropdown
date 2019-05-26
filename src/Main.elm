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
        [ { id = "a1", label = "Home Delivery (DHL)" }
        , { id = "a3", label = "Express Delivery (DHL)" }
        , { id = "b2", label = "Racehorse" }
        , { id = "c3", label = "Dove Delivery" }
        , { id = "c4", label = "UPS Delivery (oh no)" }
        ]



------------- MODEL -----------------------


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



------------------- UPDATE ----------------


type Msg
    = Toggle
    | Open
    | Close
    | SelectOption String
    | MouseDown (Maybe DomNode)
    | FocusOut (Maybe DomNode)
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

        MouseDown target ->
            ( closeIfOutside model target, Cmd.none )

        FocusOut relatedTarget ->
            ( closeIfOutside model relatedTarget, Cmd.none )

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


closeIfOutside : Model -> Maybe DomNode -> Model
closeIfOutside model domNode =
    let
        isInside =
            isInsideDropdown model.id domNode
    in
    if isInside then
        model

    else
        { model | open = False }



----------------- VIEW ---------------


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
        , on "focusout" onFocusOut
        , on "keydown" (onKeyDown KeyDown)
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
                ([ viewOption model ( -1, { id = "", label = placeholder } )
                 ]
                    ++ (getOptions
                            |> Array.indexedMap Tuple.pair
                            |> Array.map (\tuple -> viewOption model tuple)
                            |> Array.toList
                       )
                )

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


isSelected : Model -> Option -> Bool
isSelected model option =
    model.selectedId |> Maybe.map (\value -> option.id == value) |> Maybe.withDefault False


getSelectedOption : Array Option -> Model -> Maybe Option
getSelectedOption options model =
    model.selectedId
        |> Maybe.map (\value -> options |> Array.filter (\option -> option.id == value) |> Array.get 0)
        |> Maybe.withDefault Nothing



------------ MAIN -----------------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onMouseDown (decodeEvent "target" |> Decode.map MouseDown)



--------------  EVENTS ---------------------


onFocusOut : Decode.Decoder Msg
onFocusOut =
    decodeEvent "relatedTarget" |> Decode.map FocusOut


onMouseDown : Decode.Decoder Msg
onMouseDown =
    decodeEvent "target" |> Decode.map MouseDown


onKeyDown : (String -> msg) -> Decode.Decoder msg
onKeyDown toMsg =
    Decode.at [ "key" ] Decode.string
        |> Decode.andThen (\key -> Decode.succeed (toMsg key))


decodeEvent : String -> Decode.Decoder (Maybe DomNode)
decodeEvent propertyName =
    Decode.at [ propertyName ] (Decode.nullable decodeDomNode)


type DomNode
    = RootNode { id : String }
    | ChildNode { id : String, parentNode : DomNode }


decodeDomNode : Decode.Decoder DomNode
decodeDomNode =
    Decode.oneOf
        [ decodeChildNode, decodeRootNode ]


decodeRootNode : Decode.Decoder DomNode
decodeRootNode =
    Decode.map (\x -> RootNode { id = x })
        (Decode.field "id" Decode.string)


decodeChildNode : Decode.Decoder DomNode
decodeChildNode =
    Decode.map2 (\id parentNode -> ChildNode { id = id, parentNode = parentNode })
        (Decode.field "id" Decode.string)
        (Decode.field "parentNode" (Decode.lazy (\_ -> decodeDomNode)))


isInsideDropdown : String -> Maybe DomNode -> Bool
isInsideDropdown dropdownId node =
    case node of
        Nothing ->
            False

        Just (RootNode { id }) ->
            dropdownId == id

        Just (ChildNode { id, parentNode }) ->
            if dropdownId == id then
                True

            else
                isInsideDropdown dropdownId (Just parentNode)
