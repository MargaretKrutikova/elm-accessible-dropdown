module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (..)
import Browser
import Html exposing (Html, button, div, input, li, pre, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, src, style, tabindex, value)
import Select


options : Array Option
options =
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


type alias Option =
    { id : String
    , label : String
    }



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { select : Select.State
    , selectedIds : List String
    , query : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { select = Select.initialState { open = False, id = "dropdown" }
      , selectedIds = []
      , query = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotSelectMsg Select.Msg
    | SetQuery String


type SelectMsg
    = SelectOption String


selectUpdateConfig : Select.UpdateConfig Option SelectMsg
selectUpdateConfig =
    Select.updateConfig
        { toId = .id
        , onSelect = Just (\id -> Just (SelectOption id))
        }


selectViewConfig : Select.ViewConfig Option Msg
selectViewConfig =
    Select.viewConfig
        { toId = .id
        , toLabel = .label
        , placeholder = "Select ..."
        , classNamespace = Just "my-cool-dropdown"

        -- , optionDetails =
        --     \option ->
        --         { attributes = []
        --         , children = [ text (option.label ++ " \u{1F92F}") ]
        --         }
        , toMsg = GotSelectMsg
        }


isSelected : Model -> Option -> Bool
isSelected model option =
    List.any (\id -> id == option.id) model.selectedIds


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSelectMsg selectMsg ->
            let
                ( select, outCmd, outMsg ) =
                    Select.update selectUpdateConfig selectMsg model.select options (isSelected model)
            in
            case outMsg of
                Nothing ->
                    ( { model | select = select }, Cmd.map GotSelectMsg outCmd )

                Just (SelectOption id) ->
                    let
                        newSelected =
                            case List.any ((==) id) model.selectedIds of
                                False ->
                                    id :: model.selectedIds

                                True ->
                                    List.filter ((/=) id) model.selectedIds
                    in
                    ( { model | select = select, selectedIds = newSelected }, Cmd.map GotSelectMsg outCmd )

        -- SelectValue ( option, state ) ->
        --     ( { model | selectedId = Just option.id, select = state }, Cmd.none )
        SetQuery query ->
            ( { model | query = query }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "position" "relative" ]
        [ div
            [ style "position" "absolute"
            , style "top" "100px"
            , style "left" "200px"
            ]
            [ div []
                [ Select.view selectViewConfig model.select options (isSelected model)
                ]
            ]
        ]
