module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (..)
import Browser
import Html exposing (Html, button, div, input, li, pre, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, src, style, tabindex, value)
import Select.Select as Select


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
    , selectedId : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { select = Select.initialState "dropdown"
      , selectedId = "Np"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotSelectMsg (Select.Msg Option)
    | SelectValue ( Option, Select.State )


selectUpdateConfig : Model -> Select.UpdateConfig Msg Option
selectUpdateConfig model =
    Select.updateConfig
        { options = options
        , selectedId = model.selectedId
        , toId = .id
        }


selectViewConfig : Model -> Select.ViewConfig Option Msg
selectViewConfig model =
    { toId = .id
    , selectedId = model.selectedId
    , toLabel = .label
    , options = options
    , placeholder = "Select ..."
    , optionDetails =
        \option ->
            { attributes = []
            , children = [ text (option.label ++ " \u{1F92F}") ]
            }
    , toMsg = GotSelectMsg
    , onSelect = SelectValue
    , closeOnSelect = True
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSelectMsg selectMsg ->
            let
                ( select, outCmd ) =
                    Select.update (selectUpdateConfig model) selectMsg model.select
            in
            ( { model | select = select }, Cmd.map GotSelectMsg outCmd )

        SelectValue ( option, state ) ->
            ( { model | selectedId = option.id, select = state }, Cmd.none )



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
                [ Select.view (selectViewConfig model) model.select
                ]
            ]
        ]
