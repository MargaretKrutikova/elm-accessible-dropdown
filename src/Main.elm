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
    , query : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { select = Select.initialState { open = False }
      , selectedId = "Np"
      , query = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotSelectMsg (Select.Msg Option)
    | SelectValue ( Option, Select.State )
    | SetQuery String


type SelectMsg
    = SelectOption Option


selectUpdateConfig : Model -> Select.UpdateConfig Option SelectMsg
selectUpdateConfig model =
    { toId = .id
    , closeOnSelect = True
    , onSelect = SelectOption
    }


selectViewConfig : Select.ViewConfig Option Msg
selectViewConfig =
    { toId = .id
    , toLabel = .label

    -- , options =
    --     Array.filter
    --         (\option ->
    --             String.isEmpty model.query
    --                 || String.contains (String.toLower model.query) (String.toLower option.label)
    --         )
    --         options
    , placeholder = "Select ..."
    , optionDetails =
        \option ->
            { attributes = []
            , children = [ text (option.label ++ " \u{1F92F}") ]
            }
    , toMsg = GotSelectMsg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSelectMsg selectMsg ->
            let
                ( select, outCmd, outMsg ) =
                    Select.update (selectUpdateConfig model) selectMsg model.select { selectedId = model.selectedId, options = options }
            in
            case outMsg of
                Nothing ->
                    ( { model | select = select }, Cmd.map GotSelectMsg outCmd )

                Just (SelectOption option) ->
                    ( { model | select = select, selectedId = option.id }, Cmd.map GotSelectMsg outCmd )

        SelectValue ( option, state ) ->
            ( { model | selectedId = option.id, select = state }, Cmd.none )

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
                [ Select.view selectViewConfig
                    (Select.SelectId "dropdown")
                    { options = options, selectedId = model.selectedId }
                    model.select
                ]
            ]
        ]
