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


selectUpdateConfig : Model -> Select.UpdateConfig Msg Option
selectUpdateConfig model =
    Select.updateConfig
        { closeOnSelect = True
        , options = options
        , selectedId = model.selectedId
        , toId = .id
        }


selectViewConfig : Model -> Select.ViewConfig Option
selectViewConfig model =
    { toId = .id
    , selectedId = model.selectedId
    , toLabel = .label
    , options = options
    , placeholder = "Select ..."
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSelectMsg selectMsg ->
            let
                ( select, outCmd, outMsg ) =
                    Select.update (selectUpdateConfig model) selectMsg model.select
            in
            ( handleSelectMsg model select outMsg, Cmd.map GotSelectMsg outCmd )


handleSelectMsg : Model -> Select.State -> Maybe (Select.OutMsg Option) -> Model
handleSelectMsg model select msg =
    case msg of
        Just (Select.SelectedOption option) ->
            { model | select = select, selectedId = option.id }

        Nothing ->
            { model | select = select }



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
                [ Html.map GotSelectMsg (Select.view (selectViewConfig model) model.select)
                ]
            ]
        ]
