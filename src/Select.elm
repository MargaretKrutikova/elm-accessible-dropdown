module Select exposing
    ( Msg
    , State
    , UpdateConfig
    , ViewConfig
    , customUpdateConfig
    , customViewConfig
    , customizations
    , customizationsAttrs
    , customizationsViews
    , initialState
    , update
    , updateConfig
    , view
    , viewConfig
    )

import Array exposing (Array)
import Html
import Select.Internal as Internal
import Select.Keyboard as Keyboard


{-| Keeps track of the index of the keyboard focused option,
open/closed state and id attribute of the select element
-}
type State
    = State Internal.State


initialState : { open : Bool, id : String } -> State
initialState state =
    State (Internal.initialState state)



{-
   Update configs
-}


type UpdateConfig option msg
    = UpdateConfig (Internal.UpdateConfig option msg)


updateConfig :
    { toId : option -> String
    , onSelect : Maybe (String -> Maybe msg)
    }
    -> UpdateConfig option msg
updateConfig { toId, onSelect } =
    UpdateConfig
        (Internal.updateConfig
            { toId = toId
            , navigation = Keyboard.StayOnBoundary
            , onMouseClick = Maybe.map (\select -> \id -> select id) onSelect
            , onKeyPress = Maybe.map Internal.defaultKeyPress onSelect
            }
        )


customUpdateConfig :
    { toId : option -> String
    , navigation : Keyboard.NavigationStrategy
    , onMouseClick : Maybe (String -> Maybe msg)
    , onKeyPress : Maybe (Keyboard.KeyPressed -> Maybe String -> Maybe msg)
    }
    -> UpdateConfig option msg
customUpdateConfig config =
    UpdateConfig (Internal.updateConfig config)



{-
   Update function
-}


type Msg
    = Msg Internal.Msg


update : UpdateConfig option msg -> Msg -> State -> Array option -> (option -> Bool) -> ( State, Cmd Msg, Maybe msg )
update (UpdateConfig config) (Msg msg) (State state) options isSelected =
    let
        ( newState, cmd, maybeMsg ) =
            Internal.update config msg state options isSelected
    in
    ( State newState, Cmd.map Msg cmd, maybeMsg )



{-
   View
-}


type ViewConfig option msg
    = ViewConfig (Internal.ViewConfig option msg)


type Customizations option msg
    = Customizations (Internal.Customizations option msg)


type alias HtmlDetails msg =
    Internal.HtmlDetails msg


type alias OptionStatus =
    Internal.OptionStatus


viewConfig :
    { toId : option -> String
    , placeholder : String
    , toLabel : option -> String
    , toMsg : Msg -> msg
    , classNamespace : Maybe String
    }
    -> ViewConfig option msg
viewConfig { toId, placeholder, classNamespace, toLabel, toMsg } =
    ViewConfig
        (Internal.viewConfig
            { toId = toId
            , placeholder = placeholder
            , toLabel = toLabel
            , toMsg = Msg >> toMsg
            , classNamespace = classNamespace
            }
        )


customizations :
    { selectAttributes : List (Html.Attribute msg)
    , containerAttributes : List (Html.Attribute msg)
    , listAttributes : List (Html.Attribute msg)
    , viewButton : List option -> HtmlDetails msg
    , viewOption : option -> OptionStatus -> HtmlDetails msg
    }
    -> Customizations option msg
customizations config =
    Customizations
        { selectAttributes = config.selectAttributes
        , containerAttributes = config.containerAttributes
        , listAttributes = config.listAttributes
        , viewButton = config.viewButton
        , viewOption = config.viewOption
        }


customizationsViews :
    { viewButton : List option -> HtmlDetails msg
    , viewOption : option -> OptionStatus -> HtmlDetails msg
    }
    -> Customizations option msg
customizationsViews config =
    Customizations
        { selectAttributes = []
        , containerAttributes = []
        , listAttributes = []
        , viewButton = config.viewButton
        , viewOption = config.viewOption
        }


customizationsAttrs :
    { placeholder : String
    , toLabel : option -> String
    , selectAttributes : List (Html.Attribute msg)
    , containerAttributes : List (Html.Attribute msg)
    , listAttributes : List (Html.Attribute msg)
    }
    -> Customizations option msg
customizationsAttrs config =
    let
        defaults =
            Internal.defaultCustomizations { placeholder = config.placeholder, toLabel = config.toLabel }
    in
    Customizations
        { defaults
            | selectAttributes = config.selectAttributes
            , containerAttributes = config.containerAttributes
            , listAttributes = config.listAttributes
        }


customViewConfig :
    { toId : option -> String
    , toMsg : Msg -> msg
    , classNamespace : Maybe String
    }
    -> Customizations option msg
    -> ViewConfig option msg
customViewConfig { toId, toMsg, classNamespace } (Customizations cs) =
    ViewConfig
        (Internal.customViewConfig
            { toId = toId
            , toMsg = Msg >> toMsg
            , classNamespace = classNamespace
            , customizations = cs
            }
        )


view : ViewConfig option msg -> State -> Array option -> (option -> Bool) -> Html.Html msg
view (ViewConfig config) (State state) options isSelected =
    Internal.view config state options isSelected
