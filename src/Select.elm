module Select exposing
    ( Msg
    , State
    , UpdateConfig
    , ViewConfig
    , customUpdateConfig
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


viewConfig :
    { toId : option -> String
    , placeholder : Maybe String
    , toLabel : option -> String
    , toMsg : Msg -> msg
    }
    -> ViewConfig option msg
viewConfig { toId, placeholder, toLabel, toMsg } =
    ViewConfig
        (Internal.viewConfig
            { toId = toId
            , placeholder = placeholder
            , toLabel = toLabel
            , toMsg = \msg -> toMsg (Msg msg)
            }
        )


view : ViewConfig option msg -> Array option -> (option -> Bool) -> State -> Html.Html msg
view (ViewConfig config) options isSelected (State state) =
    Internal.view config options isSelected state
