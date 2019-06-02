module Select.KeyboardNavigator exposing
    ( NavigatorConfig
    , NavigatorData
    , keyNavigator
    )

import Json.Decode as Decode
import Select.Types exposing (NavigationIndex(..))


type alias NavigatorConfig msg =
    { onFocus : NavigationIndex -> msg
    , onOpen : msg
    , onClose : msg
    , onPress : NavigationIndex -> msg
    , onNoOp : msg
    }


type alias NavigatorData =
    { open : Bool
    , total : Int
    , focusedIndex : NavigationIndex
    }


type KeyPressed
    = Up
    | Down
    | Escape
    | EnterOrSpace
    | Other


keyNavigator : NavigatorConfig msg -> NavigatorData -> Decode.Decoder ( msg, Bool )
keyNavigator config data =
    Decode.field "key" Decode.string
        |> Decode.map toKeyPressed
        |> Decode.map
            (\key ->
                ( handleKey config data key
                , shouldPreventDefault data.open key
                )
            )


handleKey config data key =
    case data.open of
        False ->
            handleKeyWhenClosed config key

        True ->
            handleKeyWhenOpen config data key


handleKeyWhenClosed config key =
    case key of
        Up ->
            config.onOpen

        Down ->
            config.onOpen

        _ ->
            config.onNoOp


handleKeyWhenOpen config data key =
    case key of
        EnterOrSpace ->
            config.onPress data.focusedIndex

        Up ->
            moveIndexUp data.total data.focusedIndex |> config.onFocus

        Down ->
            moveIndexDown data.total data.focusedIndex |> config.onFocus

        Escape ->
            config.onClose

        Other ->
            config.onNoOp


moveIndexDown total (NavigationIndex index) =
    if index >= total - 1 then
        NavigationIndex 0

    else
        NavigationIndex (index + 1)


moveIndexUp total (NavigationIndex index) =
    if index <= 0 then
        NavigationIndex (total - 1)

    else
        NavigationIndex (index - 1)


shouldPreventDefault open key =
    key == EnterOrSpace && open


toKeyPressed : String -> KeyPressed
toKeyPressed key =
    case key of
        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        "Escape" ->
            Escape

        "Enter" ->
            EnterOrSpace

        " " ->
            EnterOrSpace

        _ ->
            Other
