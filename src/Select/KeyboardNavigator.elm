module Select.KeyboardNavigator exposing
    ( KeyPressed(..)
    , toKeyPressed
    )

import Json.Decode as Decode


type KeyPressed
    = Up
    | Down
    | Escape
    | EnterOrSpace
    | Other


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
