module Select.Keyboard exposing
    ( KeyPressed(..)
    , NavigationStrategy(..)
    , toKeyPressed
    )

import Json.Decode as Decode


type NavigationStrategy
    = Circular
    | StayOnBoundary


type KeyPressed
    = Up
    | Down
    | Escape
    | Enter
    | Space
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
            Enter

        " " ->
            Space

        _ ->
            Other
