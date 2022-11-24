module AccessibleDropdown exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, button, div, h2, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, tabindex)
import Html.Events as Events
import Json.Decode as Decode
import Task


allOptions : List Option
allOptions =
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



-- MODEL


type alias Option =
    { id : String
    , label : String
    }


type State
    = Open
    | Closed


type alias Model =
    { state : State
    , selectedId : Maybe String
    , focusedId : Maybe String
    , options : List Option
    }


initialModel : Model
initialModel =
    { state = Closed
    , selectedId = Nothing
    , focusedId = Nothing
    , options = allOptions
    }



-- UPDATE


type Msg
    = Toggle
    | Close
    | SelectOption String
    | KeyPress KeyPressed
    | SetFocusOn String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle ->
            if model.state |> isOpen then
                closeDropdown model

            else
                openDropdown model

        Close ->
            closeDropdown model

        SelectOption id ->
            ( { model | selectedId = Just id }, Cmd.none )

        KeyPress key ->
            if model.state |> isOpen then
                handleKeyWhenOpen model key

            else
                handleKeyWhenClosed model key

        SetFocusOn _ ->
            ( model, Cmd.none )


handleKeyWhenClosed : Model -> KeyPressed -> ( Model, Cmd Msg )
handleKeyWhenClosed model key =
    if key == Up || key == Down then
        openDropdown model

    else
        ( model, Cmd.none )


handleKeyWhenOpen : Model -> KeyPressed -> ( Model, Cmd Msg )
handleKeyWhenOpen model key =
    case key of
        Enter ->
            ( { model | selectedId = model.focusedId }, Cmd.none )

        Space ->
            ( { model | selectedId = model.focusedId }, Cmd.none )

        Up ->
            navigateWithKey model (getPrevId model)

        Down ->
            navigateWithKey model (getNextId model)

        Escape ->
            closeDropdown model

        Other ->
            ( model, Cmd.none )


navigateWithKey : Model -> Maybe String -> ( Model, Cmd Msg )
navigateWithKey model nextId =
    ( { model | focusedId = nextId }
    , nextId |> Maybe.map focusOption |> Maybe.withDefault Cmd.none
    )


firstId : List Option -> Maybe String
firstId =
    List.head >> Maybe.map .id


lastId : List Option -> Maybe String
lastId =
    List.reverse >> firstId


getPrevId : Model -> Maybe String
getPrevId model =
    case model.focusedId of
        Nothing ->
            lastId model.options

        Just id ->
            model.options |> List.map .id |> findPrev id


getNextId : Model -> Maybe String
getNextId model =
    case model.focusedId of
        Nothing ->
            firstId model.options

        Just id ->
            model.options |> List.map .id |> findNext id


openDropdown : Model -> ( Model, Cmd Msg )
openDropdown model =
    let
        focusedId =
            defaultFocused model
    in
    ( { model | state = Open, focusedId = focusedId }
    , focusedId |> Maybe.map focusOption |> Maybe.withDefault Cmd.none
    )


defaultFocused : Model -> Maybe String
defaultFocused model =
    case model.selectedId of
        Nothing ->
            firstId model.options

        Just id ->
            Just id


closeDropdown : Model -> ( Model, Cmd Msg )
closeDropdown model =
    ( { model | state = Closed, focusedId = Nothing }, Cmd.none )



-- VIEW


dropdownElementId : String
dropdownElementId =
    "dropdown"


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Accessible dropdown" ]
        , div [ class "main" ]
            [ viewDropdown model ]
        ]


viewDropdown : Model -> Html Msg
viewDropdown model =
    div
        [ id dropdownElementId
        , class "elm-dropdown"
        , Events.preventDefaultOn "keydown" keyDecoder
        , Events.on "focusout" (onFocusOut "dropdown")
        ]
        [ button
            [ class "elm-dropdown-button"
            , attribute "aria-haspopup" "listbox"
            , Events.onClick Toggle
            ]
            [ text (getButtonText model "Select...")
            ]
        , if model.state |> isOpen then
            viewList model

          else
            text ""
        ]


viewList : Model -> Html Msg
viewList model =
    div
        [ class "elm-dropdown-container", attribute "role" "listbox" ]
        [ ul
            [ class "elm-dropdown-list" ]
            (List.map (viewOption model) model.options)
        ]


viewOption : Model -> Option -> Html Msg
viewOption model option =
    let
        isSelected =
            maybeEqual model.selectedId option.id
    in
    li
        ([ attribute "role" "option"
         , id option.id
         , tabindex -1
         , Events.onClick (SelectOption option.id)
         , class "elm-dropdown-option"
         , classList
            [ ( "elm-dropdown-option--selected", isSelected )
            , ( "elm-dropdown-option--focused", maybeEqual model.focusedId option.id )
            ]
         ]
            ++ (if isSelected then
                    [ attribute "aria-selected" "true" ]

                else
                    []
               )
        )
        [ text option.label ]


maybeEqual : Maybe String -> String -> Bool
maybeEqual maybeId idToCompare =
    maybeId |> Maybe.map (\id -> id == idToCompare) |> Maybe.withDefault False


getButtonText : Model -> String -> String
getButtonText model placeholder =
    case model.selectedId of
        Nothing ->
            placeholder

        Just id ->
            model.options
                |> byId id
                |> Maybe.map .label
                |> Maybe.withDefault placeholder


byId : String -> List Option -> Maybe Option
byId id =
    List.filter (\option -> option.id == id) >> List.head



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- EVENT DECODERS


onFocusOut : String -> Decode.Decoder Msg
onFocusOut id =
    outsideTarget "relatedTarget" id


outsideTarget : String -> String -> Decode.Decoder Msg
outsideTarget targetName dropdownId =
    Decode.field targetName (isOutsideDropdown dropdownId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed Close

                else
                    Decode.fail "inside dropdown"
            )


isOutsideDropdown : String -> Decode.Decoder Bool
isOutsideDropdown dropdownId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if dropdownId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]


focusOption : String -> Cmd Msg
focusOption optionId =
    Task.attempt (\_ -> SetFocusOn optionId) (Dom.focus optionId)


keyDecoder : Decode.Decoder ( Msg, Bool )
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map toKeyPressed
        |> Decode.map
            (\key ->
                ( KeyPress key, preventDefault key )
            )


preventDefault : KeyPressed -> Bool
preventDefault key =
    key == Up || key == Down


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



-- HELPERS


isOpen : State -> Bool
isOpen state =
    state == Open


findPrev : String -> List String -> Maybe String
findPrev selectedId ids =
    List.foldr (getAdjacent selectedId) Nothing ids


findNext : String -> List String -> Maybe String
findNext selectedId ids =
    List.foldl (getAdjacent selectedId) Nothing ids


getAdjacent : String -> String -> Maybe String -> Maybe String
getAdjacent selectedId currentId resultId =
    case resultId of
        Nothing ->
            if currentId == selectedId then
                Just selectedId

            else
                Nothing

        Just id ->
            if id == selectedId then
                Just currentId

            else
                Just id
