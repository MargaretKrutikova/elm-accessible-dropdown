module Select.ScrollUtils exposing
    ( scrollToElement
    , toChildId
    , toContainerId
    )

import Browser.Dom as Dom
import Task


type ViewportPosition
    = Above Int
    | Below Int
    | Inside


type ChildId
    = ChildId String


toChildId =
    ChildId


type ContainerId
    = ContainerId String


toContainerId =
    ContainerId


offsetTop : Dom.Element -> Dom.Element -> Float
offsetTop child container =
    child.element.y - container.element.y


viewportAdjustment : Dom.Element -> Dom.Element -> Maybe Float
viewportAdjustment child container =
    -- above the viewport
    if offsetTop child container < 0 then
        Just (offsetTop child container - child.element.height / 2)

    else
    -- below the viewport
    if
        offsetTop child container + child.element.height > container.element.height
    then
        Just (offsetTop child container + 3 * child.element.height / 2 - container.element.height)

    else
        Nothing


scrollToElement : ChildId -> ContainerId -> Task.Task Dom.Error ()
scrollToElement (ChildId childId) (ContainerId containerId) =
    Task.map3
        (\option -> \container -> \viewport -> ( option, container, viewport ))
        (Dom.getElement childId)
        (Dom.getElement containerId)
        (Dom.getViewportOf containerId)
        |> Task.andThen
            (\( child, container, { viewport, scene } ) ->
                case viewportAdjustment child container of
                    Just value ->
                        Dom.setViewportOf containerId 0 (viewport.y + value)

                    Nothing ->
                        Task.succeed ()
            )
