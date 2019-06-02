module Select.ScrollUtils exposing
    ( scrollToElement
    , scrollToNavigationIndex
    , toChildId
    , toContainerId
    )

import Browser.Dom as Dom
import Select.Types exposing (NavigationIndex(..))
import Task


type ViewportPosition
    = Above
    | Below
    | Inside


type ChildId
    = ChildId String


toChildId =
    ChildId


type ContainerId
    = ContainerId String


toContainerId =
    ContainerId


viewportPosition : Dom.Element -> Dom.Element -> ViewportPosition
viewportPosition child container =
    if offsetTop child container < 0 then
        Above

    else if offsetTop child container + child.element.height > container.element.height then
        Below

    else
        Inside


offsetTop : Dom.Element -> Dom.Element -> Float
offsetTop child container =
    child.element.y - container.element.y


scrollToElement : ChildId -> ContainerId -> Task.Task Dom.Error ()
scrollToElement (ChildId childId) (ContainerId containerId) =
    Task.map2
        offsetTop
        (Dom.getElement childId)
        (Dom.getElement containerId)
        |> Task.andThen
            (\top -> Dom.setViewportOf containerId 0 top)


scrollToNavigationIndex : ChildId -> ContainerId -> Int -> NavigationIndex -> Task.Task Dom.Error ()
scrollToNavigationIndex (ChildId childId) (ContainerId containerId) length (NavigationIndex index) =
    Task.map3
        (\option -> \container -> \viewport -> ( option, container, viewport ))
        (Dom.getElement childId)
        (Dom.getElement containerId)
        (Dom.getViewportOf containerId)
        |> Task.andThen
            (\( option, container, { viewport, scene } ) ->
                case viewportPosition option container of
                    Above ->
                        if index == 0 then
                            Dom.setViewportOf containerId 0 0

                        else
                            Dom.setViewportOf containerId 0 (viewport.y - option.element.height)

                    Below ->
                        if index == length - 1 then
                            Dom.setViewportOf containerId 0 scene.height

                        else
                            Dom.setViewportOf containerId 0 (viewport.y + option.element.height)

                    Inside ->
                        Task.succeed ()
            )
