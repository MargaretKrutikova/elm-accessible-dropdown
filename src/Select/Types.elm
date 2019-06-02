module Select.Types exposing (NavigationIndex(..), getByIndex, isEqual)

import Array exposing (Array)


type NavigationIndex
    = NavigationIndex Int


isEqual : NavigationIndex -> Int -> Bool
isEqual (NavigationIndex index) =
    (==) index


getByIndex : NavigationIndex -> Array a -> Maybe a
getByIndex (NavigationIndex index) array =
    Array.get index array
