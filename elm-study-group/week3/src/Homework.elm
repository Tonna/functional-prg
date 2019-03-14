module Homework exposing (maybeToList, updateList)


maybeToList : Maybe a -> List a
maybeToList x =
    case x of
        Nothing ->
            []

        Just a ->
            [ a ]


updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList shouldChange f xs =
    let
        aaa list =
            case list of
                [] ->
                    []

                h :: t ->
                    if shouldChange h then
                        maybeToList (f h) ++ aaa t

                    else
                        h :: aaa t
    in
    aaa xs
