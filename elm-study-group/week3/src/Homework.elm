module Homework exposing (find, isJust, maybeToList, updateList, updateListKv)


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


find : (a -> Bool) -> List a -> Maybe a
find f xss =
    let
        aaa list =
            case list of
                [] ->
                    Nothing

                h :: tail ->
                    Just h
    in
    aaa (List.filter f xss)


isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        Just a ->
            True


updateListKv : List ( k, v ) -> k -> (v -> Maybe v) -> List ( k, v )
updateListKv old k f =
    let
        aaa list =
            case list of
                [] ->
                    []

                x :: xs ->
                    if Tuple.first x == k then
                        case f (Tuple.second x) of
                            Nothing ->
                                aaa xs

                            Just a ->
                                ( Tuple.first x, a ) :: aaa xs

                    else
                        x :: aaa xs
    in
    aaa old
