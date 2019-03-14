module Homework exposing (compress, isPalindrome, myButLast, myLast, myLength, myReverse)


myLast : List Int -> Maybe Int
myLast xs =
    case xs of
        [] ->
            Nothing

        y :: [] ->
            Just y

        y :: x ->
            myLast x


myButLast : List Int -> Maybe Int
myButLast xs =
    case xs of
        [] ->
            Nothing

        y :: x :: [] ->
            Just y

        y :: x ->
            myButLast x


myLength : List Int -> Int
myLength input =
    let
        count : ( List Int, Int ) -> Int
        count ( xs, accum ) =
            case xs of
                [] ->
                    accum

                y :: x ->
                    count ( x, accum + 1 )
    in
    count ( input, 0 )


myReverse : List Int -> List Int
myReverse xs =
    case xs of
        [] ->
            []

        x :: [] ->
            xs

        x :: y ->
            myReverse y ++ [ x ]


isPalindrome : List Int -> Bool
isPalindrome input =
    case input of
        [] ->
            True

        x ->
            x == myReverse x


compress : String -> String
compress str =
    let
        isIn : Char -> List Char -> Bool
        isIn char list =
            case ( char, list ) of
                ( a, [] ) ->
                    False

                ( a, x :: xs ) ->
                    a == x

        tryAdd : Char -> List Char -> List Char
        tryAdd char list =
            case ( char, list ) of
                ( a, [] ) ->
                    [ a ]

                ( a, b ) ->
                    if isIn a b then
                        b
                    else
                        a :: b

        doStuff : List Char -> List Char -> List Char
        doStuff list accum =
            case ( list, accum ) of
                ( [], [] ) ->
                    []

                ( [], a ) ->
                    a

                ( x :: [], a ) ->
                    tryAdd x a

                ( x :: xs, a ) ->
                    doStuff xs (tryAdd x a)
    in
    String.fromList (doStuff (String.toList str) [])
