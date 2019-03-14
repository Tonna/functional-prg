module Homework exposing (Address, Profile, User, andMap, bird, bird2, bird3, buldStatsUrl, catMaybes, convert, convert2, convert3, mapMaybes, setPhone)

import Date exposing (Date)
import Url.Builder as UrlBuilder


convert :
    List { name : String, email : String, phone_number : String }
    -> List { name : String, email : String }
convert =
    List.map (\e -> { name = e.name, email = e.email })


andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
    Maybe.map2 (|>)



--List.map (Maybe.map (\x -> x * 2)) [Just 1, Just 2, Just 3]


convert2 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert2 list =
    let
        aaa =
            \x ->
                Just (\n e -> { name = n, email = e })
                    |> andMap x.name
                    |> andMap x.email
    in
    catMaybes (List.map aaa list)


convert3 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert3 list =
    catMaybes
        (List.map
            (\x ->
                Maybe.map
                    (\n ->
                        { name = Maybe.withDefault "<undefined>" x.name
                        , email = Maybe.withDefault "<unspecified>" x.email
                        }
                    )
                    x.name
            )
            list
        )


bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))


bird2 : Int
bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr [ 1, 2, 3 ]


bird3 : Int
bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.map incr [ 1, 2, 3 ] |> List.filter notThree |> List.sum


type alias User =
    { profile : Profile }


type alias Profile =
    { address : Address }


type alias Address =
    { phone : String }


setPhone : String -> User -> User
setPhone =
    Debug.todo ""


mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes func list =
    case list of
        [] ->
            []

        x :: xs ->
            case func x of
                Nothing ->
                    mapMaybes func xs

                Just a ->
                    a :: mapMaybes func xs


catMaybes : List (Maybe a) -> List a
catMaybes list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    catMaybes xs

                Just a ->
                    a :: catMaybes xs


buldStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buldStatsUrl itemId ps =
    "https://myapi.com"
        ++ UrlBuilder.absolute
            [ "api", "item", String.fromInt itemId, "stats.json" ]
            (catMaybes
                [ Maybe.map (UrlBuilder.string "start_date") ps.startDate
                , Maybe.map (UrlBuilder.string "num_items" << String.fromInt) ps.numElems
                ]
            )
