module Filters exposing (Filter, Filters, addFilterToFragment, empty, filtersToFragment, removeFilterFromFragment, urlToFilters)

import Dict
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


type alias Filterable entry =
    { entry | name : String, date : String }


type alias Filters entry =
    Dict.Dict String (Filter entry)


type alias Filter entry =
    { value : String
    , entryFieldGetter : EntryFieldGetter entry
    , compareFunc : CompareFunc
    }


type alias EntryFieldGetter entry =
    Filterable entry -> String


type alias CompareFunc =
    String -> String -> Bool


type alias FilterData entry =
    { maybeVal : Maybe String
    , label : String
    , entryFieldGetter : EntryFieldGetter entry
    , compareFunc : CompareFunc
    }


empty : Filters entry
empty =
    Dict.empty


urlToFilters : Url.Url -> Filters entry
urlToFilters url =
    case url.fragment of
        Just fragment ->
            let
                -- The filters query is stored in the fragment, eg #?name=foo&from=2018-09-25
                filtersQuery =
                    fragment
                        |> String.dropLeft 1

                -- Save it in the Url's query, so we can use Url.Parser.Query to parse the filters from the Url.
                -- Also discard the "path", as we only care about the query, and we might have a path
                -- (if deployed in github pages for example) that would break the parser.
                urlWithFragmentAsQuery =
                    { url | query = Just filtersQuery, fragment = Nothing, path = "/" }

                queryParser =
                    Url.Parser.Query.map3
                        addFilters
                        (Url.Parser.Query.string "name")
                        (Url.Parser.Query.string "from")
                        (Url.Parser.Query.string "until")
            in
            Url.Parser.parse (Url.Parser.top <?> queryParser) urlWithFragmentAsQuery
                |> Maybe.withDefault empty

        Nothing ->
            empty


addFilters : Maybe String -> Maybe String -> Maybe String -> Filters entry
addFilters name from until =
    let
        maybeAddFilter : FilterData entry -> Filters entry -> Filters entry
        maybeAddFilter { maybeVal, label, entryFieldGetter, compareFunc } filters =
            case maybeVal of
                Just value ->
                    Dict.insert label (Filter value entryFieldGetter compareFunc) filters

                Nothing ->
                    filters
    in
    List.foldl maybeAddFilter
        empty
        [ { maybeVal = name, label = "name", entryFieldGetter = .name, compareFunc = (==) }
        , { maybeVal = from, label = "from", entryFieldGetter = .date, compareFunc = (>=) }
        , { maybeVal = until, label = "until", entryFieldGetter = .date, compareFunc = (<=) }
        ]


filtersToFragment : Filters entry -> String
filtersToFragment filters =
    let
        query =
            filters
                |> Dict.toList
                |> List.map (\( label, filter ) -> Url.Builder.string label filter.value)
                |> Url.Builder.toQuery
    in
    "#" ++ query


addFilterToFragment : Filters entry -> String -> Filter entry -> String
addFilterToFragment filters filterLabel filter =
    Dict.insert filterLabel filter filters
        |> filtersToFragment


removeFilterFromFragment : Filters entry -> String -> String
removeFilterFromFragment filters filterLabel =
    Dict.remove filterLabel filters
        |> filtersToFragment
