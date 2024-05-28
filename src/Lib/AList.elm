module Lib.AList exposing (AList, empty, extend, find)


type alias AList k v =
    List ( k, v )


empty : AList k v
empty =
    []


find : k -> AList k v -> Maybe v
find searchKey alist =
    case alist of
        [] ->
            Nothing

        ( key, value ) :: restAList ->
            if searchKey == key then
                Just value

            else
                find searchKey restAList


extend : k -> v -> AList k v -> AList k v
extend key value alist =
    ( key, value ) :: alist
