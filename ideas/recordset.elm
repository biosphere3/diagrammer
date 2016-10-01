import Focus exposing (Focus)


type alias RecordSet (Focus a comparable) a = Dict comparable a

getdict set = ???


get : comparable -> RecordSet f a -> Maybe a
get item set =
