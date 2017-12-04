module Json.Encoder.Combinator exposing (Encoder, string, int, float, array, object, entry)

{-| Combinator-style interface to Elm JSON encoding.

@docs Encoder

# Primitives

Encoders for primitive types are just passed through from `Json.Encode`. I exposed them here
just to make this library more self-contained.

@docs int, float, bool, null

# Combinators

This is where this library differs from `Json.Encode`. Each of these functions builds an encoder
out of smaller encoders rather than taking already-encoded values and marshalling them directly.

@docs list
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Encode


{-| The type of encoders that encode an elm value of type `a`.
-}
type alias Encoder a =
    a -> Json.Encode.Value


{-| Encodes a string as a JSON string.
-}
string : Encoder String
string =
    Json.Encode.string


{-| Encodes an int as a JSON number.
-}
int : Encoder Int
int =
    Json.Encode.int


{-| Encodes a float as a JSON number.
-}
float : Encoder Float
float =
    Json.Encode.float


{-| Encodes a bool as a JSON boolean.
-}
bool : Encoder Bool
bool =
    Json.Encode.bool


{-| Encodes any elm value as the constant JSON `null` value.
-}
null : Encoder a
null _ =
    Json.Encode.null


{-| Encodes a list as a JSON array, using the given encoder on each element.
-}
list : Encoder a -> Encoder (List a)
list elementEncoder lst =
    List.map elementEncoder lst
        |> Json.Encode.list


{-| Encodes an array as a JSON array, using the given encoder on each element.
-}
array : Encoder a -> Encoder (Array a)
array elementEncoder arr =
    Array.map elementEncoder arr
        |> Json.Encode.array


{-| Encodes a value as a JSON object using the given entries to determine field values.
-}
object : List (ObjectEntry a) -> Encoder a
object entries val =
    let
        fieldRecord : ObjectEntry a -> Maybe ( String, Json.Encode.Value )
        fieldRecord (ObjectEntry name toJson) =
            toJson val |> Maybe.map (\encoded -> ( name, encoded ))
    in
        List.filterMap fieldRecord entries |> Json.Encode.object


type ObjectEntry a
    = ObjectEntry String (a -> Maybe Json.Encode.Value)


{-| Describes a single entry in an object. See `object` for details.
-}
entry : String -> (a -> b) -> Encoder b -> ObjectEntry a
entry field selector encoder =
    ObjectEntry field (selector >> encoder >> Just)


{-| Describes an entry that should be included in an output object conditionally.
-}
maybeEntry : String -> (a -> Maybe b) -> Encoder b -> ObjectEntry a
maybeEntry field selector encoder =
    ObjectEntry field (selector >> Maybe.map encoder)


{-| Encodes a dictionary as an object, using the given encoder for values.
 -}
dictObject : Encoder a -> Encoder (Dict String a)
dictObject valueEncoder dict =
    Dict.toList dict
        |> List.map (\( k, v ) -> ( k, valueEncoder v ))
        |> Json.Encode.object


{-| Wraps an encoder of `a` to handle `Maybe a` by encoding `Nothing` as `null`.
 -}
nullable : Encoder a -> Encoder (Maybe a)
nullable encoder maybeValue =
    case maybeValue of
        Nothing ->
            Json.Encode.null

        Just value ->
            encoder value

{-| Adapts a value to "fit" an existing encoder.

Suppose you have an encoder for lists, and you'd like to encode 
-}
adapt : (a -> b) -> Encoder b -> Encoder a
adapt transformer encoder value =
    transformer value
        |> encoder
