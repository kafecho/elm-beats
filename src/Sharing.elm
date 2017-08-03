module Sharing exposing (decodeFromUrlFragment, encodeToUrlFragment)

import Domain exposing (..)
import Array exposing (..)


notesToInt : Array Int -> Int
notesToInt notes =
    let
        powers =
            Array.indexedMap (\index value -> value * 2 ^ (15 - index)) notes

        _ =
            Debug.log "powers" powers
    in
        Array.foldl (\x y -> x + y) 0 powers


binaryListFromInt : Int -> List Int -> List Int
binaryListFromInt value soFar =
    let
        divide =
            value // 2

        remainder =
            rem value 2

        updatedList =
            remainder :: soFar
    in
        if (divide == 0) then
            List.reverse updatedList
        else
            binaryListFromInt remainder updatedList


notesFromIntImpl : Int -> Int -> Array Int -> Array Int
notesFromIntImpl index src soFar =
    let
        power =
            2 ^ (15 - index)

        division =
            src // power

        remainder =
            rem src power

        updated =
            Array.set index division soFar
    in
        if (remainder == 0 || index == 15) then
            updated
        else
            notesFromIntImpl (index + 1) remainder updated


notesFromInt : Int -> Array Int
notesFromInt value =
    notesFromIntImpl 0 value (Array.repeat 16 0)


encodeScoreNotes : Score -> Array Int
encodeScoreNotes score =
    Array.map (\pattern -> notesToInt pattern.notes) score


decodeTempo tempo =
    String.toFloat tempo |> Result.withDefault defaultTempo


buildPattern : Int -> Array Instrument -> Int -> Pattern
buildPattern index instruments int_value =
    let
        instrument =
            Array.get index instruments |> Maybe.withDefault Kick

        notes =
            notesFromInt int_value
    in
        Pattern instrument notes False


decodePatterns : List String -> Array Pattern
decodePatterns strings =
    let
        as_ints =
            List.map (\token -> Result.withDefault 0 (String.toInt token)) strings

        instruments_as_array =
            Array.fromList instruments

        patterns =
            List.indexedMap (\index int_value -> buildPattern index instruments_as_array int_value) as_ints
    in
        Array.fromList patterns



{-
   Share a given drum performance (identified by a score played at a certain tempo) to a URL fragment.
   A pattern of semi quavers (1 for playing, 0 for silent) is just a binary string of 16 digits (i.e. short).
   The encoding converts each pattern to its corresponding decimal value.
-}


encodeToUrlFragment : Float -> Score -> String
encodeToUrlFragment tempo score =
    let
        encodedNotes =
            Array.map toString (encodeScoreNotes score) |> Array.toList |> String.join (",")

        urlFragment =
            toString (tempo) ++ "," ++ encodedNotes
    in
        urlFragment



{- Decode a drum performance (identified by a score played at a certain tempo) from a URL fragment -}


decodeFromUrlFragment : String -> ( Float, Score )
decodeFromUrlFragment urlFragment =
    let
        tokens =
            String.split "," urlFragment
    in
        case tokens of
            [] ->
                ( defaultTempo, startingScore )

            tempo :: [] ->
                ( decodeTempo tempo, startingScore )

            tempo :: patterns ->
                ( decodeTempo tempo, decodePatterns patterns )
