{- Elm Beats
   A very simple drum machine which uses the Web Audio API to sequence things.
   The code uses the Web Audio API to load and play audio samples.
   The sequencing is done in Elm, Elm receives the audio clock ticks and schedules playback of samples at precise intervals.
-}


port module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Navigation exposing (..)


delay : Float
delay =
    0.25



{- We only deal with 1 bar with a 4/4 signature -}


computeBarDuration : Float -> Float
computeBarDuration tempo =
    (4 * 60) / tempo



{- For each instrument defined here, there ought to be a corresponding Instrument.wav in the sample folder -}


type Instrument
    = Kick
    | Snare
    | RimShot
    | HiHatClosed
    | HiHatOpen
    | Ride
    | Clap
    | Cowbell


instruments : List Instrument
instruments =
    [ Kick
    , Snare
    , RimShot
    , HiHatClosed
    , HiHatOpen
    , Ride
    , Cowbell
    , Clap
    ]



{- A pattern is just an instrument playing a bunch of 16th notes -}


type alias Pattern =
    { instrument : Instrument
    , notes : Array Int
    }


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


encodeHashtag : Float -> Score -> String
encodeHashtag tempo score =
    let
        encodedNotes =
            Array.map toString (encodeScoreNotes score) |> Array.toList |> String.join (",")

        hashtag =
            toString (tempo) ++ "," ++ encodedNotes

        _ =
            Debug.log "Hashtag" hashtag
    in
        hashtag


decodeTempo tempo =
    String.toFloat tempo |> Result.withDefault 100


decodeHashtag : String -> ( Float, Score )
decodeHashtag hashtag =
    let
        tokens =
            String.split "," hashtag

        _ =
            Debug.log "tokens" tokens
    in
        case tokens of
            [] ->
                ( 100.0, startingScore )

            tempo :: [] ->
                ( decodeTempo tempo, startingScore )

            tempo :: patterns ->
                let
                    _ =
                        Debug.log "patterns" patterns
                in
                    ( decodeTempo tempo, decodePatterns patterns )


buildPattern : Int -> Array Instrument -> Int -> Pattern
buildPattern index instruments int_value =
    let
        instrument =
            Array.get index instruments |> Maybe.withDefault Kick

        notes =
            notesFromInt int_value
    in
        Pattern instrument notes


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



{- A score is just a bunch of instruments playing together -}


type alias Score =
    Array Pattern



{-
   What each instrument is playing.
   If you use https://github.com/halfzebra/create-elm-app then you can modify the Score live.
-}


emptyPattern =
    Array.repeat 16 0


startingScore : Score
startingScore =
    Array.fromList (List.map (\instrument -> Pattern instrument emptyPattern) instruments)


port startAudioClock : () -> Cmd msg


port audioClockUpdate : (Float -> msg) -> Sub msg


port stopAudioClock : () -> Cmd msg


type alias SampleAlias =
    String


type alias SampleUrl =
    String



{- Load a sample by giving it a name and the URL of the audio file to decode. -}


port loadSample : ( SampleAlias, SampleUrl ) -> Cmd msg



{- Play a given sample at given point in time in the future -}


port playSample : ( SampleAlias, Float ) -> Cmd msg



{- Load the sample corresponding to a given instrument. This assumes that the corresponding sample exists in the samples folder. -}


loadInstrument : Instrument -> Cmd msg
loadInstrument instrument =
    loadSample ( toString instrument, fileName instrument )


fileName : Instrument -> String
fileName instrument =
    "samples/" ++ (toString (instrument) ++ ".wav")



{- Tell the Web Audio API to load all samples for a given Score -}


loadScoreInstruments : Score -> Cmd msg
loadScoreInstruments score =
    let
        commands =
            Array.map (\pattern -> loadInstrument pattern.instrument) score
    in
        Cmd.batch (Array.toList commands)


elapsedTime : Maybe Float -> Float -> Float
elapsedTime currentTime newTime =
    case currentTime of
        Nothing ->
            0

        Just time ->
            newTime - time



{-
   Find which 16th note should be played given the current time (based on when the performance started).
   Basically, given the audio clock, we find the closest note just before it.
   We also return the offset (how far in advanced is the clock compared to the note it is supposed to be for).
   The offset is then used to schedule things in the future very precisely.
-}


findSemiQuaverIndex : Float -> Float -> ( Int, Float )
findSemiQuaverIndex tempo elapsed =
    let
        barDuration =
            computeBarDuration tempo

        nbTimes =
            floor (elapsed / barDuration)

        moduloElapsed =
            elapsed - (barDuration * toFloat (nbTimes))

        index =
            floor (moduloElapsed * 16 / barDuration)

        lateBy =
            moduloElapsed - toFloat (index) * barDuration / 16
    in
        ( index, lateBy )


type alias Model =
    { tempo : Float, score : Score, startClockValue : Maybe Float, semiQuaverIndex : Maybe Int, location : Maybe Location }


type Msg
    = Start
    | Stop
    | AudioClockUpdate Float
    | OnClick Instrument Int Int
    | Clear
    | UpdateTempo String
    | Encode
    | OnLocationChanged Location


init : String -> Location -> ( Model, Cmd Msg )
init path location =
    let
        _ =
            Debug.log "Location" location

        hashtag =
            String.filter (\char -> char /= '#') location.hash

        ( tempo, score ) =
            decodeHashtag hashtag
    in
        ( { location = Just location, tempo = tempo, score = score, startClockValue = Nothing, semiQuaverIndex = Nothing }, loadScoreInstruments score )



{- Schedule the sample for a given instrument according to the instruments pattern. -}


schedulePatternPlayback : Int -> Float -> Pattern -> Cmd Msg
schedulePatternPlayback index when pattern =
    if (Array.get index pattern.notes == Just 1) then
        playSample ( toString pattern.instrument, when )
    else
        Cmd.none



{- Schedule the playback for the entire score, based on what each instrument is playing. -}


scheduleScorePlayback : Int -> Float -> Score -> Cmd Msg
scheduleScorePlayback index when score =
    let
        commands =
            Array.map (\pattern -> schedulePatternPlayback index when pattern) score
    in
        Cmd.batch (Array.toList commands)


toggleNote : Pattern -> Int -> Pattern
toggleNote pattern noteIndex =
    let
        _ =
            Debug.log "Note index" noteIndex

        currentNote =
            Array.get noteIndex pattern.notes |> Maybe.withDefault 0

        newNote =
            1 - currentNote

        newNotes =
            Array.set noteIndex newNote pattern.notes
    in
        Pattern pattern.instrument newNotes


playNote : Int -> Instrument -> Cmd Msg
playNote note instrument =
    if (note == 1) then
        playSample ( toString instrument, 0 )
    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChanged location ->
            let
                hashtag =
                    String.filter (\char -> char /= '#') location.hash

                ( tempo, score ) =
                    decodeHashtag hashtag
            in
                ( { model | score = score, tempo = tempo }, Cmd.none )

        UpdateTempo s ->
            let
                tempo =
                    String.toFloat s |> Result.withDefault 100.0
            in
                ( { model | tempo = tempo }, Cmd.none )

        Encode ->
            case model.location of
                Nothing ->
                    ( model, Cmd.none )

                Just location ->
                    let
                        encoded =
                            encodeHashtag model.tempo model.score

                        url =
                            location.protocol ++ "//" ++ location.host ++ "/#" ++ encoded

                        _ =
                            Debug.log "hashtag" url
                    in
                        ( model, Navigation.newUrl url )

        Start ->
            ( { model | startClockValue = Nothing }, startAudioClock () )

        Stop ->
            ( { model | startClockValue = Nothing }, stopAudioClock () )

        Clear ->
            ( { model | score = startingScore }, Cmd.none )

        OnClick instrument instrumentIndex noteIndex ->
            let
                _ =
                    Debug.log "Instrument" instrument

                _ =
                    Debug.log "Instrument index" instrumentIndex

                _ =
                    Debug.log "Note index" noteIndex

                pattern_maybe =
                    Array.get instrumentIndex model.score

                _ =
                    Debug.log "Pattern_maybe" pattern_maybe

                note_maybe =
                    Maybe.andThen (\pattern -> Array.get noteIndex pattern.notes) pattern_maybe
            in
                case ( pattern_maybe, note_maybe ) of
                    ( Just pattern, Just note ) ->
                        let
                            updatedPattern =
                                toggleNote pattern noteIndex

                            updatedScore =
                                Array.set instrumentIndex updatedPattern model.score

                            playCommand =
                                if ((not (isPlaying model)) && note == 0) then
                                    playSample ( toString instrument, 0 )
                                else
                                    Cmd.none
                        in
                            ( { model | score = updatedScore }, playCommand )

                    _ ->
                        ( model, Cmd.none )

        AudioClockUpdate value ->
            {- So when get a clock tick, we find out which 16th note should be played. -}
            let
                elapsed =
                    elapsedTime model.startClockValue value

                ( index, lateBy ) =
                    findSemiQuaverIndex model.tempo elapsed

                command =
                    if (Just (index) /= model.semiQuaverIndex) then
                        scheduleScorePlayback index (value - lateBy + delay) model.score
                    else
                        Cmd.none

                startClockValue =
                    if (model.startClockValue == Nothing) then
                        Just value
                    else
                        model.startClockValue
            in
                ( { model | startClockValue = startClockValue, semiQuaverIndex = Just index }, command )


isPlaying : Model -> Bool
isPlaying model =
    model.startClockValue /= Nothing


createCell : Instrument -> Int -> Int -> Int -> Html Msg
createCell instrument instrumentIndex noteIndex flag =
    td
        [ class "note-cell"
        , classList [ ( "play-on", flag == 1 ), ( "quarter-note-start", noteIndex % 4 == 0 ) ]
        , onClick (OnClick instrument instrumentIndex noteIndex)
        ]
        []


render : Int -> Pattern -> Html Msg
render instrumentIndex pattern =
    let
        notes =
            pattern.notes

        instrumentCell =
            td [ class "instrument-cell" ] [ text (toString pattern.instrument) ]

        noteCells =
            Array.toList (Array.indexedMap (createCell pattern.instrument instrumentIndex) notes)
    in
        tr [] (instrumentCell :: noteCells)



{-
   Thanks to https://stackoverflow.com/a/33860064/453932
-}


sliderView : Model -> Html Msg
sliderView model =
    div [ id "slider-control" ]
        [ input
            [ type_ "range"
            , Html.Attributes.min "80"
            , Html.Attributes.max "140"
            , value <| toString model.tempo
            , onInput UpdateTempo
            ]
            []
        , text ((toString model.tempo) ++ " bpm")
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ h1 [] [ text "Elm Beats!!!" ] ]
        , div []
            [ sliderView model
            , button [ onClick Start, disabled (isPlaying model) ] [ text "Start" ]
            , button [ onClick Stop, disabled (not (isPlaying model)) ] [ text "Stop" ]
            , button [ onClick Clear ] [ text "Clear" ]
            ]
        , div [] (Array.toList (Array.indexedMap render model.score))
        , div []
            [ text "Share this via a hashtag "
            , button [ title "Save your pattern in a browser URL", onClick Encode ] [ text "Share" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    audioClockUpdate AudioClockUpdate


main =
    Navigation.programWithFlags OnLocationChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
