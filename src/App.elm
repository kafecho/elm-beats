{- Elm Beats
   A very simple drum machine which uses the Web Audio API to sequence things.
   The code uses the Web Audio API to load and play audio samples.
   The sequencing is done in Elm, Elm receives the audio clock ticks and schedules playback of samples at precise intervals.
-}


module App exposing (..)

import Array exposing (..)
import Navigation exposing (..)
import Domain exposing (..)
import Sharing exposing (..)
import Views exposing (..)
import Ports exposing (..)
import Scheduling exposing (..)


fileName : Instrument -> String
fileName instrument =
    "samples/" ++ (toString (instrument) ++ ".wav")



{- Load the sample corresponding to a given instrument. This assumes that the corresponding sample exists in the samples folder. -}


loadInstrument : Instrument -> Cmd msg
loadInstrument instrument =
    loadSample ( toString instrument, fileName instrument )



{- Tell the Web Audio API to load all samples for a given Score -}


loadScoreInstruments : Score -> Cmd msg
loadScoreInstruments score =
    let
        commands =
            Array.map (\pattern -> loadInstrument pattern.instrument) score
    in
        Cmd.batch (Array.toList commands)


toggleNote : Pattern -> Int -> Pattern
toggleNote pattern noteIndex =
    let
        currentNote =
            Array.get noteIndex pattern.notes |> Maybe.withDefault 0

        newNote =
            1 - currentNote

        newNotes =
            Array.set noteIndex newNote pattern.notes
    in
        Pattern pattern.instrument newNotes pattern.muted


playNote : Int -> Instrument -> Cmd Msg
playNote note instrument =
    if (note == 1) then
        playSample ( toString instrument, 0 )
    else
        Cmd.none



{- Schedule the sample for a given instrument according to the instruments pattern. -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMuteToggled index ->
            case Array.get index model.score of
                Just pattern ->
                    let
                        updatedPattern =
                            { pattern | muted = not (pattern.muted) }
                    in
                        ( { model | score = Array.set index updatedPattern model.score }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ClearPattern index ->
            case Array.get index model.score of
                Just pattern ->
                    let
                        updatedPattern =
                            { pattern | notes = emptyPattern }
                    in
                        ( { model | score = Array.set index updatedPattern model.score }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OnInstrumentClicked instrument ->
            if (not (isPlaying model)) then
                ( model, playSample ( toString instrument, 0.0 ) )
            else
                ( model, Cmd.none )

        OnLocationChanged location ->
            let
                urlFragment =
                    String.filter (\char -> char /= '#') location.hash

                ( tempo, score ) =
                    decodeFromUrlFragment urlFragment
            in
                ( { model | score = score, tempo = tempo }, Cmd.none )

        UpdateTempo s ->
            let
                tempo =
                    String.toFloat s |> Result.withDefault defaultTempo
            in
                ( { model | tempo = tempo }, Cmd.none )

        Share ->
            case model.location of
                Nothing ->
                    ( model, Cmd.none )

                Just location ->
                    let
                        encoded =
                            encodeToUrlFragment model.tempo model.score

                        url =
                            location.protocol ++ "//" ++ location.host ++ location.pathname ++ "#" ++ encoded
                    in
                        ( model, Navigation.newUrl url )

        Start ->
            ( { model | startClockValue = Nothing }, startAudioClock () )

        Stop ->
            ( { model | startClockValue = Nothing }, stopAudioClock () )

        ClearScore ->
            ( { model | score = startingScore }, Cmd.none )

        OnNoteClicked instrument instrumentIndex noteIndex ->
            let
                pattern_maybe =
                    Array.get instrumentIndex model.score

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


subscriptions : Model -> Sub Msg
subscriptions model =
    audioClockUpdate AudioClockUpdate


init : String -> Location -> ( Model, Cmd Msg )
init path location =
    let
        urlFragment =
            String.filter (\char -> char /= '#') location.hash

        ( tempo, score ) =
            decodeFromUrlFragment urlFragment
    in
        ( { location = Just location, tempo = tempo, score = score, startClockValue = Nothing, semiQuaverIndex = Nothing }, loadScoreInstruments score )


main : Program String Model Msg
main =
    Navigation.programWithFlags OnLocationChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
