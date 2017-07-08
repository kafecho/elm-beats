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


delay =
    0.25



{-
   Tempo expressed in beats per minute
-}


tempo : Float
tempo =
    95.0



{- We only deal with 1 bar with a 4/4 signature -}


barDuration : Float
barDuration =
    (4 * 60) / tempo



{- For each instrument defined here, there ought to be a corresponding Instrument.wav in the sample folder -}


type Instrument
    = Kick
    | HiHatClosed
    | HiHatOpen
    | Snare
    | Synth



{- A pattern is just an instrument playing a bunch of 16th notes -}


type alias Pattern =
    { instrument : Instrument
    , notes : Array Int
    }



{- A score is just a bunch of instruments playing together -}


type alias Score =
    List Pattern



{-
   What each instrument is playing.
   If you use https://github.com/halfzebra/create-elm-app then you can modify the Score live.
-}


score : Score
score =
    [ Pattern Kick (Array.fromList [ 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0 ])
    , Pattern Snare (Array.fromList [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ])
    , Pattern HiHatClosed (Array.fromList [ 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1 ])
    , Pattern HiHatOpen (Array.fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 ])
    , Pattern Synth (Array.fromList [ 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0 ])
    ]


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
            List.map (\pattern -> loadInstrument pattern.instrument) score
    in
        Cmd.batch commands


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


findSemiQuaverIndex : Float -> ( Int, Float )
findSemiQuaverIndex elapsed =
    let
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
    { startClockValue : Maybe Float, semiQuaverIndex : Maybe Int }


type Msg
    = Start
    | Stop
    | AudioClockUpdate Float


init : String -> ( Model, Cmd Msg )
init path =
    ( { startClockValue = Nothing, semiQuaverIndex = Nothing }, loadScoreInstruments score )



{- Schedule the sample for a given instrument according to the instruments pattern. -}


schedulePatternPlayback : Int -> Float -> Pattern -> Cmd Msg
schedulePatternPlayback index when pattern =
    if (Array.get index pattern.notes == Just 1) then
        playSample ( toString pattern.instrument, when )
    else
        Cmd.none



{- Schedule the playback for the entire score, based on what each instrument is playing. -}


scheduleScorePlayback : Int -> Float -> List Pattern -> Cmd Msg
scheduleScorePlayback index when score =
    let
        commands =
            List.map (\pattern -> schedulePatternPlayback index when pattern) score
    in
        Cmd.batch commands


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | startClockValue = Nothing }, startAudioClock () )

        Stop ->
            ( { model | startClockValue = Nothing }, stopAudioClock () )

        AudioClockUpdate value ->
            {- So when get a clock tick, we find out which 16th note should be played. -}
            let
                elapsed =
                    elapsedTime model.startClockValue value

                ( index, lateBy ) =
                    findSemiQuaverIndex elapsed

                command =
                    if (Just (index) /= model.semiQuaverIndex) then
                        scheduleScorePlayback index (value - lateBy + delay) score
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


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Elm Beats. Work in progress!!!" ]
        , div []
            [ button [ onClick Start, disabled (isPlaying model) ] [ text "Start" ]
            , button [ onClick Stop, disabled (not (isPlaying model)) ] [ text "Stop" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    audioClockUpdate AudioClockUpdate


main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
