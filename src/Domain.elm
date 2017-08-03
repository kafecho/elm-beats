module Domain exposing (..)

import Array exposing (..)
import Navigation exposing (..)


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
    , muted : Bool
    }



{- A score is just a bunch of instruments playing together -}


type alias Score =
    Array Pattern


type alias SampleAlias =
    String


type alias SampleUrl =
    String


type alias Model =
    { tempo : Float
    , score : Score
    , startClockValue : Maybe Float
    , semiQuaverIndex : Maybe Int
    , location : Maybe Location
    }


type Msg
    = Start
    | UpdateTempo String
    | Stop
    | ClearScore
    | Share
    | OnLocationChanged Location
    | AudioClockUpdate Float
    | OnNoteClicked Instrument Int Int
    | OnInstrumentClicked Instrument
    | ClearPattern Int
    | OnMuteToggled Int


isPlaying : Model -> Bool
isPlaying model =
    model.startClockValue /= Nothing



-- An empty pattern, 16 semi quavers of silence


emptyPattern : Array Int
emptyPattern =
    Array.repeat 16 0



-- The starting score is all instruments playing nothing


startingScore : Score
startingScore =
    Array.fromList (List.map (\instrument -> Pattern instrument emptyPattern False) instruments)



-- The Web Audio API schedules sample playbacks in the near future.
-- This parameter controls this scheduling delay.


delay : Float
delay =
    0.25


minTempo : Float
minTempo =
    80.0


maxTempo : Float
maxTempo =
    220.0


defaultTempo : Float
defaultTempo =
    100.0



{- We only deal with 1 bar with a 4/4 signature -}


computeBarDuration : Float -> Float
computeBarDuration tempo =
    (4 * 60) / tempo


elapsedTime : Maybe Float -> Float -> Float
elapsedTime currentTime newTime =
    case currentTime of
        Nothing ->
            0

        Just time ->
            newTime - time
