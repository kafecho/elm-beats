module Scheduling exposing (..)

import Domain exposing (..)
import Ports exposing (..)
import Array exposing (..)


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


schedulePatternPlayback : Int -> Float -> Pattern -> Cmd Msg
schedulePatternPlayback index when pattern =
    if (Array.get index pattern.notes == Just 1 && pattern.muted == False) then
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
