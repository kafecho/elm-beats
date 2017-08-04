-- A bunch of ports to talk to the Web Audio API


port module Ports exposing (..)

import Domain exposing (..)


{-
   Start the JavaScript loop that sends audio clock updates to Elm.
   The JavaScript loop is tidied to a requestAnimationFrame call.
-}


port startAudioClock : () -> Cmd msg



-- Stop the JavaScript process that sends audio clock updates to Elm.


port stopAudioClock : () -> Cmd msg



-- Elm subscription to receive audio clock updates in the form of a float value.


port audioClockUpdate : (Float -> msg) -> Sub msg



{- Load a sample by giving it a name and the URL of the audio file (for example .wav or .mp3) to decode. -}


port loadSample : ( SampleAlias, SampleUrl ) -> Cmd msg



{- Play a given sample at given point in time in the future -}


port playSample : ( SampleAlias, Float ) -> Cmd msg
