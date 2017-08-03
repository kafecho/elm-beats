port module Ports exposing (..)

import Domain exposing (..)


port startAudioClock : () -> Cmd msg


port audioClockUpdate : (Float -> msg) -> Sub msg


port stopAudioClock : () -> Cmd msg



{- Load a sample by giving it a name and the URL of the audio file to decode. -}


port loadSample : ( SampleAlias, SampleUrl ) -> Cmd msg



{- Play a given sample at given point in time in the future -}


port playSample : ( SampleAlias, Float ) -> Cmd msg
