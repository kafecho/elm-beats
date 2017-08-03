module Views exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Navigation exposing (..)
import Domain exposing (..)


createCell : Bool -> Instrument -> Int -> Int -> Int -> Html Msg
createCell muted instrument instrumentIndex noteIndex flag =
    let
        playOnMuted =
            flag == 1 && muted == True

        playOnUnMuted =
            flag == 1 && muted == False
    in
        td
            [ class "note-cell"
            , classList [ ( "play-on", playOnUnMuted ), ( "play-on-muted", playOnMuted ), ( "quarter-note-start", noteIndex % 4 == 0 ) ]
            , onClick (OnNoteClicked instrument instrumentIndex noteIndex)
            ]
            []


renderMuteToggle : Pattern -> Html Msg
renderMuteToggle pattern =
    if (pattern.muted) then
        -- text "Un-mute"
        i [ class "fa fa-volume-off" ] []
    else
        i [ class "fa fa-volume-up" ] []


render : Int -> Pattern -> Html Msg
render instrumentIndex pattern =
    let
        notes =
            pattern.notes

        instrumentCell =
            td [ class "instrument-cell", onClick (OnInstrumentClicked pattern.instrument) ] [ text (toString pattern.instrument) ]

        muteToggleCell =
            td [ class "mute-toggle-cell", onClick (OnMuteToggled instrumentIndex) ] [ renderMuteToggle pattern ]

        noteCells =
            Array.toList (Array.indexedMap (createCell pattern.muted pattern.instrument instrumentIndex) notes)

        clearCell =
            List.singleton
                (td
                    [ class "clear-cell", onClick (ClearPattern instrumentIndex) ]
                    [ i [ class "fa fa-close" ] [] ]
                )

        cells =
            instrumentCell :: muteToggleCell :: noteCells
    in
        tr [] (List.append cells clearCell)



{-
   Thanks to https://stackoverflow.com/a/33860064/453932
-}


sliderView : Model -> Html Msg
sliderView model =
    div [ id "slider-control" ]
        [ input
            [ type_ "range"
            , Html.Attributes.min (toString minTempo)
            , Html.Attributes.max (toString maxTempo)
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
        , div [] [ h2 [] [ text "A drum machine powered by Elm and the Web Audio API" ] ]
        , div []
            [ sliderView model
            , button [ onClick Start, disabled (isPlaying model) ] [ text "Start" ]
            , button [ onClick Stop, disabled (not (isPlaying model)) ] [ text "Stop" ]
            , button [ onClick ClearScore ] [ text "Clear" ]
            ]
        , div [] (Array.toList (Array.indexedMap render model.score))
        , div []
            [ text "Share this via a hashtag "
            , button [ title "Click this and share your browser's URL", onClick Share ] [ text "Share" ]
            ]
        , footer [ id "footer" ]
            [ div [ id "about-me" ]
                [ text "An Elm experiment, made in South Africa by "
                , a [ href "https://twitter.com/gbelrose" ] [ text "Guillaume Belrose" ]
                ]
            , div [ id "inspired-by" ]
                [ text "Heavily inspired by this "
                , a [ href "https://learningmusic.ableton.com/" ] [ text "awesome Ableton web app !!" ]
                ]
            ]
        ]
