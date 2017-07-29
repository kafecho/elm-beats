Beats by Elm or "Yo Yo. Let's make some noise".

See it [live](http://www.soundcode.co.za/elm-beats-demo/).

## Overview

This is hopefully the start of a fully fledged audio app written in Elm.

For now, this is just a drum machine. Just click on a grid to create a pattern. Adjust the tempo to your liking and share your creations via a URL fragment. 

Under the hood, the code sequences a bunch of drum kit samples defined in the samples folder. The playback is done via the Web Audio API with Elm taking care of sequencing things so they play at the right time.

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).
Follow the instructions on the web site to install it.

The inspiration for this work comes from the very awesome [Ableton experiment](https://learningmusic.ableton.com/)

## Run the app in live mode

Just do `elm-app start`
This runs the app in the development mode.  
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits so you make changes to the Score being played.   
You will also see any lint errors in the console.

## Much more to do.

If you have any idea for improvements, feel free to file a Github Issue and I will take a look.
