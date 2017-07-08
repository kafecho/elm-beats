Beats by Elm or "Yo Yo. Let's make some noise".

## Overview

This is hopefully the start of a fully fledged audio app written in Elm.

For now, it is just a simple drum machine playing a score which is defined in code. It sequences a bunch of drum kit samples defined in the samples folder. The playback is done via the Web Audio API with Elm taking care of sequencing things so they play at the right time.

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

I only spent an afternoon working on this, so there is a lot more to do.

As you can see the UI is rather sparse. At the very least, I would like to add a grid like UI so you can schedule which instrument is playing when.
