require('./main.css');

var AudioContext = window.AudioContext || window.webkitAudioContext;
var audioContext = new AudioContext();

var Elm = require('./App.elm');

var samples = {};
var request = null;
var samples = {};

var root = document.getElementById('root');
var app = Elm.App.embed(root,"");

app.ports.startAudioClock.subscribe(function(){
  if (request == null){
    var readCurrentTime = function(){
      app.ports.audioClockUpdate.send(audioContext.currentTime);
      request = window.requestAnimationFrame ( readCurrentTime );
    };

    request = window.requestAnimationFrame ( readCurrentTime );
  }
});

app.ports.stopAudioClock.subscribe(function(){
  if (request){
    window.cancelAnimationFrame(request);
    request = null;
  }
});

var notifySampleLoadingFailed = function(url){
  console.log("Unable to load the sample @", url);
};

app.ports.loadSample.subscribe(function ( array ) {
    var key = array[0];
    var url = array[1];
    if (samples[key] == null){
      var request = new XMLHttpRequest();
      request.open('GET', url, true);
    request.responseType = 'arraybuffer';
    request.onload = function () {
        if (request.status === 200) {
            var audioData = request.response;
            audioContext.decodeAudioData(audioData,
                function (audioBuffer){
                    samples[key]=audioBuffer;
                    console.log("Loaded", key, url);
                },
                function (e){
                    notifySampleLoadingFailed(url);
                });
        } else {
            notifySampleLoadingFailed(url);
        }
    }

    request.onerror = function () {
        notifySampleLoadingFailed(url);
    }

    request.send();
    }
});


app.ports.playSample.subscribe(function (array){
    var key = array[0];
    var when = array[1];
    var source = audioContext.createBufferSource();

    source.buffer = samples[key];
    source.connect(audioContext.destination);

    source.start(when);
});
