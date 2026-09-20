// shinyjs functions needed for formant_app
// NB: ui.R should include "includeScript("www/formant_app_shinyjs.js")"

// Functions imported by shinyjs - must begin with "shinyjs." and have "params"
shinyjs.playme_js = function(params) {
    let a = document.getElementById(params.audio_id);
    if (!a) return;

    // Clean up old listeners to prevent duplicates
    if (a._timeUpdateListener) a.removeEventListener('timeupdate', a._timeUpdateListener);
    if (a._endedListener) a.removeEventListener('ended', a._endedListener);

    a.currentTime = params.from;
    a.playbackRate = params.rate || 1.0;
    a.preservesPitch = false; // Shifts pitch with speed, matching R's behavior
    a.webkitPreservesPitch = false; // Safari fallback

    const finishPlayback = function() {
        // Delegate to stopAudio_js to pause and cleanly remove listeners
        shinyjs.stopAudio_js({audio_id: params.audio_id});

        // Notify R that playback is complete.
        // Use Date.now() to ensure Shiny registers the event even if triggered multiple times.
        Shiny.setInputValue('audio_playback_done', Date.now(), {priority: 'event'});

        // NOTE: We intentionally DO NOT send audio_current_time here.
        // This allows R's stopPlay() to restore the original_cursor without being overwritten.
    };

    a._timeUpdateListener = function() {
        // Send current time to R to move the cursor continuously
        Shiny.setInputValue('audio_current_time', a.currentTime);

        // Check if we reached the target time OR if the audio naturally ended
        if (a.currentTime >= params.to || a.ended) {
            finishPlayback();
        }
    };

    a._endedListener = function() {
        finishPlayback();
    };

    a.addEventListener('timeupdate', a._timeUpdateListener);
    a.addEventListener('ended', a._endedListener);

    // Catch autoplay rejections
    let playPromise = a.play();
    if (playPromise !== undefined) {
        playPromise.catch(error => {
            console.error("Autoplay prevented:", error);
            finishPlayback();
        });
    }
};

shinyjs.stopAudio_js = function(params) {
    let a = document.getElementById(params.audio_id);
    if (!a) return;
    a.pause();
    if (a._timeUpdateListener) {
        a.removeEventListener('timeupdate', a._timeUpdateListener);
        a._timeUpdateListener = null;
    }
    if (a._endedListener) {
        a.removeEventListener('ended', a._endedListener);
        a._endedListener = null;
    }
    // Notify R that playback has stopped
    Shiny.setInputValue('audio_playback_done', Date.now(), {priority: 'event'});
};

shinyjs.play_synth_uri = function(params) {
    // lazily create the element only when needed, ensuring document.body exists
    var asynth = document.getElementById('synthAudio');
    if (!asynth) {
        asynth = document.createElement('audio');
        asynth.id = 'synthAudio';
        asynth.style.display = 'none';
        document.body.appendChild(asynth);
    }
    asynth.src = params.uri;
    asynth.currentTime = 0;
    asynth.playbackRate = params.rate || 1.0;
    asynth.preservesPitch = false;
    asynth.webkitPreservesPitch = false; // Safari fallback
    asynth.play();
};


// Manually remove all brush div's in case the brush is not properly cleared
// (seems like a bug in Shiny)
shinyjs.clearBrush = function(params) {
    // select all elements whose id contains "_brush"
    // alert('running clearBrush');
    var myId = document.querySelectorAll('[id*=' + CSS.escape(params.s) + ']');
    for (var i=0; i < myId.length; i++) {
      // remove element (detour via its parentNode)
      myId[i].parentNode.removeChild(myId[i]);
    }
};

// Override Shiny's default assignment of height = '400px' to plot divisions
// (otherwise I can't find a way to make overlaid plots resizable)
shinyjs.inheritSize = function(params) {
    // params: parentDiv
    // alert('inheritSize');
    var plotDiv = document.getElementById(params.parentDiv).children;
    for (var i = 0; i < plotDiv.length; i++) {
      plotDiv[i].style.height = 'inherit';
    }
};

// Move the scrollbar as ordered from R
shinyjs.scrollBar = function(params) {
    // params: id, width, left
    var sl = document.getElementById(params.id);
    sl.style.width = params.width;
    sl.style.left = params.left;
};
