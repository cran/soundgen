// js functions needed for formant_app
// NB: ui.R should include "includeScript("www/formant_app.js")"


document.addEventListener('DOMContentLoaded', () => {
  // execute when the page is loaded

  // Functions for making the scrollbar draggable
  // (see https://codepen.io/thenutz/pen/VwYeYEE for inspiration)
  const slider = document.getElementById('scrollBar');
  const track = document.getElementById('scrollBarCont');
  var max_right = track.clientWidth - slider.clientWidth;
  var mouseDown = false;
  var startX = 0, scrollBarLeft = 0, scrollBarLeft_new = 0;
  slider.style.left = '0%';
  var cntrlIsPressed = false;
  var spec = document.getElementById('plotRow');  // ideally the whole row instead of spec
  function rmPcnt(x) {
    return(Number(x.slice(0, -1)));
  };
  // rmPcnt('200%');  // returns "200" as a number

  slider.addEventListener('mousedown', (e) => {
    mouseDown = true;
    slider.classList.add('active');
    scrollBarLeft = rmPcnt(slider.style.left);
    // NB: operate with % not px b/c R doesn't know track.clientWidth
    startX = e.pageX / track.clientWidth * 100;
    // prevent the slider from going beyond its track
    max_right = 100 - rmPcnt(slider.style.width);
  });

  document.addEventListener('mousemove', (e) => {
    // NB: could be attached to slider or track instead of document,
    // but this way the slider can be dragged regardless of the vertical
    // position of the mouse - handy, ~Audacity
    if(!mouseDown) return;
    e.preventDefault();
    scrollBarLeft_new = scrollBarLeft + e.pageX / track.clientWidth * 100 - startX;

    // prevent the slider from going into negative or beyond audio dur
    if (scrollBarLeft_new < 0) {
      scrollBarLeft_new = 0;
    } else if (scrollBarLeft_new > max_right) {
      scrollBarLeft_new = max_right;
    }

    // move the slider to the next pos
    slider.style.left = scrollBarLeft_new + '%';
  });
  // Shiny.onInputChange(scrollBarLeft_new, scrollBarLeft_new);

  document.addEventListener('mouseup', () => {
    // debugger;
    mouseDown = false;
    slider.classList.remove('active');
    // send to R: scrollBarLeft as proportion of track width
    // https://shiny.rstudio.com/articles/communicating-with-js.html
    Shiny.setInputValue('scrollBarLeft', scrollBarLeft_new / 100);
  });

  // scroll by clicking the track left/right of the scrollbar
  track.addEventListener('mousedown', (e) => {
    var sl_rect = slider.getBoundingClientRect();
    if (e.clientX < sl_rect.left) {
      Shiny.setInputValue('scrollBarMove', 'l' + Math.random());
      // see https://shiny.rstudio.com/articles/communicating-with-js.html
      // with priority: event" it fires twice
    } else if (e.clientX > sl_rect.right) {
      Shiny.setInputValue('scrollBarMove', 'r' + Math.random());
    }
  });

  // mouse wheel events
  spec.addEventListener("wheel", event => {
    event.preventDefault();
    const delta = Math.sign(event.deltaY);
    if (delta < 0) {
      var dir = 'r';
    } else {
      var dir = 'l';
    }
    if (cntrlIsPressed) {
      // zoom
      Shiny.setInputValue('zoomWheel', dir + Math.random());
    } else {
      // scroll
      Shiny.setInputValue('scrollBarWheel', dir + Math.random());
    }
  });


  // Event listeners for hotkeys
  $(document).on("keydown", function (e) {
    let isInput = $(e.target).is('input, textarea, select, [contenteditable]');

  // Handle Modal Enter/Esc keys globally
  if ($('.modal-dialog').length > 0) {
     if (e.key === 'Enter') {
        // Force Shiny to sync the textInput value before clicking OK
        var activeEl = document.activeElement;
        if (activeEl && activeEl.tagName === 'INPUT') {
          $(activeEl).trigger('change');
        }
        // Small delay to ensure the textInput value reaches the server before the button click
        setTimeout(function() {
          if ($('#ok_new').length) $('#ok_new').click();
          if ($('#ok_edit').length) $('#ok_edit').click();
        }, 50);
        e.preventDefault();
        return;
      }

      if (e.key === 'Escape') {
        $('.modal').modal('hide');
        e.preventDefault();
        return;
      }
    }

    // Send hotkeys to Shiny (only if not typing in an input)
    if (!isInput) {
        Shiny.onInputChange("userPressedSmth", e.key + Math.random().toString(36).substr(2, 8));
        // w/o Math.random() only the first of a series of identical
        // keydown events is sent to server()
    }

    if (e.key == "Control") cntrlIsPressed = true;
  });


  $(document).keyup(function() {
    cntrlIsPressed = false;
    // prevent spacebar from activating the last pressed button
    if(event.key == " ") {
      event.preventDefault();
    }
  });

  document.querySelector('#ann_table').onclick = function(ev) {
    // ev.target <== td element
    // ev.target.parentElement <== tr
    // debugger;
    var tn = ev.target.constructor.name;
    if (tn == 'HTMLTableCellElement') {
      // click inside a table row
      var r = ev.target.parentElement.rowIndex;
      if (r > 0) {
        // not the header: send word to R
    		Shiny.setInputValue('tableRow', r);
      }
    }
  };

  // Notify R whenever any modal is hidden
  $(document).on('hidden.bs.modal', '.modal', function() {
    Shiny.setInputValue('modal_hidden', Math.random());
  });

});
