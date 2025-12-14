# formant_app()
#
# To do: IPA chart as another option in addition to Hillenbrand; synthetic sound - set pitch to mean f0 in annotated region - enable pitch tracking (but slow); deleting a formant in the button also deletes the annotation (bug); maybe an option to have log-spectrogram and log-spectrum (a bit tricky b/c all layers have to be adjusted); maybe remove the buggy feature of editing formant freq in the button as text, just display current value there (but then how to make it NA?); LPC saves all avail formants - check beh when changing nFormants across annotations & files; from-to in play sometimes weird (stops audio while cursor is still moving); highlight smts disappears in ann_table (buggy! tricky!); load audio upon session start; maybe arbitrary number of annotation tiers

# Start with a fresh R session and run the command options(shiny.reactlog=TRUE)
# Then run your app in a show case mode: runApp('inst/shiny/formant_app', display.mode = "showcase")
# At any time you can hit Ctrl+F3 (or for Mac users, Command+F3) in your web browser to launch the reactive log visualization.


# # tip: to read the output, do smth like:
# a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
# as.numeric(unlist(strsplit(a$pitch, ',')))

server = function(input, output, session) {
  # make overlaid plots resizable (js fix)
  shinyjs::js$inheritSize(parentDiv = 'specDiv')

  # set max upload file size to 30 MB
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)

  myPars = reactiveValues(
    print = FALSE,          # if TRUE, some functions print a meassage to the console when called
    debugQn = T,        # for debugging - click "?" to step into the code
    zoomFactor = 2,         # zoom buttons change time zoom by this factor
    zoomFactor_freq = 1.5,  # same for frequency
    drawSpec = TRUE,
    shinyTip_show = 1000,      # delay until showing a tip (ms)
    shinyTip_hide = 0,         # delay until hiding a tip (ms)
    initDur = 2000,            # initial duration to analyze (ms)
    spec_xlim = c(0, 2000),
    out_fTracks = list(),      # a list for storing formant tracks per file
    out_spects = list(),       # a list for storing spectrograms
    selectedF = 'F1',          # preselect F1 for correction
    slider_ms = 50,            # how often to update play slider
    scrollFactor = .75,        # how far to scroll on arrow press/click
    wheelScrollFactor = .1,    # how far to scroll on mouse wheel (prop of xlim)
    samplingRate_idx = 1,      # sampling rate scaling index for playback
    listen_alphanum = TRUE,    # enable/disable alphanumeric hotkeys
    listen_enter = FALSE,      # enable/disable ENTER to close modal (new annotation)
    listen_enter_edit = FALSE, # ENTER to edit an existing annotation
    cursor = 0,
    listenToFbtn = FALSE,      # buggy
    play = list(on = FALSE)
  )
  tooltip_options = list(delay = list(show = 1000, hide = 0))
  # NB: using myPars$play$cursor for some reason invalidates the observer,
  # so it keeps executing as fast as it can - no idea why!

  # clean-up of www/ folder: remove all files except temp.wav
  # if (!dir.exists("www")) dir.create("www")  # otherwise trouble with shinyapps.io
  if (file.exists('www/temp.csv')) {
    showModal(modalDialog(
      title = "Unsaved data",
      "Found unsaved data from a prevous session. Append to the new output?",
      easyClose = TRUE,
      footer = tagList(
        actionButton("discard", "Discard"),
        actionButton("append", "Append")
      )
    ))
  }
  observeEvent(input$discard, {
    file.remove('www/temp.csv')
    removeModal()
  })
  observeEvent(input$append, {
    myPars$out = try(read.csv('www/temp.csv', stringsAsFactors = FALSE))
    myPars$out = unique(myPars$out)  # remove duplicate rows
    removeModal()
  })

  files = list.files('www/', pattern = '.wav')
  for (f in files){
    file.remove(paste0('www/', f))
  }

  reset = function() {
    if (myPars$print) print('Resetting...')
    myPars$ann = NULL         # a dataframe of annotations for the current file
    myPars$currentAnn = NULL  # the idx of currently selected annotation
    myPars$bp = NULL          # selected points (under brush)
    myPars$spec = NULL
    myPars$spec_trimmed = NULL
    myPars$formantTracks = NULL
    myPars$formants = NULL
    myPars$analyzedUpTo = NULL
    myPars$selection = NULL
    myPars$cursor = 0
    myPars$spectrogram_brush = NULL
  }

  resetSliders = function() {
    if (myPars$print) print('Resetting sliders...')
    sliders_to_reset = c(
      names(input)[which(names(input) %in% names(formant_app_defaults))],
      'spectrum_xlim', 'spec_ylim', 'speedSound'
    )
    for (v in sliders_to_reset) {
      new_value = formant_app_defaults[[v]]
      suppressWarnings(try(updateSliderInput(session, v, value = new_value)))
      suppressWarnings(try(updateNumericInput(session, v, value = new_value)))
    }
    updateSelectInput(session, 'wn', selected = formant_app_defaults$wn)
    updateSelectInput(session, 'wn_lpc', selected = formant_app_defaults$wn_lpc)
    updateSliderInput(session, 'spec_ylim', value = formant_app_defaults$spec_ylim)
    updateSliderInput(session, 'spectrum_xlim',
                      value = formant_app_defaults$spectrum_xlim)
    updateRadioButtons(session, 'spec_colorTheme',
                       selected = formant_app_defaults$spec_colorTheme)
    updateSelectInput(session, 'osc', selected = formant_app_defaults$osc)
    updateSelectInput(session, 'vtl_method', selected = formant_app_defaults$vtl_method)
    updateSliderInput(session, 'speedSound', value = formant_app_defaults$speedSound)
    updateTextInput(session, 'coeffs', value = formant_app_defaults$coeffs)
    updateCheckboxInput(session, 'interceptZero', value = formant_app_defaults$interceptZero)
    updateCheckboxInput(session, 'tube', value = formant_app_defaults$tube)
  }
  observeEvent(input$reset_to_def, resetSliders(), ignoreNULL = FALSE)

  loadAudio = function() {
    # shinyjs::js$inheritSize(parentDiv = 'specDiv')
    if (myPars$print) print('Loading audio...')
    done()
    reset()  # also triggers done(), but done() needs to run first in case loadAudio
    # is re-executed (need to save myPars$ann --> myPars$out)

    # if there is a csv among the uploaded files, use the annotations in it
    ext = substr(input$loadAudio$name,
                 (nchar(input$loadAudio$name) - 2),
                 nchar(input$loadAudio$name))
    old_out_idx = which(ext == 'csv')[1]  # grab the first csv, if any
    if (!is.na(old_out_idx)) {
      user_ann = read.csv(input$loadAudio$datapath[old_out_idx], stringsAsFactors = FALSE)
      oblig_cols = c('file', 'from', 'to')
      if (nrow(user_ann) > 0 &
          !any(!oblig_cols %in% colnames(user_ann))) {
        idx_missing = which(apply(user_ann[, oblig_cols], 1, function(x) any(is.na(x))))
        if (length(idx_missing) > 0) user_ann = user_ann[-idx_missing, ]
        need_cols = c('label', 'dF', 'vtl', myPars$ff)
        missing_cols = need_cols[which(!need_cols %in% colnames(user_ann))]
        if (length(missing_cols) > 0) user_ann[, missing_cols] = NA
        user_ann = user_ann[, c(oblig_cols, need_cols)]
        if (nrow(user_ann) > 0) {
          if (is.null(myPars$out)) {
            myPars$out = user_ann
          } else {
            myPars$out = soundgen:::rbind_fill(myPars$out, user_ann)
            # remove duplicate rows
            myPars$out = unique(myPars$out)
          }
        }
      }
    }

    # work only with audio files
    idx_audio = which(apply(matrix(input$loadAudio$type), 1, function(x) {
      grepl('audio', x, fixed = TRUE)
    }))
    if (length(idx_audio) > 0) {
      if (is.null(myPars$fileList)) {
        myPars$fileList = input$loadAudio[idx_audio, ]
        myPars$n = 1   # file number in queue
      } else {
        sameFiles = which(myPars$fileList$name %in% input$loadAudio$name)
        if (length(sameFiles) > 0) {
          message('Note: uploading the same audio file twice overwrites previous annotations')
          if (!is.null(myPars$out)) {
            myPars$out = myPars$out[!myPars$out$file %in% myPars$fileList$name[sameFiles]]
            if (length(myPars$out) == 0) myPars$out = NULL
          }
          myPars$fileList = myPars$fileList[-sameFiles, ]
        }
        myPars$n = nrow(myPars$fileList) + 1
        myPars$fileList = rbind(myPars$fileList, input$loadAudio[idx_audio, ])
      }
      myPars$nFiles = nrow(myPars$fileList)  # number of uploaded files in queue
      myPars$drawSpec = FALSE  # hold on plotting the spectrogram until after running analyze()
      choices = as.list(myPars$fileList$name)
      names(choices) = myPars$fileList$name
      if (input$fileList == myPars$fileList$name[myPars$n])
        readAudio(myPars$n)  # doesn't fire automatically if the same as before
      updateSelectInput(session, 'fileList',
                        choices = as.list(myPars$fileList$name),
                        selected = myPars$fileList$name[myPars$n])
    } else if(!is.na(old_out_idx)) {
      # only a new csv uploaded - just refresh the current file
      readAudio(myPars$n)
    }
  }
  observeEvent(input$loadAudio, loadAudio())

  readAudio = function(i) {
    # reads an audio file with tuneR::readWave
    if (myPars$print) print('Reading audio...')
    temp = myPars$fileList[i, ]
    myPars$myAudio_filename = temp$name
    myPars$myAudio_path = temp$datapath
    myPars$myAudio_type = temp$type

    extension = substr(myPars$myAudio_filename,
                       nchar(myPars$myAudio_filename) - 2,
                       nchar(myPars$myAudio_filename))
    if (extension == 'wav' | extension == 'WAV') {
      myPars$temp_audio = tuneR::readWave(temp$datapath)
    } else if (extension == 'mp3' | extension == 'MP3') {
      myPars$temp_audio = tuneR::readMP3(temp$datapath)
    } else {
      warning('Input not recognized: must be a wav or mp3 file')
    }

    myPars$myAudio = as.numeric(myPars$temp_audio@left)
    myPars$ls = length(myPars$myAudio)
    myPars$samplingRate = myPars$temp_audio@samp.rate
    myPars$maxAmpl = 2 ^ (myPars$temp_audio@bit - 1)
    if (input$normalizeInput) {
      myPars$myAudio = myPars$myAudio / max(abs(myPars$myAudio)) * myPars$maxAmpl
    }
    myPars$nyquist = myPars$samplingRate / 2 / 1000
    # updateSliderInput(session, 'spec_ylim',
    #                   value = c(0, min(def_form['spec_ylim', 'default'], myPars$nyquist)),
    #                   max = myPars$nyquist)
    # updateSliderInput(session, 'spectrum_xlim',
    #                   value = c(0, min(def_form['spectrum_xlim', 'default'], myPars$nyquist)),
    #                   max = myPars$nyquist)
    myPars$dur = length(myPars$temp_audio@left) * 1000 / myPars$temp_audio@samp.rate
    myPars$time = seq(1, myPars$dur, length.out = myPars$ls)
    myPars$spec_xlim = c(0, min(myPars$initDur, myPars$dur))
    if (!is.finite(myPars$spec_xlim[2])) browser()  # weird glitches
    myPars$regionToAnalyze = myPars$spec_xlim
    myPars$myAudio_list = list(
      sound = myPars$myAudio,
      samplingRate = myPars$samplingRate,
      scale = myPars$maxAmpl,
      timeShift = 0,
      ls = length(myPars$myAudio),
      duration = myPars$dur / 1000
    )

    # shorten window and step if the input is very short
    max_win = round(myPars$dur / 2)
    if (input$windowLength > myPars$dur) {
      updateNumericInput(session, 'windowLength', value = max_win)
      updateNumericInput(session, 'step', value = max_win / 2)
    }
    if (input$windowLength_lpc > myPars$dur) {
      updateNumericInput(session, 'windowLength_lpc', value = max_win)
      updateNumericInput(session, 'step_lpc', value = max_win / 2)
    }

    # update info - file number ... out of ...
    updateSelectInput(session, 'fileList',
                      label = NULL,
                      selected = myPars$fileList$name[myPars$n])
    file_lab = paste0('File ', myPars$n, ' of ', myPars$nFiles)
    output$fileN = renderUI(HTML(file_lab))

    # if we've already worked with this file,
    # re-load the annotations and (if in current session) formant tracks + spec
    idx = which(myPars$out$file == myPars$myAudio_filename)
    if (length(idx) > 0) {
      myPars$ann = myPars$out[idx, ]
      myPars$currentAnn = 1
    } else {
      myPars$ann = NULL
    }
    if (!is.null(myPars$out_fTracks[[myPars$myAudio_filename]])) {
      myPars$formantTracks = myPars$out_fTracks[[myPars$myAudio_filename]]
      myPars$spec = myPars$out_specs[[myPars$myAudio_filename]]
      myPars$drawSpec = TRUE  # don't need to wait for analyze() to run
    }
    drawAnn()
    # drawAnnTbl()
  }

  extractSpectrogram = observe({
    # Instead of re-loading the file every time, could save the spectrogram
    # matrix and re-draw manually with soundgen:::filled.contour.mod
    if (!is.null(myPars$myAudio)) {  # & is.null(myPars$spec)
      if (myPars$print) print('Extracting spectrogram...')
      temp_spec = try(soundgen:::.spectrogram(
        myPars$myAudio_list,
        samplingRate = myPars$samplingRate,
        dynamicRange = input$dynamicRange,
        windowLength = input$windowLength,
        step = input$step,
        wn = input$wn,
        zp = 2 ^ input$zp,
        contrast = input$specContrast,
        brightness = input$specBrightness,
        blur = c(input$blur_freq, input$blur_time),
        specType = input$specType,
        output = 'all',
        plot = FALSE
      ))
      if (!inherits(temp_spec, 'try-error') &&
          length(temp_spec) > 0 &&
          is.matrix(temp_spec$processed)) {
        myPars$spec = temp_spec$processed
        myPars$reassigned = temp_spec$reassigned
      }
    }
  })

  writeAudioFile = observeEvent(c(myPars$temp_audio, myPars$samplingRate_idx), {
    if (myPars$print) print('Writing audio file...')
    # Method: saves a temporary audio file in 'www/'. This is a workaround since
    # html tag for some reason cannot play myPars$myAudio_path (although feeding
    # it to spectrogram works - so probably only works within R). Alternatives:
    # soundgen::play() or shinyFiles library

    # first remove the previous sound file to avoid cluttering up the www/ folder
    if (!is.null(myPars$myfile)){
      file.remove(paste0('www/', myPars$myfile))
    }
    randomID = paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = '')
    myPars$myfile = paste0(randomID, '.wav')
    # this is the new sound file. NB: has to be saved in www/ !!!
    seewave::savewav(myPars$temp_audio,
                     f = myPars$samplingRate * myPars$samplingRate_idx,
                     filename = paste0('www/', myPars$myfile),
                     extensible = FALSE)
    output$htmlAudio = renderUI(
      tags$audio(src = myPars$myfile, type = myPars$myAudio_type,
                 id = 'myAudio',
                 style = "display: none;")
    )
  }, ignoreInit = TRUE)
  observeEvent(input$samplingRate_mult, {
    myPars$samplingRate_idx = 2 ^ input$samplingRate_mult
  })


  # update the number of formant buttons
  output$fButtons = renderUI({
    c(
      list(actionButton(
        inputId = 'remF',
        label = tooltip(HTML("<img src='icons/minus.png' width = '25px'>"),
                        "Remove one formant"), # '-',
        class = "buttonInline"
      )),

      lapply(1:input$nFormants, function(f) {  # need to return a list, hence lapply
        tags$div(
          id = paste0('fDiv_', f),
          class = ifelse(f == 1, 'fBox selected', 'fBox'),
          textInput(
            inputId = paste0('F', f, '_text'),
            label = tooltip(paste0('F', f),
                            "Click to select, type a number to correct,
                            any text or NA to treat as missing, empty to reset to default"),
            value = '')
        )
      }),

      list(actionButton(
        inputId = 'addF',
        label = tooltip(HTML("<img src='icons/plus.png' width = '25px'>"),
                        "Add one formant"), # '+',
        class = "buttonInline"
      )),

      list(actionButton(
        inputId = 'defaultFmtBtn',
        label = tooltip(HTML("<img src='icons/update.png' width = '30px'>"),
                        "Reset formant values to defaults as measured"),
        class = "buttonInline"
      ))
    )
  })

  observeEvent(input$addF, {
    updateSliderInput(session, 'nFormants',
                      value = input$nFormants + 1)
  })

  observeEvent(input$remF, {
    updateSliderInput(session, 'nFormants',
                      value = max(1, input$nFormants - 1))
  })

  # edit myPars$ann when formant freq is modified manually as text
  observeEvent(input$nFormants, {
    lapply(1:input$nFormants, function(f) {
      fn = paste0('F', f, '_text')
      observeEvent(input[[fn]], {
        hr()  # otherwise row highlight disappears, no idea why!
        if (isolate(myPars$listenToFbtn)) {
          v = input[[fn]]
          vn = suppressWarnings(as.numeric(v))
          if (v == '') {
            # empty string - reset to default
            myPars$ann[myPars$currentAnn, paste0('F', f)] = myPars$formants[f]
            updateTextInput(
              session, fn,
              value = as.character(myPars$formants[f]))
          } else if (!is.na(vn)) {
            # number - treat as manual correction
            myPars$ann[myPars$currentAnn, paste0('F', f)] = vn
          } else {
            # any other (invalid) input - treat as a missing formant (NA)
            myPars$ann[myPars$currentAnn, paste0('F', f)] = NA
          }
          updateVTL()
          hr()
        }
      })

      # add onclick event with shinyjs to select the formant to edit
      this_div = paste0('fDiv_', f)
      shinyjs::onclick(id = this_div, {
        myPars$selectedF = paste0('F', f)
        shinyjs::addCssClass(id = this_div, class = 'selected')
        other_divs = paste0('fDiv_', (1:input$nFormants)[-f])
        for (d in other_divs) {
          shinyjs::removeCssClass(id = d, class = 'selected')
        }
      })
      # start listening to formant buttons when one is in focus
      shinyjs::onevent('focusin', id = fn, {
        myPars$listenToFbtn = TRUE
        # print('LISTENING')
      })
    })
    avFmPerSel()
  })
  # focusout fires when it shouldn't, so we stop listening if the mouse is over
  # the main panel or the spectrum instead (ie basically anywhere else)
  shinyjs::onevent('hover', id = 'left', {
    myPars$listenToFbtn = FALSE
    # print('STOP LISTENING')
  })
  shinyjs::onevent('hover', id = 'spectrum', {
    myPars$listenToFbtn = FALSE
    # print('STOP LISTENING')
  })

  # Updating spec / osc stuff to speed up plotting
  observe({
    if (!is.null(myPars$myAudio)) {
      # if (myPars$print) print('Scaling audio...')
      if (input$osc == 'dB') {
        myPars$myAudio_scaled = osc(
          myPars$myAudio,
          dynamicRange = input$dynamicRange,
          dB = TRUE,
          maxAmpl = myPars$maxAmpl,
          plot = FALSE,
          returnWave = TRUE)
        myPars$ylim_osc = c(-2 * input$dynamicRange, 0)
      } else {
        myPars$myAudio_scaled = myPars$myAudio
        myPars$ylim_osc = c(-myPars$maxAmpl, myPars$maxAmpl)
      }
    }
  })

  observe({
    # Cut just the part of spec currently needed for plotting
    # (faster than plotting a huge matrix with xlim/ylim)
    if (!is.null(myPars$spec) & !is.null(myPars$myAudio_scaled)) {
      if (myPars$print) print('Trimming the spec & osc')
      # spec
      x = as.numeric(colnames(myPars$spec))
      idx_x = which(x >= (myPars$spec_xlim[1] / 1.05) &
                      x <= (myPars$spec_xlim[2] * 1.05))
      # 1.05 - a bit beyond b/c we use xlim/ylim and may get white space
      y = as.numeric(rownames(myPars$spec))
      idx_y = which(y >= (input$spec_ylim[1] / 1.05) &
                      y <= (input$spec_ylim[2] * 1.05))
      myPars$spec_trimmed = downsample_spec(
        myPars$spec[idx_y, idx_x],
        maxPoints = 10 ^ input$spec_maxPoints)
      # dim(myPars$spec_trimmed)

      # osc
      idx_s = max(1, (myPars$spec_xlim[1] / 1.05 * myPars$samplingRate / 1000)) :
        min(myPars$ls, (myPars$spec_xlim[2] * 1.05 * myPars$samplingRate / 1000))
      downs_osc = 10 ^ input$osc_maxPoints

      isolate({
        myPars$myAudio_trimmed = myPars$myAudio_scaled[idx_s]
        myPars$time_trimmed = myPars$time[idx_s]
        myPars$ls_trimmed = length(myPars$myAudio_trimmed)
        if (!is.null(myPars$myAudio_trimmed) &&
            myPars$ls_trimmed > downs_osc) {
          myseq = round(seq(1, myPars$ls_trimmed,
                            length.out = downs_osc))
          myPars$myAudio_trimmed = myPars$myAudio_trimmed[myseq]
          myPars$time_trimmed = myPars$time_trimmed[myseq]
          myPars$ls_trimmed = length(myseq)
        }
      })
      myPars$drawSpec = TRUE
    }
  })

  downsample_sound = function(x, maxPoints) {
    if (!is.null(myPars$myAudio_trimmed) &&
        myPars$ls_trimmed > (10 ^ input$osc_maxPoints)) {
      if (myPars$print) print('Downsampling osc')
      myseq = round(seq(1, myPars$ls_trimmed,
                        by = myPars$ls_trimmed / input$osc_maxPoints))
      myPars$myAudio_trimmed = myPars$myAudio_trimmed[myseq]
      myPars$ls_trimmed = length(myseq)
    }
  }

  downsample_spec = function(x, maxPoints) {
    lxy = nrow(x) * ncol(x)
    if (length(lxy) > 0 && lxy > maxPoints) {
      if (myPars$print) print('Downsampling spectrogram...')
      lx = ncol(x)  # time
      ly = nrow(x)  # freq
      downs = sqrt(lxy / maxPoints)
      seqx = round(seq(1, lx, length.out = lx / downs))
      seqy = round(seq(1, ly, length.out = ly / downs))
      out = x[seqy, seqx]
    } else {
      out = x
    }
    return(out)
  }

  observeEvent(c(myPars$spec, myPars$spec_xlim, myPars$analyzedUpTo), {
    if (!is.null(myPars$spec)) {
      if (is.null(myPars$analyzedUpTo)) {
        myPars$regionToAnalyze = myPars$spec_xlim
        call = TRUE
      } else {
        if (myPars$analyzedUpTo < myPars$spec_xlim[2]) {
          myPars$regionToAnalyze = c(myPars$analyzedUpTo, myPars$spec_xlim[2])
          if (diff(myPars$regionToAnalyze) < 500) {
            # too little new audio to analyze - extend
            myPars$regionToAnalyze[2] = min(
              myPars$regionToAnalyze[2] + 500, myPars$dur
            )
            if (diff(myPars$regionToAnalyze) < 500) {
              myPars$regionToAnalyze[1] = max(
                myPars$regionToAnalyze[2] - 500, 0
              )
            }
          }
          call = TRUE
        } else {
          call = FALSE
        }
      }
      if (call) extractFormants()
    }
  })

  observeEvent(
    c(input$windowLength_lpc, input$step_lpc, input$wn_lpc, input$zp_lpc,
      input$dynamicRange_lpc, input$silence, input$minformant, input$maxbw), {
      extractFormants()
    })

  observe({
    # keep track of the maximum number of formant ever analyzed
    # to make sure the output table has enough columns
    myPars$maxF = max(myPars$maxF,
                      input$nFormants,
                      length(colnames(myPars$ann)) - 6)  # just the formant columns
  })

  observeEvent(input$nFormants, {
    myPars$ff = paste0('F', 1:input$nFormants)
    if (!is.null(myPars$formantTracks)) {
      missingCols = myPars$ff[which(!myPars$ff %in% colnames(myPars$formantTracks))]
      if (length(missingCols > 0)) myPars$formantTracks[, missingCols] = NA
    }
    if (!is.null(myPars$ann)) {
      missingCols = myPars$ff[which(!myPars$ff %in% colnames(myPars$ann))]
      myPars$ann[, missingCols] = NA
    }
  })



  #################
  ### P L O T S ###
  #################

  ## SPECTROGRAM
  output$spectrogram = renderPlot({
    if (myPars$drawSpec == TRUE) {
      if (myPars$print) print('Drawing spectrogram...')
      par(mar = c(0.2, 2, 0.5, 2))  # no need to save user's graphical par-s - revert to orig on exit
      if (is.null(myPars$spec)) {
        plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
        text(x = 5, y = 5, cex = 3,
             labels =
               'Upload wav/mp3 file(s) to begin...\n
             Suggested max duration\n
             about a few minutes')
      } else {
        if (input$specType != 'reassigned') {
          # rasterized spectrogram
          try(soundgen:::filled.contour.mod(
            x = as.numeric(colnames(myPars$spec_trimmed)),
            y = as.numeric(rownames(myPars$spec_trimmed)),
            z = t(myPars$spec_trimmed),
            col = soundgen:::switchColorTheme(input$spec_colorTheme)(input$nColors),
            yScale = 'linear',
            xlim = myPars$spec_xlim,
            xaxt = 'n',
            xlab = '',
            ylab = '',
            main = '',
            ylim = input$spec_ylim
          ))
        } else {
          # unrasterized reassigned spectrogram
          try(soundgen:::plotUnrasterized(
            myPars$reassigned,
            col = soundgen:::switchColorTheme(input$spec_colorTheme)(input$nColors),
            yScale = 'linear',
            xlim = myPars$spec_xlim,
            xaxt = 'n',
            xaxs = 'i', xlab = '',
            ylab = '',
            main = '',
            ylim = input$spec_ylim,
            cex = input$reass_cex
          ))
        }

        # Add text label of file name
        ran_x = myPars$spec_xlim[2] - myPars$spec_xlim[1]
        ran_y = input$spec_ylim[2] - input$spec_ylim[1]
        text(x = myPars$spec_xlim[1] + ran_x * .01,
             y = input$spec_ylim[2] - ran_y * .01,
             labels = myPars$myAudio_filename,
             adj = c(0, 1))  # left, top
      }
    }
  })

  observe({
    myPars$specOver_opts = list(
      xlim = myPars$spec_xlim, ylim = input$spec_ylim,
      xaxs = "i", yaxs = "i",
      bty = 'n', xaxt = 'n', yaxt = 'n',
      xlab = '', ylab = '')
  })

  output$specOver = renderPlot({
    if (!is.null(myPars$spec)) {
      par(mar = c(0.2, 2, 0.5, 2))
      # bg=NA makes the image transparent

      # empty plot to enable hover/click events for the spectrogram underneath
      do.call(plot, c(list(
        x = myPars$spec_xlim,
        y = input$spec_ylim,
        type = 'n'),
        myPars$specOver_opts))

      # Add a rectangle showing the current annotation
      if (!is.null(myPars$currentAnn)) {
        isolate({  # from myPars$ann
          rect(
            xleft = myPars$ann$from[myPars$currentAnn],
            xright = myPars$ann$to[myPars$currentAnn],
            ybottom = input$spec_ylim[1],
            ytop = input$spec_ylim[2],
            col = rgb(.2, .2, .2, alpha = .25),
            border = NA
          )
        })
      }

      # Add a rectangle showing the selected region
      if (!is.null(myPars$spectrogram_brush)) {
        rect(
          xleft = myPars$spectrogram_brush$xmin,
          xright = myPars$spectrogram_brush$xmax,
          ybottom = input$spec_ylim[1],
          ytop = input$spec_ylim[2],
          col = rgb(.2, .2, .2, alpha = .15),
          border = NA
        )
      }

      # Add formant tracks
      if (!is.null(myPars$formantTracks)) {
        for (f in 2:(length(myPars$ff) + 1)) {
          points(myPars$formantTracks$time,
                 myPars$formantTracks[, f] / 1000,
                 pch = 16,
                 col = input$spec_col,
                 cex = input$spec_cex)
        }
      }

      # Add formant frequencies from current ann
      if (!is.null(myPars$currentAnn)) {
        ff = myPars$ann[myPars$currentAnn, myPars$ff]
        text(x = rep(myPars$ann$from[myPars$currentAnn], length(ff)),
             y = ff / 1000,
             labels = names(ff),
             adj = c(0, .5), col = 'green', cex = 1.5)
      }

      # grid and x/y labels on hover
      if (!is.null(myPars$spectrogram_hover)) {
        # horizontal line
        do.call(points, c(list(
          x = myPars$spec_xlim,
          y = rep(myPars$spectrogram_hover$y, 2),
          type = 'l', lty = 3),
          myPars$specOver_opts))
        # frequency label
        do.call(text, list(
          x = myPars$spec_xlim[1],
          y = myPars$spectrogram_hover$y,
          labels = myPars$spectrogram_hover$freq,
          adj = c(0, 0)))
        # vertical line
        do.call(points, list(
          x = rep(myPars$spectrogram_hover$x, 2),
          y = input$spec_ylim,
          type = 'l', lty = 3))
        # time label
        do.call(text, list(
          x = myPars$spectrogram_hover$x,
          y = input$spec_ylim[1] + .025 * diff(input$spec_ylim),
          labels = soundgen:::convert_sec_to_hms(myPars$spectrogram_hover$x / 1000, 3),
          adj = .5))
      }

    }
  }, bg = 'transparent')

  output$specSlider = renderPlot({
    if (!is.null(myPars$spec)) {
      par(mar = c(0.2, 2, 0.5, 2))
      # bg=NA makes the image transparent

      if (myPars$cursor == 0) {
        # just a transparent plot
        do.call(plot, c(list(
          x = 1, type = 'n'),
          myPars$specOver_opts))
      } else {
        # horizontal line at current play time
        do.call(plot, c(list(
          x = rep(myPars$cursor, 2),
          y = input$spec_ylim,
          type = 'l'),
          myPars$specOver_opts))
      }
    }
  }, bg = 'transparent')

  observeEvent(input$spectrogram_hover, {
    if (!is.null(input$spectrogram_hover) & !is.null(myPars$spec)) {
      myPars$spectrogram_hover = input$spectrogram_hover
      cursor_hz = myPars$spectrogram_hover$y * 1000
      cursor_notes = soundgen::HzToNotes(cursor_hz)
      myPars$spectrogram_hover$freq = paste0(
        round(myPars$spectrogram_hover$y * 1000), ' Hz (',
        cursor_notes, ')')
    }
  })

  shinyjs::onevent('mouseout', id = 'specOver', {
    # NB: more flexible and less mafan than juggling with the observer of
    # input$spectrogram_hover
    myPars$spectrogram_hover = NULL
    shinyjs::js$clearBrush(s = '_brush')
  } )

  observeEvent(input$spectrogram_click, {
    myPars$cursor = input$spectrogram_click$x
    if (!is.null(myPars$currentAnn)) {
      inside_sel = (myPars$ann$from[myPars$currentAnn] < input$spectrogram_click$x) &
        (myPars$ann$to[myPars$currentAnn] > input$spectrogram_click$x)
      if (inside_sel) {
        # update both myPars$ann and the corresponding formant button
        myPars$ann[myPars$currentAnn, myPars$selectedF] = round(input$spectrogram_click$y * 1000)
        updateTextInput(
          session, paste0(myPars$selectedF, '_text'),
          value = as.character(myPars$ann[myPars$currentAnn, myPars$selectedF]))
        updateVTL()
      } else {
        myPars$spectrogram_brush = NULL
        shinyjs::js$clearBrush(s = '_brush')
      }
    } else {
      myPars$spectrogram_brush = NULL
      shinyjs::js$clearBrush(s = '_brush')
    }
  })

  observeEvent(input$spectrogram_dblclick, {
    if (!is.null(myPars$spectrogram_brush)) {
      showModal(dataModal_new())
    }
  })

  observeEvent(input$spectrogram_brush, {
    myPars$spectrogram_brush = input$spectrogram_brush
  })


  ## OSCILLOGRAM
  observe({
    output$oscillogram = renderPlot({
      if (!is.null(myPars$myAudio_trimmed)) {
        if (myPars$print) print('Drawing osc...')
        par(mar = c(2, 2, 0, 2))
        plot(myPars$time_trimmed,
             myPars$myAudio_trimmed,
             type = 'l',
             xlim = myPars$spec_xlim,
             ylim = myPars$ylim_osc,
             axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
             xlab = 'Time, ms',
             ylab = '')
        box()
        # axis(side = 1)
        time_location = axTicks(1)
        time_labels = soundgen:::convert_sec_to_hms(time_location / 1000, 3)
        axis(side = 1, at = time_location, labels = time_labels)
        if (input$osc == 'dB') {
          axis(side = 4, at = seq(0, input$dynamicRange, by = 10))
          mtext("dB", side = 2, line = 3)
        }
        abline(h = 0, lty = 2)
      }
    }, execOnResize = TRUE)
  })


  ## SPECTRUM
  output$spectrum = renderPlot({
    if (!is.null(myPars$spectrum)) {
      if (myPars$print) print('Drawing spectrum...')
      par(mar = c(2, 2, 0.5, 0))
      xlim = input$spectrum_xlim # xlim = c(0, myPars$spectrum_freq_range[2])
      ylim = c(
        myPars$spectrum_ampl_range[1],
        # leave extra room for formant labels
        myPars$spectrum_ampl_range[2] + 5
      )
      plot(myPars$spectrum$freq,
           myPars$spectrum$ampl,
           xlab = '', ylab = '',
           xlim = xlim,
           ylim = ylim,
           xaxs = "i",
           type = 'l')
      # # fill in the AUC if needed
      # polygon(c(myPars$spectrum$freq, xlim[2], xlim[1]),
      #         c(myPars$spectrum$ampl, ylim[1], ylim[1]),
      #         border = NA,
      #         col = rgb(.5, .5, .5, .15))
      spectrum_peaks()

      # add a vertical line and freq label on hover
      if (!is.null(myPars$spectrum_hover)) {
        abline(v = myPars$spectrum_hover$x, lty = 2)
        text(x = myPars$spectrum_hover$x,
             y = myPars$spectrum_ampl_range[1],
             labels = myPars$spectrum_hover$cursor,
             adj = c(0, 0))
        # show the nearest spectral peak
        text(x = myPars$spectrum_hover$pf,
             y = myPars$spectrum_hover$pa,
             labels = myPars$spectrum_hover$peak,
             adj = c(0, 0))
        points(x = myPars$spectrum_hover$pf,
               y = myPars$spectrum_hover$pa, pch = 16)
      }

      # plot formant frequencies, if any
      if (!is.null(myPars$ann) &&
          any(is.numeric(myPars$currentAnn))) {
        ff = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
      } else if (!is.null(myPars$formants) &&
                 any(!is.na(as.numeric(myPars$formants)))) {
        ff = as.numeric(myPars$formants)
      } else {
        ff = numeric(0)
      }

      if (length(ff) > 0) {
        for (i in seq_along(ff)) {
          f = ff[i] / 1000
          if (is.numeric(f) & any(!is.na(f))) {
            idx_f = which.min(abs(myPars$spectrum$freq - f))
            text(
              x = f,
              y = myPars$spectrum$ampl[idx_f] + 2,  # +2 to avoid overlap with cross
              labels = paste0('F', i),
              adj = c(.5, 0)
            )
            segments(
              x0 = f, y0 = ylim[1] - .04 * diff(ylim),
              x1 = f, y1 = myPars$spectrum$ampl[idx_f],
              lty = 3, col = 'lightgreen'
            )
          }
        }
      }

      # plot the spectrum of the synthetic sound
      if (input$spectrum_plotSynth && !is.null(myPars$spectrum_synth)) {
        points(
          myPars$spectrum_synth$freq,
          myPars$spectrum_synth$ampl,
          type = 'l',
          lty = 2,
          col = 'blue')
      }
    }
  })

  observe({
    if (!is.null(myPars$spec_trimmed)) {
      if (myPars$print) print('Extracting spectrum of selection...')
      if (!is.null(myPars$selection) && length(myPars$selection) > 0) {
        # take the spectrum of selection (annotated region) from raw audio
        myPars$spectrum = try(as.list(soundgen:::getSmoothSpectrum(
          sound = myPars$selection,
          samplingRate = myPars$samplingRate,
          len = input$spectrum_len * (myPars$samplingRate / 1000 / 2) / diff(input$spec_ylim),
          loessSpan = 10 ^ input$spectrum_smooth
        )))
        # note: len is corrected to ensure constant resolution no matter how
        # much we zoom in on a particular frequency region
        # if (inherits(myPars$spectrum, 'try-error')) browser()
        # myPars$spectrum = list(
        #     freq = as.numeric(rownames(myPars$spec)),
        #     ampl = rowMeans(myPars$spec_trimmed)
        # )
      } else {
        # just use the visible part of the spectrogram, but all freq bins
        # (so not the same as myPars$spec_trimmed)
        time_stamps = as.numeric(colnames(myPars$spec))
        idx = which(time_stamps >= myPars$spec_xlim[1] &
                      time_stamps <= myPars$spec_xlim[2])
        if (length(idx) > 1) {
          ampl = as.numeric(rowMeans(myPars$spec[, idx, drop = FALSE]))
          ampl[ampl == 0] = 1e-16  # avoid -Inf in log-spectrum
          spec_temp = list(
            freq = as.numeric(rownames(myPars$spec[, idx])),
            ampl = 20 * log10(ampl)
          )
          myPars$spectrum = try(as.list(soundgen:::getSmoothSpectrum(
            spectrum = spec_temp,
            len = input$spectrum_len * (myPars$samplingRate / 1000 / 2) / diff(input$spec_ylim),
            loessSpan = 10 ^ input$spectrum_smooth
          )))
        }
      }
      isolate({
        # myPars$spectrum_freq_range = try(range(myPars$spectrum$freq))
        idx = try(which(myPars$spectrum$freq < input$spec_ylim[2]))
        if (!inherits(idx, 'try-error')) {
          myPars$spectrum_ampl_range = range(myPars$spectrum$ampl[idx])
        }
      })
    }
  })

  spectrum_peaks = reactive({
    # find peaks in the spectrum
    if (!is.null(myPars$spectrum)) {
      if (myPars$print) print('Looking for spectral peaks...')
      sp_zoo = zoo::as.zoo(myPars$spectrum$ampl)
      temp = zoo::rollapply(sp_zoo,
                            width = 3,
                            align = 'center',
                            function(x) which.max(x) == 2)
      myPars$spectrum_peaks = zoo::index(temp)[zoo::coredata(temp)]
    }
  })

  observeEvent(input$spectrum_click, {
    # update both myPars$ann and the corresponding formant button
    if (!is.null(myPars$currentAnn)) {
      myPars$ann[myPars$currentAnn, myPars$selectedF] = round(input$spectrum_click$x * 1000)
      updateTextInput(
        session, paste0(myPars$selectedF, '_text'),
        value = as.character(myPars$ann[myPars$currentAnn, myPars$selectedF]))
      updateVTL()
    }
  })

  observeEvent(input$spectrum_dblclick, {
    # update both myPars$ann and the corresponding formant button
    if (!is.null(myPars$currentAnn)) {
      pf_idx = which.min(abs(input$spectrum_dblclick$x -
                               myPars$spectrum$freq[myPars$spectrum_peaks]))
      pf = myPars$spectrum$freq[myPars$spectrum_peaks[pf_idx]]
      myPars$ann[myPars$currentAnn, myPars$selectedF] = round(pf * 1000)
      updateTextInput(
        session, paste0(myPars$selectedF, '_text'),
        value = as.character(myPars$ann[myPars$currentAnn, myPars$selectedF]))
      updateVTL()
    }
  })

  observeEvent(input$spectrum_hover, {
    if (!is.null(myPars$spectrum) & !is.null(input$spectrum_hover)) {
      myPars$spectrum_hover = data.frame(x = input$spectrum_hover$x,
                                         y = input$spectrum_hover$y)
      cursor_hz = round(input$spectrum_hover$x * 1000)
      cursor_notes = soundgen::HzToNotes(cursor_hz)
      myPars$spectrum_hover$cursor = paste0(cursor_hz, 'Hz (', cursor_notes, ')')

      pf_idx = which.min(abs(myPars$spectrum_hover$x -
                               myPars$spectrum$freq[myPars$spectrum_peaks]))
      myPars$spectrum_hover$pa =myPars$spectrum$ampl[myPars$spectrum_peaks[pf_idx]]
      myPars$spectrum_hover$pf = myPars$spectrum$freq[myPars$spectrum_peaks[pf_idx]]
      nearest_peak_hz = round(myPars$spectrum_hover$pf * 1000)
      nearest_peak_notes = soundgen::HzToNotes(nearest_peak_hz)
      myPars$spectrum_hover$peak = paste0(nearest_peak_hz, ' Hz (',
                                          nearest_peak_notes, ')')
    }
  })
  observeEvent(input$spectrum_smooth, {
    myPars$spectrum_hover = NULL
  })


  ## FORMANT SPACE
  output$fmtSpace = renderPlot({
    if (!is.null(myPars$currentAnn) &&
        !is.null(myPars$ann[myPars$currentAnn, ]) &&
        (is.finite(myPars$ann[myPars$currentAnn, ]$F1) &
         is.finite(myPars$ann[myPars$currentAnn, ]$F2))) {
      if (myPars$print) print('Drawing formant space')
      caf = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
      if (input$fmtSpacePlot == 'vowelSpace') {
        cafr = schwa(formants = caf)$ff_relative_dF
        xlim = range(c(hillenbrand$F1Rel, cafr[1]))
        ylim = range(c(hillenbrand$F2Rel, cafr[2]))
        par(mar = c(0, 0, 0, 0))
        plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n', xlab = '', ylab = '',
             xlim = xlim, ylim = ylim, bty = 'n', xaxt = 'n', yaxt = 'n')
        text(hillenbrand$F1Rel, hillenbrand$F2Rel,
             labels = hillenbrand$vowel, cex = 1.5, col = 'blue')
        points(cafr[1], cafr[2], pch = 4, cex = 2.5, col = 'red')
      } else if (input$fmtSpacePlot == 'regression') {
        if (input$vtl_method == 'regression') {
          soundgen:::getFormantDispersion(
            formants = caf,
            method = 'regression',
            interceptZero = input$interceptZero,
            tube = input$tube,
            speedSound = input$speedSound,
            plot = TRUE
          )
        } else {
          plot(1:10, type='n', bty='n', xlab='', ylab='', xaxt='n', yaxt='n')
          text(5, 9, label = 'Only VTL estimation with regression can be plotted here')
        }

      }
    } else {
      par(mar = c(0, 0, 0, 0))
      plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n', xlab = '', ylab = '',
           bty = 'n', xaxt = 'n', yaxt = 'n')
      text(hillenbrand$F1Rel, hillenbrand$F2Rel,
           labels = hillenbrand$vowel, cex = 1.5, col = 'blue')
    }
  })


  ## ANNOTATIONS
  drawAnn = function() {
    output$ann_plot = renderPlot({
      if (myPars$print) print('Drawing annotations...')
      isolate({
        if (!is.null(myPars$ann)) {
          if (nrow(myPars$ann) > 0) {
            par(mar = c(0, 2, 0, 2))
            plot(myPars$time_trimmed,
                 xlim = myPars$spec_xlim,
                 ylim = c(.2, .8),
                 type = 'n',
                 xaxs = "i", yaxs = "i",
                 bty = 'n',
                 axes = FALSE,
                 xlab = '', ylab = ''
            )
            for (i in 1:nrow(myPars$ann)) {
              r = rnorm(1, 0, .05)  # random vertical shift to avoid overlap
              # highlight current annotation
              highlight = ifelse(is.numeric(myPars$currentAnn) &&
                                   i == myPars$currentAnn,
                                 TRUE, FALSE)
              segments(x0 = myPars$ann$from[i],
                       x1 = myPars$ann$to[i],
                       y0 = .5 + r, y1 = .5 + r,
                       lwd = ifelse(highlight, 3, 2),
                       col = ifelse(highlight, 'blue', 'black'))
              segments(x0 = myPars$ann$from[i],
                       x1 = myPars$ann$from[i],
                       y0 = .45 + r, y1 = .55 + r,
                       lwd = ifelse(highlight, 3, 2),
                       col = ifelse(highlight, 'blue', 'black'))
              segments(x0 = myPars$ann$to[i],
                       x1 = myPars$ann$to[i],
                       y0 = .45 + r, y1 = .55 + r,
                       lwd = ifelse(highlight, 3, 2),
                       col = ifelse(highlight, 'blue', 'black'))
              middle_i = mean(as.numeric(myPars$ann[i, c('from', 'to')]))
              text(x = middle_i,
                   y = .5 + r,
                   labels = myPars$ann$label[i],
                   adj = c(.5, 0), cex = 1.5)
            }
            par(mar = c(0.2, 2, 0.5, 2))
          }
        } else if (!is.null(myPars$spec)) {
          par(mar = c(0, 2, 0, 2))
          plot(1:10,
               type = 'n',
               bty = 'n',
               axes = FALSE,
               xlab = '', ylab = '')
          text(5, 5,
               labels = paste('Select a region of spectrogram and double-click',
                              'to create an annotation'))
        }
      })
    })
  }
  observeEvent(c(myPars$spec_xlim, myPars$currentAnn), drawAnn())

  observeEvent(myPars$currentAnn, {
    if (!is.null(myPars$currentAnn)) {
      if (myPars$print) print('Updating selection...')
      sel_points = as.numeric(round(myPars$ann[myPars$currentAnn, c('from', 'to')] /
                                      1000 * myPars$samplingRate))
      # in case of weird times in annotations, keep selection between 0 and audio length
      sel_points[1] = max(0, sel_points[1])
      sel_points[2] = min(sel_points[2], myPars$ls)
      idx_points = sel_points[1]:sel_points[2]
      myPars$selection = myPars$myAudio[idx_points]
      # move the spec view to show the selected ann
      ann_dur = myPars$ann$to[myPars$currentAnn] -
        myPars$ann$from[myPars$currentAnn]
      mid_view = mean(myPars$spec_xlim)
      mid_ann = mean(as.numeric(myPars$ann[myPars$currentAnn, c('from', 'to')]))
      shift = mid_ann - mid_view
      if (myPars$ann$from[myPars$currentAnn] < myPars$spec_xlim[1] |
          myPars$ann$to[myPars$currentAnn] > myPars$spec_xlim[2]) {
        if (diff(myPars$spec_xlim) > ann_dur) {
          # the ann fits based on current zoom level
          myPars$spec_xlim[1] = max(0, myPars$spec_xlim[1] + shift)
          myPars$spec_xlim[2] = min(myPars$dur, myPars$spec_xlim[2] + shift)
        } else {
          # zoom out enough to show the whole ann
          half_span = ann_dur * 1.5 / 2
          myPars$spec_xlim[1] = max(0, mid_ann - half_span)
          myPars$spec_xlim[2] = min(myPars$dur, mid_ann + half_span)
        }
      }
      hr()
    }
  })

  observeEvent(input$ann_click, {
    # select the annotation whose middle (label) is closest to the click
    if (!is.null(myPars$ann)) {
      ds = abs(input$ann_click$x - (myPars$ann$from + myPars$ann$to) / 2)
      myPars$currentAnn = which.min(ds)
      myPars$spectrogram_brush = list(xmin = myPars$ann$from[myPars$currentAnn],
                                      xmax = myPars$ann$to[myPars$currentAnn])
      myPars$cursor = myPars$ann$from[myPars$currentAnn]
      avFmPerSel()
    }
  })

  observeEvent(input$ann_dblclick, {
    # select and edit the double-clicked annotation
    if (!is.null(myPars$ann)) {
      ds = abs(input$ann_dblclick$x - (myPars$ann$from + myPars$ann$to) / 2)
      myPars$currentAnn = which.min(ds)
      showModal(dataModal_edit())
    }
  })

  dataModal_new = function() {
    myPars$listen_alphanum = FALSE
    myPars$listen_enter = TRUE
    modalDialog(
      textInput("annotation", "New annotation:",
                placeholder = '...some info...'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_new", "OK")
      ),
      easyClose = TRUE
    )
  }

  new_annotation = function() {
    if (myPars$print) print('Creating a new annotation...')
    new = data.frame(
      # idx = ifelse(is.null(myPars$ann), 1, nrow(myPars$ann) + 1),
      file = myPars$myAudio_filename,
      from = round(myPars$spectrogram_brush$xmin),
      to = round(myPars$spectrogram_brush$xmax),
      label = input$annotation,
      dF = NA, vtl = NA,
      stringsAsFactors = FALSE)
    new[, myPars$ff] = NA

    # depending on the history of changing input$nFormants / output.csv,
    # there may be more formants in myPars$ann than in the current sel
    if (is.null(myPars$ann)) {
      myPars$ann = new
    } else {
      myPars$ann = soundgen:::rbind_fill(myPars$ann, new)
    }

    # reorder and select the newly added annotation
    ord = order(myPars$ann$from)
    myPars$ann = myPars$ann[ord, ]
    myPars$currentAnn = which(ord == nrow(myPars$ann))

    # paste in formant frequencies
    avFmPerSel()
    myPars$ann[myPars$currentAnn, myPars$ff] = myPars$formants[myPars$ff]
    updateFBtn(myPars$formants[myPars$ff])
    updateVTL()

    # clear the selection, close the modal
    removeModal()
    myPars$listen_alphanum = TRUE
    myPars$listen_enter = FALSE
    drawAnn()
    avFmPerSel()
    # hr()

    # save a backup in case the app crashes before done() fires
    temp = soundgen:::rbind_fill(myPars$out, myPars$ann)
    temp = temp[order(temp$file), ]
    my_formants <<- temp
    write.csv(temp, 'www/temp.csv', row.names = FALSE)
  }

  observeEvent(input$ok_new, {
    new_annotation()
  })

  updateFBtn = function(ff) {
    if (myPars$print) print('Updating formant buttons...')
    for (f in 1:input$nFormants) {
      updateTextInput(session, inputId = paste0('F', f, '_text'),
                      value = ff[f])
    }
  }

  dataModal_edit = function() {
    myPars$listen_alphanum = FALSE
    myPars$listen_enter_edit = TRUE
    modalDialog(
      textInput("annotation", "New annotation:",
                placeholder = '...some info...'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_edit", "OK")
      ),
      easyClose = TRUE
    )
  }

  edit_annotation = function() {
    myPars$ann$label[myPars$currentAnn] = input$annotation
    removeModal()
    myPars$listen_alphanum = TRUE
    myPars$listen_enter_edit = FALSE
    drawAnn()
    # drawAnnTbl()
  }
  observeEvent(input$ok_edit, {
    edit_annotation()
  })



  ### ANALYZE
  extractFormants = function() {
    if (!is.null(myPars$myAudio) & !is.null(myPars$regionToAnalyze)) {
      if (myPars$print) print('Extracting formants...')
      if (input$coeffs != '') {
        coeffs = as.numeric(input$coeffs)
      } else {
        coeffs = NULL
      }
      sel_anal = max(1, round(myPars$regionToAnalyze[1] / 1000 * myPars$samplingRate)) :
        min(myPars$ls, round(myPars$regionToAnalyze[2] / 1000 * myPars$samplingRate))
      myPars$myAudio_list_sel = list(
        sound = myPars$myAudio[sel_anal],
        samplingRate = myPars$samplingRate,
        scale = myPars$maxAmpl,
        timeShift = myPars$regionToAnalyze[1]/1000,
        ls = length(myPars$myAudio),
        duration = myPars$dur / 1000
      )

      # temp_anal = try(soundgen:::.analyze(
      #   myPars$myAudio_list,
      #   windowLength = input$windowLength_lpc,
      #   step = input$step_lpc,
      #   wn = input$wn_lpc,
      #   zp = input$zp_lpc,
      #   dynamicRange = input$dynamicRange_lpc,
      #   silence = input$silence,
      #   formants = list(
      #     coeffs = coeffs,
      #     minformant = input$minformant,
      #     maxbw = input$maxbw
      #   ),
      #   nFormants = NULL,  # save all available formants
      #   pitchMethods = NULL,  # disable pitch tracking
      #   roughness = NULL,  # no roughness analysis
      #   novelty = NULL,    # no ssm
      #   loudness = NULL,   # no loudness analysis
      #   amRange = NULL,  # no AM analysis
      #   summaryFun = NULL,
      #   plot = FALSE,
      #   returnPitchCands = FALSE
      # ))

      temp_anal = try(soundgen:::getFormants(
        audio = myPars$myAudio_list_sel,
        windowLength = input$windowLength_lpc,
        step = input$step_lpc,
        wn = input$wn_lpc,
        zp = input$zp_lpc,
        dynamicRange = input$dynamicRange_lpc,
        silence = input$silence,
        formants = list(
          coeffs = coeffs,
          minformant = input$minformant,
          maxbw = input$maxbw
        ),
        nFormants = NULL  # save all available formants
      ))

      if (!inherits(temp_anal, 'try-error') &&
          length(temp_anal) > 0 &&
          is.list(temp_anal)) {
        myPars$temp_anal = temp_anal
        myPars$nMeasuredFmts = length(grep('_freq', colnames(myPars$temp_anal)))
        myPars$allF_colnames = paste0('f', 1:myPars$nMeasuredFmts, '_freq')
        myPars$ff = myPars$ff[1:min(input$nFormants, myPars$nMeasuredFmts)]
        myPars$temp_anal = myPars$temp_anal[, c('time', myPars$allF_colnames)]
        colnames(myPars$temp_anal) = c('time', paste0('F', 1:myPars$nMeasuredFmts))
        for (c in colnames(myPars$temp_anal)) {
          if (any(!is.na(myPars$temp_anal[, c]))) {
            # in case of all NAs
            myPars$temp_anal[, c] = round(myPars$temp_anal[, c])
          }
        }
        isolate({
          myPars$analyzedUpTo = myPars$regionToAnalyze[2]
          if (is.null(myPars$formantTracks)) {
            myPars$formantTracks = myPars$temp_anal
          } else {
            new_time_range = range(myPars$temp_anal$time)
            idx = which(myPars$formantTracks$time >= new_time_range[1] &
                          myPars$formantTracks$time <= new_time_range[2])
            if (length(idx) > 0) {
              myPars$formantTracks = myPars$formantTracks[-idx, ]
            }
            myPars$formantTracks = soundgen:::rbind_fill(myPars$formantTracks, myPars$temp_anal)
            # myPars$formantTracks = myPars$formantTracks[order(myPars$formantTracks$time), ]
          }
        })
      }
    }
  }

  # if any of LPC settings change, we re-analyze the entire file
  observeEvent(
    c(input$nFormants, input$silence, input$coeffs, input$minformant,
      input$maxbw, input$windowLength_lpc, input$step_lpc,
      input$wn_lpc, input$zp_lpc, input$dynamicRange_lpc), {
        myPars$analyzedUpTo = 0
      },
    ignoreInit = TRUE
  )

  # show the default number of LPC coefficients (grayed out)
  observeEvent(myPars$samplingRate, {
    updateTextInput(
      session, 'coeffs',
      placeholder = as.character(round(myPars$samplingRate/1000) + 3))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  avFmPerSel = function() {
    # analyze annotated selection
    if (!is.null(myPars$currentAnn) & !is.null(myPars$formantTracks)) {
      if (myPars$print) print('Averaging formants in selection...')
      isolate({
        # don't want dependency on myPars$ann
        idx = which(
          myPars$formantTracks$time >= myPars$ann$from[myPars$currentAnn] &
            myPars$formantTracks$time <= myPars$ann$to[myPars$currentAnn]
        )
      })
      fMat = myPars$formantTracks[idx, 2:(length(myPars$ff) + 1)]
      myPars$formants = apply(fMat, 2, function(x)
        round(do.call(input$summaryFun, list(x, na.rm = TRUE))))
      # myPars$bandwidth ?

      if (FALSE) {
        # fill in NAs
        idx_f_na = which(is.na(myPars$ann[myPars$currentAnn, myPars$ff]))
        if (length(idx_f_na) > 0)
          myPars$ann[myPars$currentAnn, myPars$ff[idx_f_na]] = myPars$formants[idx_f_na]
      }

      # fill in the formant boxes - note that we use the (possibly
      # user-modified) myPars$ann instead of myPars$formants
      updateFBtn(as.character(myPars$ann[myPars$currentAnn, myPars$ff]))
    }
  }
  observeEvent(myPars$formantTracks, avFmPerSel())

  observeEvent(input$defaultFmtBtn, {
    # browser()
    for (f in 1:input$nFormants) {
      fn = paste0('F', f, '_text')
      updateTextInput(
        session, fn,
        value = as.character(myPars$formants[f]))
    }
    myPars$ann[myPars$currentAnn, myPars$ff] = myPars$formants[myPars$ff]
    updateVTL()
  })

  observeEvent(myPars$ann, {
    if (myPars$print) print('Drawing ann_table...')
    if (!is.null(myPars$ann)) {
      # ann_for_print = myPars$ann[, which(!colnames(myPars$ann) %in% c('X', 'file'))]
      show_cols = c('from', 'to', 'label', 'dF', 'vtl', paste0('F', 1:ncol(myPars$ann)))
      ann_for_print = myPars$ann[, show_cols[which(show_cols %in% colnames(myPars$ann))]]
    } else {
      ann_for_print = '...waiting for some annotations...'
    }
    output$ann_table = renderTable(
      format(ann_for_print),
      align = 'c', striped = FALSE,
      bordered = TRUE, hover = FALSE, width = '100%'
    )
    hr()
  }, ignoreNULL = FALSE)

  hr = function() {
    if (!is.null(myPars$currentAnn)) {
      # Sys.sleep(.5)
      session$sendCustomMessage('highlightRow', myPars$currentAnn)
    }
  }

  updateVTL = function(rows = myPars$currentAnn) {
    if (!is.null(rows) && length(rows) > 0 && !is.null(myPars$ann[rows, ])) {
      if (myPars$print) print('Updating VTL...')
      for (i in rows) {
        if (!is.null(myPars$ann[i, ]) &&
            any(!is.na(myPars$ann[i, myPars$ff]))) {
          fmts_ann = as.numeric(myPars$ann[i, myPars$ff])
          vtl_ann = estimateVTL(
            formants = fmts_ann,
            method = input$vtl_method,
            speedSound = input$speedSound,
            interceptZero = input$interceptZero,
            tube = input$tube,
            output = 'detailed'
          )
          try({
            vtl_ann$formantDispersion = round(vtl_ann$formantDispersion)
          }, silent = TRUE)
          try({
            vtl_ann$vocalTract = round(vtl_ann$vocalTract, 2)
          }, silent = TRUE)
          myPars$ann$dF[i] = vtl_ann$formantDispersion
          myPars$ann$vtl[i] = vtl_ann$vocalTract
        }
      }
    }
  }
  observeEvent(c(input$vtl_method, input$speedSound, input$interceptZero, input$tube), {
    if (!is.null(myPars$ann) && nrow(myPars$ann) > 0) {
      updateVTL(rows = 1:nrow(myPars$ann))
    }
  })

  observeEvent(input$tableRow, {
    if (!is.null(myPars$ann) && input$tableRow > 0) {
      myPars$currentAnn = input$tableRow
      myPars$spectrogram_brush = list(xmin = myPars$ann$from[myPars$currentAnn],
                                      xmax = myPars$ann$to[myPars$currentAnn])
      avFmPerSel()
    }
  }, ignoreInit = TRUE)


  ## Buttons for operations with selection
  startPlay = function() {
    if (!is.null(myPars$myAudio)) {
      if (!is.null(myPars$spectrogram_brush) &&
          (myPars$spectrogram_brush$xmax - myPars$spectrogram_brush$xmin > 100)) {
        # at least 100 ms selected
        myPars$play$from = myPars$spectrogram_brush$xmin / 1000
        myPars$play$to = myPars$spectrogram_brush$xmax / 1000
      } else {
        myPars$play$from = myPars$cursor / 1000 # myPars$spec_xlim[1] / 1000
        myPars$play$to = myPars$spec_xlim[2] / 1000
      }
      myPars$play$dur = myPars$play$to - myPars$play$from
      myPars$play$timeOn = proc.time()
      myPars$play$timeOff = myPars$play$timeOn + myPars$play$dur / myPars$samplingRate_idx
      myPars$cursor_temp = myPars$cursor
      myPars$play$on = TRUE
      if (myPars$print) print('Playing selection...')

      # play selection
      if (input$audioMethod == 'Browser') {
        # play with javascript
        shinyjs::js$playme_js(  # need an external js script for this
          audio_id = 'myAudio',  # defined in tags$audio
          from = myPars$play$from / myPars$samplingRate_idx,
          to = myPars$play$to / myPars$samplingRate_idx)
      } else {
        # or play with R:
        playme(myPars$myAudio,
               samplingRate = myPars$samplingRate * myPars$samplingRate_idx,
               from = myPars$play$from / myPars$samplingRate_idx,
               to = myPars$play$to / myPars$samplingRate_idx)
      }
    }
  }
  observeEvent(c(input$selection_play), startPlay())  # add myPars$myAudio for autoplay

  stopPlay = function() {
    myPars$play$on = FALSE
    myPars$cursor = myPars$cursor_temp
    shinyjs::js$stopAudio_js(audio_id = 'myAudio')
  }
  observeEvent(input$selection_stop, stopPlay())

  observe({
    if (!is.null(myPars$play$on) && myPars$play$on) {
      time = proc.time()
      if (!is.null(myPars$slider_ms)) invalidateLater(myPars$slider_ms)
      if ((time - myPars$play$timeOff)[3] > 0) {
        myPars$play$on = FALSE
        myPars$cursor = myPars$cursor_temp  # reset to original cursor
      } else {
        myPars$cursor = myPars$play$from * 1000 + as.numeric(time - myPars$play$timeOn)[3] * 1000 * myPars$samplingRate_idx
        # [3] for "elapsed", ie "real" time
      }
    }
  })

  deleteSel = function() {
    if (!is.null(myPars$currentAnn)) {
      myPars$ann = myPars$ann[-myPars$currentAnn, ]
      myPars$selection = NULL
      myPars$currentAnn = NULL
      myPars$spectrum_synth = NULL
      drawAnn()
      # drawAnnTbl()
    }
  }
  observeEvent(input$selection_delete, deleteSel())

  observeEvent(input$selection_annotate, {
    if (!is.null(myPars$spectrogram_brush)) {
      showModal(dataModal_new())
    }
  })

  # HOTKEYS
  observeEvent(input$userPressedSmth, {
    button_key = substr(input$userPressedSmth, 1, nchar(input$userPressedSmth) - 8)
    # see https://keycode.info/
    if (button_key == ' ') {                  # SPACEBAR (play / stop)
      if (myPars$play$on) stopPlay() else startPlay()
    } else if (button_key %in% c('Delete', 'Backspace')) {    # DELETE (delete current annotation)
      deleteSel()
    } else if (button_key == 'ArrowLeft') {    # ARROW LEFT (scroll left)
      shiftFrame('left', step = myPars$scrollFactor)
    } else if (button_key == 'ArrowRight') {    # ARROW RIGHT (scroll right)
      shiftFrame('right', step = myPars$scrollFactor)
    } else if (button_key == 'ArrowUp') {       # ARROW UP (horizontal zoom-in)
      changeZoom(myPars$zoomFactor)
    } else if (myPars$listen_alphanum & button_key %in% c('s', 'S')) {    # S (horizontal zoom to selection)
      zoomToSel()
    } else if (button_key == 'ArrowDown') {   # ARROW DOWN (horizontal zoom-out)
      changeZoom(1 / myPars$zoomFactor)
    } else if (button_key == '+') {     # + (vertical zoom-in)
      changeZoom_freq(1 / myPars$zoomFactor_freq)
    } else if (button_key == '-') {    # - (vertical zoom-out)
      changeZoom_freq(myPars$zoomFactor_freq)
    } else if (myPars$listen_alphanum & button_key %in% c('a', 'A')) {  # A (new annotation)
      if (!is.null(myPars$spectrogram_brush))
        showModal(dataModal_new())
    } else if (button_key == 'PageDown') {               # PageDown (next file)
      nextFile()   # disable b/c it's natural to press ENTER to close a modal win
    } else if (button_key == 'PageUp') {                # PageUp (previous file)
      lastFile()
    } else if ((myPars$listen_enter | myPars$listen_enter_edit) & button_key == 'Enter') {
      # this creates a problem if ENTER is pressed too fast --> label not saved
      # if (myPars$listen_enter) {
      #   new_annotation()
      # } else {
      #   edit_annotation()
      # }
    } else if (button_key == 'p') {
      synthAndPlay()
    }
  })


  ## ZOOM
  changeZoom_freq = function(coef) {
    # midpoint = mean(input$spec_ylim)
    # halfRan = diff(input$spec_ylim) / 2 / coef
    # newLow = max(0, midpoint - halfRan)
    # newHigh = min(myPars$samplingRate / 2, midpoint + halfRan)
    newHigh = min(input$spec_ylim[2] * coef, myPars$samplingRate / 2 / 1000)
    updateSliderInput(session, 'spec_ylim', value = c(0, newHigh))
  }
  observeEvent(input$zoomIn_freq, changeZoom_freq(1 / myPars$zoomFactor_freq))
  observeEvent(input$zoomOut_freq, changeZoom_freq(myPars$zoomFactor_freq))
  observeEvent(input$spec_ylim, {
    updateSliderInput(session, 'spectrum_xlim', value = input$spec_ylim)
  })

  changeZoom = function(coef, toCursor = FALSE) {
    # intelligent zoom-in a la Audacity: midpoint moves closer to selection/cursor
    if (!is.null(myPars$cursor) & toCursor) {
      if (!is.null(myPars$spectrogram_brush)) {
        midpoint = 3/4 * mean(c(myPars$spectrogram_brush$xmin,
                                myPars$spectrogram_brush$xmax)) +
          1/4 * mean(myPars$spec_xlim)
      } else {
        if (myPars$cursor > 0) {
          midpoint = 3/4 * myPars$cursor + 1/4 * mean(myPars$spec_xlim)
        } else {
          # when first opening a file, zoom in to the beginning
          midpoint = mean(myPars$spec_xlim) / coef
        }
      }
    } else {
      midpoint = mean(myPars$spec_xlim)
    }
    halfRan = diff(myPars$spec_xlim) / 2 / coef
    newLeft = max(0, midpoint - halfRan)
    newRight = min(myPars$dur, midpoint + halfRan)
    myPars$spec_xlim = c(newLeft, newRight)
    # use user-set time zoom in the next audio
    if (!is.null(myPars$spec_xlim) &&
        !any(!is.finite(myPars$spec_xlim)))
      myPars$initDur = diff(myPars$spec_xlim)
  }
  observeEvent(input$zoomIn, changeZoom(myPars$zoomFactor, toCursor = TRUE))
  observeEvent(input$zoomOut, changeZoom(1 / myPars$zoomFactor))
  zoomToSel = function() {
    if (!is.null(myPars$spectrogram_brush)) {
      myPars$spec_xlim = round(c(myPars$spectrogram_brush$xmin,
                                 myPars$spectrogram_brush$xmax))
    }
  }
  observeEvent(input$zoomToSel, {
    zoomToSel()
  })

  shiftFrame = function(direction, step = 1) {
    ran = diff(myPars$spec_xlim)
    shift = ran * step
    if (direction == 'left') {
      newLeft = max(0, myPars$spec_xlim[1] - shift)
      newRight = newLeft + ran
    } else if (direction == 'right') {
      newRight = min(myPars$dur, myPars$spec_xlim[2] + shift)
      newLeft = newRight - ran
    }
    myPars$spec_xlim = c(newLeft, newRight)
    # update cursor when shifting frame, but not when zooming
    myPars$cursor = myPars$spec_xlim[1]
  }
  observeEvent(input$scrollLeft, shiftFrame('left', step = myPars$scrollFactor))
  observeEvent(input$scrollRight, shiftFrame('right', step = myPars$scrollFactor))

  moveSlider = observe({
    if (myPars$print) print('Moving slider')
    width = round(diff(myPars$spec_xlim) / myPars$dur * 100, 2)
    left = round(myPars$spec_xlim[1] / myPars$dur * 100, 2)
    shinyjs::js$scrollBar(  # need an external js script for this
      id = 'scrollBar',  # defined in UI
      width = paste0(width, '%'),
      left = paste0(left, '%')
    )
    myPars$cursor = myPars$spec_xlim[1]
  })

  observeEvent(input$scrollBarLeft, {
    if (!is.null(myPars$spec)) {
      spec_span = diff(myPars$spec_xlim)
      scrollBarLeft_ms = input$scrollBarLeft * myPars$dur
      myPars$spec_xlim = c(max(0, scrollBarLeft_ms),
                           min(myPars$dur, scrollBarLeft_ms + spec_span))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$scrollBarMove, {
    direction = substr(input$scrollBarMove, 1, 1)
    if (direction == 'l') {
      shiftFrame('left', step = myPars$scrollFactor)
    } else if (direction == 'r') {
      shiftFrame('right', step = myPars$scrollFactor)
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$scrollBarWheel, {
    direction = substr(input$scrollBarWheel, 1, 1)
    if (direction == 'l') {
      shiftFrame('left', step = myPars$wheelScrollFactor)
    } else if (direction == 'r') {
      shiftFrame('right', step = myPars$wheelScrollFactor)
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$zoomWheel, {
    direction = substr(input$zoomWheel, 1, 1)
    if (direction == 'l') {
      changeZoom(1 / myPars$zoomFactor)
    } else if (direction == 'r') {
      changeZoom(myPars$zoomFactor, toCursor = TRUE)
    }
  }, ignoreNULL = TRUE)

  # # step-overlap
  # observeEvent(input$overlap, {
  #   # change step if overlap changes, but don't change step if windowLength changes
  #   step = round(input$windowLength * (1 - input$overlap / 100))
  #   if (input$step != step)
  #     updateNumericInput(session, 'step', value = step)
  # }, ignoreInit = TRUE)
  # observeEvent(c(input$step, input$windowLength), {
  #   # change overlap if step or windowLength change
  #   overlap = (1 - input$step / input$windowLength) * 100
  #   if (input$overlap != overlap)
  #     updateSliderInput(session, 'overlap', value = overlap)
  # })
  #
  #   observeEvent(input$overlap_lpc, {
  #     # change step if overlap changes, but don't change step if windowLength changes
  #     step_lpc = round(input$windowLength_lpc * (1 - input$overlap_lpc / 100))
  #     if (input$step_lpc != step_lpc)
  #       updateNumericInput(session, 'step_lpc', value = step_lpc)
  #   }, ignoreInit = TRUE)
  #   observeEvent(c(input$step_lpc, input$windowLength_lpc), {
  #     # change overlap if step or windowLength change
  #     overlap_lpc = (1 - input$step_lpc / input$windowLength_lpc) * 100
  #     if (input$overlap_lpc != overlap_lpc)
  #       updateSliderInput(session, 'overlap_lpc', value = overlap_lpc)
  #   })


  # SAVE OUTPUT
  done = function() {
    # meaning we are done with a sound - prepares the output
    updateVTL()  # in case of glitches (smts VTL is not updated
    myPars$spectrum_hover = NULL  # clear the spectrum plot
    myPars$spectrum_synth = NULL  # clear synthetic spectrum
    # as it should be when ff are changed manually)
    if (myPars$print) print('Running done()...')
    if (!is.null(myPars$ann)) {
      if (is.null(myPars$out)) {
        myPars$out = myPars$ann
      } else {
        # remove previous records for this file, if any
        idx = which(myPars$out$file == myPars$myAudio_filename)
        if (length(idx) > 0)
          myPars$out = myPars$out[-idx, ]

        # append annotations from the current audio
        myPars$out = soundgen:::rbind_fill(myPars$out, myPars$ann)
      }
      # keep track of formant tracks and spectrograms
      # to avoid analyzing them again if the user goes
      # back and forth between files
      myPars$out_fTracks[[myPars$myAudio_filename]] = myPars$formantTracks
      myPars$out_spects[[myPars$myAudio_filename]] = myPars$spec
    }
    if (!is.null(myPars$out)) {
      # re-order and save a backup
      myPars$out = myPars$out[order(myPars$out$file, myPars$out$from), ]
      my_formants <<- myPars$out
      write.csv(myPars$out, 'www/temp.csv', row.names = FALSE)
    }
  }

  observeEvent(input$fileList, {
    done()
    myPars$n = which(myPars$fileList$name == input$fileList)
    reset()
    if (length(myPars$n) == 1 && myPars$n > 0) readAudio(myPars$n)
  }, ignoreInit = TRUE)

  nextFile = function() {
    if (!is.null(myPars$myAudio_path)) {
      done()
      if (myPars$n < myPars$nFiles) {
        myPars$n = myPars$n + 1
        updateSelectInput(session, 'fileList',
                          selected = myPars$fileList$name[myPars$n])
        # ...which triggers observeEvent(input$fileList)
      }
    }

  }
  observeEvent(input$nextFile, nextFile())

  lastFile = function() {
    if (!is.null(myPars$myAudio_path)) {
      done()
      if (myPars$n > 1) {
        myPars$n = myPars$n - 1
        updateSelectInput(session, 'fileList',
                          selected = myPars$fileList$name[myPars$n])
      }
    }
  }
  observeEvent(input$lastFile, lastFile())

  output$saveRes = downloadHandler(
    filename = function() 'output.csv',
    content = function(filename) {
      done()  # finalize the last file
      write.csv(myPars$out, filename, row.names = FALSE)
      my_formants <<- myPars$out
      if (file.exists('www/temp.csv')) file.remove('www/temp.csv')
      # offer to close the app
      showModal(modalDialog(
        title = "Terminate the app?",
        easyClose = TRUE,
        footer = tagList(
          actionButton("terminate_no", "Keep working"),
          actionButton("terminate_yes", "Terminate")
        )
      ))
    }
  )
  observeEvent(input$terminate_no, {
    removeModal()
  })
  observeEvent(input$terminate_yes, {
    stopApp(returnValue = list(formants = myPars$out,
                               settings = reactiveValuesToList(input)))
    rm(formant_app_defaults, envir = .GlobalEnv)
  })

  observeEvent(input$about, {
    if (myPars$debugQn) {
      browser()  # back door for debugging)
    } else {
      showNotification(
        ui = paste0(
          "App for measuring formants in small annotated segments: soundgen ",
          packageVersion('soundgen'), ". Spectrogram: select an area and ",
          "double-click to add an annotation, left-click to correct a formant ",
          "measure. Spectrum: single-click to update a formant to cursor ",
          "position, double click to update to the closest spectral peak. More ",
          "info: ?formant_app and http://cogsci.se/soundgen.html"),
        duration = 20,
        closeButton = TRUE,
        type = 'default'
      )
    }
  })

  synthAndPlay = function() {
    if (!is.null(myPars$ann[myPars$currentAnn, ]) &&
        any(!is.na(myPars$ann[myPars$currentAnn, myPars$ff]))) {
      if (myPars$print) print('Calling soundgen()...')
      pitch = list(time = c(0, .1, .9, 1),
                   value = c(input$pitch / 1.5, input$pitch,
                             input$pitch / 1.11, input$pitch / 1.5))
      if (input$adaptivePitch)
        pitch$value = pitch$value * 17 / myPars$ann$vtl[myPars$currentAnn]
      ff_vector = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
      # drop NAs in formant frequencies, otherwise soundgen can't synthesize them
      ff_vector = ff_vector[!is.na(ff_vector)]
      temp_s = try(soundgen(
        sylLen = 300 * myPars$samplingRate_idx,
        pitch = pitch,
        formants = ff_vector,
        samplingRate = myPars$samplingRate,
        invalidArgAction = 'ignore',  # otherwise might reset samplingRate
        temperature = .001, tempEffects = list(formDisp = 0, formDrift = 0)))
      if (!inherits(temp_s, 'try-error')) {
        # myPars$spectrum_synth = try(soundgen::getSpectralEnvelope())
        myPars$spectrum_synth = try(as.list(soundgen:::getSmoothSpectrum(
          sound = temp_s,
          samplingRate = myPars$samplingRate,
          len = input$spectrum_len * (myPars$samplingRate / 1000 / 2) / diff(input$spec_ylim),
          loessSpan = 10 ^ input$spectrum_smooth
        )))
        # set to the same upper limit as audio spectrum for neat plotting
        if (is.numeric(myPars$spectrum_synth$ampl)) {
          myPars$spectrum_synth$ampl = myPars$spectrum_synth$ampl -
            max(myPars$spectrum_synth$ampl) + myPars$spectrum_ampl_range[2]
        }
        if (input$audioMethod == 'Browser') {
          # save a temporary file and play with the browser
          seewave::savewav(temp_s,
                           f = myPars$samplingRate * myPars$samplingRate_idx,
                           filename = 'www/temp.wav',
                           extensible = FALSE)
          shinyjs::js$play_file(filename = 'temp.wav')
        } else {
          # play directly in R without saving to disk
          playme(temp_s, samplingRate = myPars$samplingRate * myPars$samplingRate_idx)
        }
      }
    }
  }
  observeEvent(input$synthBtn, synthAndPlay())
}
