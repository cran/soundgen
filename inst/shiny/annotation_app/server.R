# annotation_app()
#
# To do: maybe save empty lines for files with no annotations (as an option - add a box to tick under "general")

# Start with a fresh R session and run the command options(shiny.reactlog=TRUE)
# Then run your app in a show case mode: runApp('inst/shiny/formant_app', display.mode = "showcase")
# At any time you can hit Ctrl+F3 (or for Mac users, Command+F3) in your web browser to launch the reactive log visualization.

server = function(input, output, session) {
  # make plots resizable (js fix)
  shinyjs::js$inheritSize(parentDiv = 'specDiv')
  # set max upload file size to 30 MB
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  env = soundgen:::.soundgen_env

  myPars = reactiveValues(
    print = FALSE,         # if TRUE, some functions print a message to the console when called
    debugQn = FALSE,       # for debugging - click "?" to step into the code
    zoomFactor = 2,        # zoom buttons change time zoom by this factor
    zoomFactor_freq = 1.5, # same for frequency
    shinyTip_show = 1000,  # delay until showing a tip (ms)
    shinyTip_hide = 0,     # delay until hiding a tip (ms)
    initDur = 2000,        # initial duration to plot (ms)
    spec_xlim = c(0, 2000),
    out_specs = list(),    # a list for storing spectrograms
    slider_ms = 50,        # how often to update play slider
    scrollFactor = .75,    # how far to scroll on arrow press/click
    wheelScrollFactor = .1,# how far to scroll on mouse wheel (prop of xlim)
    hotkeys_enabled = TRUE,    # enable/disable hotkeys
    listen_enter = FALSE,      # enable/disable ENTER to close modal (new annotation)
    cursor = 0,
    play = list(on = FALSE)
  )

  tooltip_options = list(delay = list(show = 1000, hide = 0))

  # NB: using myPars$play$cursor for some reason invalidates the observer,
  # so it keeps executing as fast as it can - no idea why!

  # clean-up of the temp folder
  backup_file = file.path(tempdir(), 'temp.csv')
  if (file.exists(backup_file)) {
    showModal(modalDialog(
      title = "Unsaved data",
      "Found unsaved data from a prevous session. Append to the new output?",
      easyClose = FALSE,
      footer = tagList(
        actionButton("discard", "Discard"),
        actionButton("append", "Append")
      )
    ))
  }

  observeEvent(input$discard, {
    file.remove(backup_file)
    removeModal()
  })

  observeEvent(input$append, {
    myPars$out = try(read.csv(backup_file, stringsAsFactors = FALSE))
    myPars$out = unique(myPars$out)  # remove duplicate rows
    removeModal()
  })

  reset = function() {
    if (myPars$print) print('Resetting...')
    myPars$ann = NULL         # a dataframe of annotations for the current file
    myPars$currentAnn = NULL  # the idx of currently selected annotation
    myPars$spec = NULL
    myPars$reassigned = NULL
    myPars$selection = NULL
    myPars$cursor = 0
    myPars$spectrogram_brush = NULL
    shinyjs::js$clearBrush(s = '_brush')
  }

  resetSliders = function() {
    if (myPars$print) print('Resetting sliders...')
    def = env$ann$def
    df = env$ann$def_ann

    for (v in names(df)) {
      if (v %in% names(input)) {
        suppressWarnings(try(updateNumericInput(session, v,
                                                value = def[[v]])))
        suppressWarnings(try(updateSliderInput(session, v,
                                               value = def[[v]])))
      }
    }

    if ('wn' %in% names(input))
      updateSelectInput(session, 'wn', selected = def$wn)
    if ('spec_colorTheme' %in% names(input))
      updateRadioButtons(session, 'spec_colorTheme',
                         selected = def$spec_colorTheme)
    if ('osc' %in% names(input))
      updateSelectInput(session, 'osc', selected = def$osc)
    if ('summaryFun' %in% names(input))
      updateSelectInput(session, 'summaryFun', selected = def$summaryFun)
    if ('audioMethod' %in% names(input))
      updateRadioButtons(session, 'audioMethod',
                         selected = def$audioMethod)
    if ('specType' %in% names(input))
      updateRadioButtons(session, 'specType', selected = def$specType)
  }
  observeEvent(input$reset_to_def, resetSliders(), ignoreNULL = FALSE)

  # helper function to handle modal display and button debouncing
  # (preven OK from closing the model too fast, before the typed-in label is processed)
  openAnnotationModal = function(is_new = TRUE) {
    myPars$hotkeys_enabled = FALSE
    if (is_new) {
      myPars$listen_enter = TRUE
      shiny::showModal(dataModal_new())
    } else {
      myPars$listen_enter = FALSE
      shiny::showModal(dataModal_edit())
    }
  }

  loadAudio = function() {
    if (myPars$print) print('Loading audio...')
    done()
    reset()  # also triggers done(), but done() needs to run first in case loadAudio
    # is re-executed (need to save myPars$ann --> myPars$out)

    # if there is a csv among the uploaded files, use the annotations in it
    ext = tolower(tools::file_ext(input$loadAudio$name))
    old_out_idx = which(ext == 'csv')[1]  # grab the first csv, if any
    if (!is.na(old_out_idx)) {
      user_ann = read.csv(input$loadAudio$datapath[old_out_idx], stringsAsFactors = FALSE)
      oblig_cols = c('file', 'from', 'to')
      if (nrow(user_ann) > 0 &&
          !any(!oblig_cols %in% colnames(user_ann))) {
        idx_missing = which(apply(user_ann[, oblig_cols], 1, function(x) any(is.na(x))))
        if (length(idx_missing) > 0) user_ann = user_ann[-idx_missing, ]
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
      choices = as.list(myPars$fileList$name)
      names(choices) = myPars$fileList$name
      if (identical(input$fileList, myPars$fileList$name[myPars$n]))
        readAudioFile(myPars$n)  # doesn't fire automatically if the same as before
      updateSelectInput(session, 'fileList',
                        choices = as.list(myPars$fileList$name),
                        selected = myPars$fileList$name[myPars$n])
    } else if(!is.na(old_out_idx)) {
      # only a new csv uploaded - just refresh the current file
      readAudioFile(myPars$n)
    }
  }

  observeEvent(input$loadAudio, loadAudio())

  readAudioFile = function(i) {
    # reads an audio file with tuneR::readWave
    if (myPars$print) print('Reading audio...')
    temp = myPars$fileList[i, ]
    myPars$myAudio_filename = temp$name
    myPars$myAudio_path = temp$datapath
    myPars$myAudio_type = temp$type
    extension = tolower(tools::file_ext(myPars$myAudio_filename))

    if (extension == 'wav') {
      myPars$temp_audio = tuneR::readWave(temp$datapath)
    } else if (extension == 'mp3') {
      myPars$temp_audio = tuneR::readMP3(temp$datapath)
    } else {
      warning('Input not recognized: must be a wav or mp3 file')
    }

    myPars$myAudio = as.numeric(myPars$temp_audio@left)
    myPars$ls = length(myPars$myAudio)
    myPars$samplingRate = myPars$temp_audio@samp.rate
    myPars$maxAmpl = 2 ^ (myPars$temp_audio@bit - 1)
    max_abs = max(abs(myPars$myAudio))
    if (input$normalizeInput) {
      myPars$myAudio = myPars$myAudio / max_abs * myPars$maxAmpl
    }
    myPars$nyquist = myPars$samplingRate / 2 / 1000
    myPars$dur = length(myPars$temp_audio@left) * 1000 / myPars$temp_audio@samp.rate
    myPars$myAudio_list = list(
      sound = myPars$myAudio,
      samplingRate = myPars$samplingRate,
      scale = myPars$maxAmpl,
      scale_used = max_abs,
      timeShift = 0,
      ls = length(myPars$myAudio),
      duration = myPars$dur / 1000
    )
    myPars$time = seq(1, myPars$dur, length.out = myPars$ls)
    myPars$spec_xlim = c(0, min(myPars$initDur, myPars$dur))

    # shorten window and step if the input is very short
    max_win = round(myPars$dur / 2)
    if (input$windowLength > myPars$dur) {
      updateNumericInput(session, 'windowLength', value = max_win)
      updateNumericInput(session, 'step', value = max_win / 2)
    }

    # update info - file number ... out of ...
    updateSelectInput(session, 'fileList',
                      label = NULL,
                      selected = myPars$fileList$name[myPars$n])
    file_lab = paste0('File ', myPars$n, ' of ', myPars$nFiles)
    output$fileN = renderUI(HTML(file_lab))

    # if we've already worked with this file,
    # re-load the annotations and (if in current session) spectrogram
    idx = which(myPars$out$file == myPars$myAudio_filename)
    if (length(idx) > 0) {
      myPars$ann = myPars$out[idx, ]
      myPars$currentAnn = 1
    } else {
      myPars$ann = NULL
    }
    myPars$spec = myPars$out_specs[[myPars$myAudio_filename]]

    # embed the audio for playback with the browser
    tmp_wav = tempfile(fileext = ".wav")
    soundgen:::writeAudio(myPars$myAudio, audio = myPars$myAudio_list,
                          filename = tmp_wav)
    raw_audio = readBin(tmp_wav, "raw", file.info(tmp_wav)$size)
    audio_uri = base64enc::dataURI(raw_audio, mime = "audio/wav")
    unlink(tmp_wav)

    output$htmlAudio = renderUI(
      tags$audio(src = audio_uri, id = 'myAudio', style = "display: none;")
    )
  }

  ##############################
  ### SPECTROGRAM EXTRACTION ###
  ##############################

  # Debounced STFT parameters (only these trigger full STFT recomputation)
  stft_params = debounce(reactive({
    list(
      windowLength = input$windowLength,
      step = input$step,
      wn = input$wn,
      zp = input$zp,
      specType = input$specType
    )
  }), 500)

  # Debounced display parameters (these only re-process an existing STFT)
  display_params = debounce(reactive({
    list(
      dynamicRange = input$dynamicRange,
      specContrast = input$specContrast,
      specBrightness = input$specBrightness,
      blur_freq = input$blur_freq,
      blur_time = input$blur_time
    )
  }), 500)

  # Raw STFT computation (only re-runs when STFT params change)
  stft_raw = reactive({
    req(myPars$myAudio_list)
    params = stft_params()

    if (params$specType == 'reassigned') {
      # For reassigned, specManual doesn't support dataframes,
      # so we must recompute everything including display params
      d_params = display_params()
      temp_spec = try(soundgen:::.spectrogram(
        myPars$myAudio_list,
        dynamicRange = d_params$dynamicRange,
        windowLength = params$windowLength,
        step = params$step,
        wn = params$wn,
        zp = 2 ^ params$zp,
        contrast = d_params$specContrast,
        brightness = d_params$specBrightness,
        blur = c(d_params$blur_freq, d_params$blur_time),
        specType = params$specType,
        output = 'all',
        plot = FALSE
      ))
      if (!inherits(temp_spec, 'try-error') && length(temp_spec) > 0) {
        return(list(processed = temp_spec$processed,
                    reassigned = temp_spec$reassigned,
                    is_reassigned = TRUE))
      }
    } else {
      # For rasterized, just compute the raw STFT (no display processing)
      temp_spec = try(soundgen:::.spectrogram(
        myPars$myAudio_list,
        windowLength = params$windowLength,
        step = params$step,
        wn = params$wn,
        zp = 2 ^ params$zp,
        specType = params$specType,
        output = 'all',
        plot = FALSE
      ))
      if (!inherits(temp_spec, 'try-error') && length(temp_spec) > 0) {
        return(list(original = temp_spec$original, is_reassigned = FALSE))
      }
    }
    NULL
  })

  # Processed spectrogram (applies display params; fast for rasterized via specManual)
  spec_processed = reactive({
    req(stft_raw())
    raw = stft_raw()

    if (raw$is_reassigned) {
      # Already fully processed in stft_raw
      return(raw)
    }

    # For rasterized, apply display params using specManual (skips STFT)
    d_params = display_params()
    temp_spec = try(soundgen:::.spectrogram(
      myPars$myAudio_list,
      specManual = raw$original,
      specType = stft_params()$specType,
      dynamicRange = d_params$dynamicRange,
      contrast = d_params$specContrast,
      brightness = d_params$specBrightness,
      blur = c(d_params$blur_freq, d_params$blur_time),
      output = 'all',
      plot = FALSE
    ))
    if (!inherits(temp_spec, 'try-error') && length(temp_spec) > 0) {
      return(list(processed = temp_spec$processed,
                  reassigned = NULL,
                  is_reassigned = FALSE))
    }
    NULL
  })

  # Update reactiveValues when processed spec changes
  observe({
    req(spec_processed())
    res = spec_processed()
    myPars$spec = res$processed
    myPars$reassigned = res$reassigned
  })


  ##################################
  ### AUDIO SCALING & TRIMMING   ###
  ##################################

  # Audio scaling (depends on osc type and dynamicRange, not on spec_xlim)
  audio_scaled = reactive({
    req(myPars$myAudio)
    if (input$osc == 'dB') {
      list(
        sound = osc(myPars$myAudio,
                    dynamicRange = input$dynamicRange,
                    dB = TRUE,
                    maxAmpl = myPars$maxAmpl,
                    plot = FALSE,
                    returnWave = TRUE),
        ylim = c(-2 * input$dynamicRange, 0)
      )
    } else {
      list(
        sound = myPars$myAudio,
        ylim = c(-myPars$maxAmpl, myPars$maxAmpl)
      )
    }
  })

  # Combined trimming reactive: trims spec, reassigned, and osc in one pass
  # This ensures plots always use consistent trimmed data (no stale data risk)
  trimmed_data = reactive({
    req(myPars$spec, audio_scaled())
    if (myPars$print) print('Trimming the spec & osc')

    scaled = audio_scaled()

    # Trim spectrogram
    x = as.numeric(colnames(myPars$spec))
    idx_x = which(x >= (myPars$spec_xlim[1] / 1.05) &
                    x <= (myPars$spec_xlim[2] * 1.05))
    # 1.05 - a bit beyond b/c we use xlim/ylim and may get white space
    y = as.numeric(rownames(myPars$spec))
    idx_y = which(y >= (input$spec_ylim[1] / 1.05) &
                    y <= (input$spec_ylim[2] * 1.05))
    spec_trimmed = downsample_spec(
      myPars$spec[idx_y, idx_x],
      maxPoints = 10 ^ input$spec_maxPoints)

    # Trim reassigned spectrogram
    reassigned_trimmed = NULL
    if (!is.null(myPars$reassigned) && nrow(myPars$reassigned) > 0) {
      idx_r = which(myPars$reassigned$time >= myPars$spec_xlim[1] / 1.05 &
                      myPars$reassigned$time <= myPars$spec_xlim[2] * 1.05 &
                      myPars$reassigned$freq >= input$spec_ylim[1] / 1.05 &
                      myPars$reassigned$freq <= input$spec_ylim[2] * 1.05)
      reassigned_trimmed = myPars$reassigned[idx_r, ]
    }

    # Trim oscillogram
    idx_s = max(1, (myPars$spec_xlim[1] / 1.05 * myPars$samplingRate / 1000)) :
      min(myPars$ls, (myPars$spec_xlim[2] * 1.05 * myPars$samplingRate / 1000))
    downs_osc = 10 ^ input$osc_maxPoints
    myAudio_trimmed = scaled$sound[idx_s]
    time_trimmed = myPars$time[idx_s]
    ls_trimmed = length(myAudio_trimmed)
    if (!is.null(myAudio_trimmed) && ls_trimmed > downs_osc) {
      myseq = round(seq(1, ls_trimmed, length.out = downs_osc))
      myAudio_trimmed = myAudio_trimmed[myseq]
      time_trimmed = time_trimmed[myseq]
      ls_trimmed = length(myseq)
    }

    list(
      spec = spec_trimmed,
      reassigned = reassigned_trimmed,
      audio = myAudio_trimmed,
      time = time_trimmed,
      ls = ls_trimmed,
      ylim_osc = scaled$ylim
    )
  })

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

  #################
  ### P L O T S ###
  #################

  ## SPECTROGRAM
  output$spectrogram = renderPlot({
    if (myPars$print) print('Drawing spectrogram...')
    par(mar = c(0.2, 2, 0.5, 2))  # no need to save user's graphical par-s - revert to orig on exit

    if (is.null(myPars$spec) && is.null(myPars$reassigned)) {
      plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
      text(x = 5, y = 5, cex = 3,
           labels = 'Upload wav/mp3 file(s) to begin...\nSuggested max duration ~10 min')
    } else {
      td = trimmed_data()

      if (input$specType != 'reassigned' && !is.null(td$spec)) {
        # rasterized spectrogram
        soundgen:::filled.contour.mod(
          x = as.numeric(colnames(td$spec)),
          y = as.numeric(rownames(td$spec)),
          z = t(td$spec),
          col = soundgen:::switchColorTheme(input$spec_colorTheme)(input$nColors),
          yScale = input$spec_yScale,
          xlim = myPars$spec_xlim,
          xaxt = 'n',
          xlab = '',
          ylab = '',
          main = '',
          ylim = input$spec_ylim
        )
      } else {
        # unrasterized reassigned spectrogram
        if (!is.null(td$reassigned) && nrow(td$reassigned) > 0) {
          soundgen:::plotUnrasterized(
            td$reassigned,
            col = soundgen:::switchColorTheme(input$spec_colorTheme)(input$nColors),
            yScale = input$spec_yScale,
            xlim = myPars$spec_xlim,
            xaxt = 'n',
            xaxs = 'i', xlab = '',
            ylab = '',
            main = '',
            ylim = input$spec_ylim,
            cex = input$reass_cex
          )
        }
      }

      # Add text label of file name
      if (input$spec_yScale %in% c('bark', 'mel', 'ERB')) {
        spec_ylim = HzToOther(input$spec_ylim * 1000, input$spec_yScale)
        nyquist = HzToOther(myPars$samplingRate / 2, input$spec_yScale)
      } else {
        spec_ylim = input$spec_ylim
        nyquist = myPars$samplingRate / 2000
      }
      if (spec_ylim[2] > nyquist) spec_ylim[2] = nyquist
      text_y_lab = spec_ylim[2] - diff(spec_ylim) * .01
      text(x = myPars$spec_xlim[1] + diff(myPars$spec_xlim) * .01,
           y = text_y_lab,
           labels = myPars$myAudio_filename,
           adj = c(0, 1))  # left, top
    }
  })

  observeEvent(input$spectrogram_click, {
    myPars$cursor = input$spectrogram_click$x
  })

  observeEvent(input$spectrogram_dblclick, {
    if (!is.null(myPars$spectrogram_brush)) {
      openAnnotationModal(TRUE)
    }
  })

  observeEvent(input$spectrogram_brush, {
    myPars$spectrogram_brush = input$spectrogram_brush
  })

  ## OSCILLOGRAM
  output$oscillogram = renderPlot({
    td = trimmed_data()
    if (!is.null(td$audio)) {
      if (myPars$print) print('Drawing osc...')
      par(mar = c(2, 2, 0, 2))
      plot(td$time,
           td$audio,
           type = 'l',
           xlim = myPars$spec_xlim,
           ylim = td$ylim_osc,
           axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o',
           xlab = 'Time, ms',
           ylab = '')
      box()
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

  ## ANNOTATIONS
  output$ann_plot = renderPlot({
    td = trimmed_data()
    if (myPars$print) print('Drawing annotations...')

    if (!is.null(myPars$ann) && nrow(myPars$ann) > 0) {
      par(mar = c(0, 2, 0, 2))
      plot(td$time,
           xlim = myPars$spec_xlim,
           ylim = c(.2, .8),
           type = 'n',
           xaxs = "i", yaxs = "i",
           bty = 'n',
           axes = FALSE,
           xlab = '', ylab = ''
      )
      for (i in seq_len(nrow(myPars$ann))) {
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
    }
  })

  observeEvent(input$ann_dblclick, {
    # select and edit the double-clicked annotation
    if (!is.null(myPars$ann)) {
      ds = abs(input$ann_dblclick$x - (myPars$ann$from + myPars$ann$to) / 2)
      myPars$currentAnn = which.min(ds)
      openAnnotationModal(FALSE)
    }
  })

  dataModal_new = function() {
    modalDialog(
      textInput("annotation", "New annotation:",
                placeholder = '...some info...'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_new", "OK")
      ),
      easyClose = FALSE
    )
  }

  new_annotation = function() {
    if (myPars$print) print('Creating a new annotation...')
    new = data.frame(
      file = myPars$myAudio_filename,
      from = round(myPars$spectrogram_brush$xmin),
      to = round(myPars$spectrogram_brush$xmax),
      label = input$annotation,
      stringsAsFactors = FALSE)

    # depending on the history, there may be more columns in myPars$ann than in
    # the current sel
    if (is.null(myPars$ann)) {
      myPars$ann = new
    } else {
      myPars$ann = soundgen:::rbind_fill(myPars$ann, new)
    }

    # reorder and select the newly added annotation
    ord = order(myPars$ann$from)
    myPars$ann = myPars$ann[ord, ]
    myPars$currentAnn = which(ord == nrow(myPars$ann))

    # clear the selection, close the modal
    removeModal()
    myPars$hotkeys_enabled = TRUE
    myPars$listen_enter = FALSE

    # save a backup in case the app crashes before done() fires
    temp = soundgen:::rbind_fill(myPars$out, myPars$ann)
    temp = unique(temp[order(temp$file), ])  # remove duplicate rows
    write.csv(temp, backup_file, row.names = FALSE)
  }

  observeEvent(input$ok_new, {
    new_annotation()
  })

  dataModal_edit = function() {
    modalDialog(
      textInput("annotation", "Edit annotation:",
                value = myPars$ann$label[myPars$currentAnn],
                placeholder = '...some info...'
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok_edit", "OK")
      ),
      easyClose = FALSE
    )
  }
  edit_annotation = function() {
    myPars$ann$label[myPars$currentAnn] = input$annotation
    removeModal()
    myPars$hotkeys_enabled = TRUE
  }
  observeEvent(input$ok_edit, edit_annotation())

  observeEvent(input$modal_hidden, {
    myPars$hotkeys_enabled = TRUE
  }, ignoreInit = TRUE)


  observeEvent(myPars$ann, {
    if (myPars$print) print('Drawing ann_table...')
    if (!is.null(myPars$ann)) {
      show_cols = c('from', 'to', 'label')
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
      session$sendCustomMessage('highlightRow', myPars$currentAnn)
    }
  }

  observeEvent(input$tableRow, {
    if (!is.null(myPars$ann) && input$tableRow > 0) {
      myPars$currentAnn = input$tableRow
      myPars$spectrogram_brush = list(xmin = myPars$ann$from[myPars$currentAnn],
                                      xmax = myPars$ann$to[myPars$currentAnn])
    }
  }, ignoreInit = TRUE)

  ## Buttons for operations with selection
  startPlay = function() {
    req(myPars$myAudio)
    if (myPars$print) print('Playing selection...')
    # ensure cursor is valid before calculating play region
    if (is.null(myPars$cursor) || is.na(myPars$cursor)) {
      myPars$cursor = myPars$spec_xlim[1]
    }
    if (!is.null(myPars$spectrogram_brush) &&
        # at least 50 ms selected
        (myPars$spectrogram_brush$xmax - myPars$spectrogram_brush$xmin > 50)) {
      myPars$play$from = myPars$spectrogram_brush$xmin / 1000
      myPars$play$to = myPars$spectrogram_brush$xmax / 1000
    } else {
      myPars$play$from = myPars$cursor / 1000
      myPars$play$to = myPars$spec_xlim[2] / 1000
    }
    myPars$play$dur = myPars$play$to - myPars$play$from
    myPars$play$timeOn = proc.time()
    myPars$play$timeOff = myPars$play$timeOn + myPars$play$dur
    myPars$play$on = TRUE
    myPars$play$original_cursor = myPars$cursor

    if (input$audioMethod == 'Browser') {
      shinyjs::js$playme_js(audio_id = 'myAudio',
                            from = myPars$play$from,
                            to = myPars$play$to)
    } else {
      idx_start = max(1, round(myPars$play$from * myPars$samplingRate))
      idx_end = min(myPars$ls, round(myPars$play$to * myPars$samplingRate))
      if (idx_end >= idx_start) {
        audio_subset = myPars$myAudio[idx_start:idx_end]
        soundgen::playme(
          audio_subset,
          samplingRate = myPars$samplingRate)
      }
    }
  }
  observeEvent(input$selection_play, startPlay(), ignoreInit = TRUE)

  stopPlay = function() {
    # Prevent infinite loop if called from the audio_playback_done observer
    if (!isTRUE(myPars$play$on)) return(invisible(NULL))
    if (myPars$print) print('stopPlay')
    myPars$play$on = FALSE # Set to FALSE before calling JS
    shinyjs::js$stopAudio_js(audio_id = 'myAudio')

    if (is.numeric(myPars$play$original_cursor)) {
      myPars$cursor = myPars$play$original_cursor
    } else {
      myPars$cursor = myPars$spec_xlim[1]
    }
  }
  observeEvent(c(input$selection_stop, input$audio_playback_done),
               stopPlay(), ignoreInit = TRUE)

  observe({
    req(myPars$play$on, input$audioMethod == 'R')
    invalidateLater(20)
    time = proc.time()
    if ((time - myPars$play$timeOff)[3] > 0) {
      myPars$play$on = FALSE
      myPars$play$finished_at = Sys.time() # Record finish time
    } else {
      myPars$cursor = myPars$play$from * 1000 +
        as.numeric(time - myPars$play$timeOn)[3] * 1000
    }
  })

  deleteSel = function() {
    if (!is.null(myPars$currentAnn)) {
      myPars$ann = myPars$ann[-myPars$currentAnn, ]
      myPars$selection = NULL
      myPars$currentAnn = NULL
    }
  }
  observeEvent(input$selection_delete, deleteSel())
  observeEvent(input$selection_annotate, {
    if (!is.null(myPars$spectrogram_brush)) {
      openAnnotationModal(TRUE)
    }
  })

  snapAnnotation = function() {
    if (is.null(myPars$currentAnn) || is.null(myPars$spectrogram_brush))
      return(invisible(NULL))
    new_from = round(myPars$spectrogram_brush$xmin)
    new_to   = round(myPars$spectrogram_brush$xmax)
    if (new_to > new_from) {
      myPars$ann$from[myPars$currentAnn] = new_from
      myPars$ann$to[myPars$currentAnn]   = new_to
    }
  }
  observeEvent(input$selection_snap, snapAnnotation())

  # HOTKEYS
  observeEvent(input$userPressedSmth, {
    button_key = substr(input$userPressedSmth, 1, nchar(input$userPressedSmth) - 8)
    if (!myPars$hotkeys_enabled) return()
    # see https://keycode.info/
    if (button_key == ' ') {                  # SPACEBAR (play / stop)
      if (isTRUE(myPars$play$on)) {
        stopPlay()
      } else {
        # Ignore queued Spacebar presses that arrive right after R playback finishes
        if (!is.null(myPars$play$finished_at) &&
            as.numeric(Sys.time() - myPars$play$finished_at, units = "secs") < 0.25) {
          myPars$play$finished_at = NULL
          return(invisible(NULL))
        }
        startPlay()
      }
    } else if (button_key %in% c('Delete', 'Backspace')) {    # DELETE (delete current annotation)
      deleteSel()
    } else if (button_key == 'ArrowLeft') {    # ARROW LEFT (scroll left)
      shiftFrame('left', step = myPars$scrollFactor)
    } else if (button_key == 'ArrowRight') {    # ARROW RIGHT (scroll right)
      shiftFrame('right', step = myPars$scrollFactor)
    } else if (button_key == 'ArrowUp') {       # ARROW UP (horizontal zoom-in)
      changeZoom(myPars$zoomFactor)
    } else if (button_key %in% c('s', 'S')) {    # S (horizontal zoom to selection)
      zoomToSel()
    } else if (button_key == 'ArrowDown') {   # ARROW DOWN (horizontal zoom-out)
      changeZoom(1 / myPars$zoomFactor)
    } else if (button_key == '+') {     # + (vertical zoom-in)
      changeZoom_freq(1 / myPars$zoomFactor_freq)
    } else if (button_key == '-') {    # - (vertical zoom-out)
      changeZoom_freq(myPars$zoomFactor_freq)
    } else if (button_key %in% c('a', 'A')) {  # A (new annotation)
      if (!is.null(myPars$spectrogram_brush))
        openAnnotationModal(TRUE)
    } else if (button_key == 'PageDown') {   # PageDown (next file)
      nextFile()
    } else if (button_key == 'PageUp') {     # PageUp (previous file)
      lastFile()
    } else if (button_key %in% c('t', 'T')) {
      snapAnnotation()
    }
  })

  ## ZOOM
  changeZoom_freq = function(coef) {
    newHigh = min(input$spec_ylim[2] * coef, myPars$samplingRate / 2 / 1000)
    updateSliderInput(session, 'spec_ylim', value = c(0, newHigh))
  }

  observeEvent(input$zoomIn_freq, changeZoom_freq(1 / myPars$zoomFactor_freq))
  observeEvent(input$zoomOut_freq, changeZoom_freq(myPars$zoomFactor_freq))

  changeZoom = function(coef, toCursor = FALSE) {
    # intelligent zoom-in a la Audacity: midpoint moves closer to selection/cursor
    if (!is.null(myPars$cursor) && toCursor) {
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
      newRight = min(myPars$dur, newLeft + ran)
      newLeft = max(0, newRight - ran)
    } else if (direction == 'right') {
      newRight = min(myPars$dur, myPars$spec_xlim[2] + shift)
      newLeft = max(0, newRight - ran)
      newRight = min(myPars$dur, newLeft + ran)
    }
    myPars$spec_xlim = c(newLeft, newRight)
    # update cursor when shifting frame, but not when zooming
    myPars$cursor = myPars$spec_xlim[1]
  }

  observeEvent(input$scrollLeft, shiftFrame('left', step = myPars$scrollFactor))
  observeEvent(input$scrollRight, shiftFrame('right', step = myPars$scrollFactor))

  moveSlider = observe({
    req(myPars$dur > 0)
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

  # SAVE OUTPUT
  done = function() {
    # meaning we are done with a sound - prepares the output
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
      # keep track of spectrograms to avoid analyzing them again if the user
      # goes back and forth between files
      myPars$out_specs[[myPars$myAudio_filename]] = myPars$spec
    }
    if (!is.null(myPars$out)) {
      # re-order and save a backup
      myPars$out = myPars$out[order(myPars$out$file, myPars$out$from), ]
      write.csv(myPars$out, backup_file, row.names = FALSE)
    }
  }

  observeEvent(input$fileList, {
    done()
    myPars$n = which(myPars$fileList$name == input$fileList)
    reset()
    if (length(myPars$n) == 1 && myPars$n > 0) readAudioFile(myPars$n)
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
      if (file.exists(backup_file))
        file.remove(backup_file)
      # offer to close the app
      showModal(modalDialog(
        title = "Terminate the app?",
        easyClose = FALSE,
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
    # capture current settings for reproducibility
    settings_list = reactiveValuesToList(input)
    settings_list = lapply(settings_list, function(x)
      if(is.vector(x) && !is.null(names(x))) unname(x) else x)
    stopApp(returnValue = list(annotations = myPars$out, settings = settings_list))
  })

  observeEvent(input$about, {
    if (myPars$debugQn) {
      browser()  # back door for debugging)
    } else {
      showNotification(
        ui = paste0(
          "App for annotating audio: soundgen ",
          packageVersion('soundgen'), ". Select an area of the spectrogram and ",
          "double-click or press A to add an annotation"),
        duration = 20,
        closeButton = TRUE,
        type = 'default'
      )
    }
  })
}
