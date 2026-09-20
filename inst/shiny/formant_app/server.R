# formant_app()
#
# To do: IPA chart as another option in addition to Hillenbrand; synthetic sound - set pitch to mean f0 in annotated region - enable pitch tracking (but slow); maybe an option to have log-spectrogram and log-spectrum (a bit tricky b/c all layers have to be adjusted); LPC saves all available formants - check behavior when changing nFormants across annotations & files; maybe arbitrary number of annotation tiers

# Start with a fresh R session and run the command options(shiny.reactlog=TRUE)
# Then run your app in a show case mode: runApp('inst/shiny/formant_app', display.mode = "showcase")
# At any time you can hit Ctrl+F3 (or for Mac users, Command+F3) in your web browser to launch the reactive log visualization.


# # tip: to read the output, do smth like:
# a = read.csv('~/Downloads/output.csv', stringsAsFactors = FALSE)
# as.numeric(unlist(strsplit(a$pitch, ',')))


server = function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024 ^ 2)
  shinyjs::js$inheritSize(parentDiv = 'specDiv')
  env = soundgen:::.soundgen_env

  myPars = reactiveValues(
    print = FALSE, debugQn = FALSE,
    zoomFactor = 2, zoomFactor_freq = 1.5,
    initDur = 2000, spec_xlim = c(0, 2000),
    out_fTracks = list(), out_specs = list(),
    selectedF = 'F1', scrollFactor = .75,
    wheelScrollFactor = .1, cursor = 0,
    play = list(on = FALSE), maxF = 4,
    samplingRate_idx = 1, synth_audio = NULL,
    hotkeys_enabled = TRUE
  )

  # Pre-register observers for all possible formant buttons
  # (nFormants ranges from 1 to 10 per def_form)
  # SERVER: Replace the existing `for (f in 1:10)` loop with this:
  for (f in 1:10) {
    local({
      f_local = f
      # Formant selection
      observeEvent(input[[paste0('selF_', f_local)]], {
        myPars$selectedF = paste0('F', f_local)
        for (i in 1:input$nFormants) {
          btn_id = paste0('selF_', i)
          if (i == f_local) {
            shinyjs::addClass(id = btn_id, class = 'selected')
          } else {
            shinyjs::removeClass(id = btn_id, class = 'selected')
          }
        }
      }, ignoreInit = TRUE)

      # NA button
      observeEvent(input[[paste0('F', f_local, '_NA')]], {
        if (!is.null(myPars$currentAnn) && !is.null(myPars$ann)) {
          myPars$ann[myPars$currentAnn, paste0('F', f_local)] = NA
          updateFBtn()
          updateVTL()
        }
      }, ignoreInit = FALSE)
    })
  }

  observeEvent(input$nFormants, {
    myPars$ff = paste0('F', 1:input$nFormants)
    if (!is.null(myPars$formantTracks)) {
      missingCols = myPars$ff[
        which(!myPars$ff %in% colnames(myPars$formantTracks))]
      if (length(missingCols) > 0)
        myPars$formantTracks[, missingCols] = NA
    }

    if (!is.null(myPars$ann)) {
      missingCols = myPars$ff[
        which(!myPars$ff %in% colnames(myPars$ann))]
      if (length(missingCols) > 0)
        myPars$ann[, missingCols] = NA

      if (!is.null(myPars$currentAnn) &&
          !is.null(myPars$formants)) {
        for (f in 1:input$nFormants) {
          col = paste0('F', f)
          if (is.na(myPars$ann[myPars$currentAnn, col]) &&
              col %in% names(myPars$formants) &&
              !is.na(myPars$formants[col])) {
            myPars$ann[myPars$currentAnn, col] =
              myPars$formants[col]
          }
        }
      }
    }

    # Highlight the currently selected formant button
    for (i in 1:input$nFormants) {
      btn_id = paste0('selF_', i)
      if (paste0('F', i) == myPars$selectedF) {
        shinyjs::addClass(id = btn_id, class = 'selected')
      } else {
        shinyjs::removeClass(id = btn_id, class = 'selected')
      }
    }

    avFmPerSel()
  }, ignoreInit = TRUE)

  # clean-up of the temp folder
  backup_file = file.path(tempdir(), 'temp.csv')
  if (file.exists(backup_file)) {
    showModal(modalDialog(
      title = "Unsaved data",
      "Found unsaved data from a previous session. Append?",
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
  }, ignoreInit = TRUE)

  observeEvent(input$append, {
    myPars$out = try(read.csv(backup_file,
                              stringsAsFactors = FALSE))
    if (!inherits(myPars$out, 'try-error')) myPars$out = unique(myPars$out)
    removeModal()
  }, ignoreInit = TRUE)

  reset = function() {
    if (myPars$print) print('Resetting...')
    myPars$ann = NULL
    myPars$currentAnn = NULL
    myPars$spec = NULL
    myPars$reassigned = NULL
    myPars$formantTracks = NULL
    myPars$formants = NULL
    myPars$analyzedUpTo = NULL
    myPars$selection = NULL
    myPars$cursor = 0
    myPars$spectrogram_brush = NULL
  }

  resetSliders = function() {
    if (myPars$print) print('Resetting sliders...')
    def = env$formant$def
    df = env$formant$def_form

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
    if ('wn_lpc' %in% names(input))
      updateSelectInput(session, 'wn_lpc', selected = def$wn_lpc)
    if ('spec_colorTheme' %in% names(input))
      updateRadioButtons(session, 'spec_colorTheme',
                         selected = def$spec_colorTheme)
    if ('osc' %in% names(input))
      updateSelectInput(session, 'osc', selected = def$osc)
    if ('vtl_method' %in% names(input))
      updateSelectInput(session, 'vtl_method', selected = def$vtl_method)
    if ('speedSound' %in% names(input))
      updateNumericInput(session, 'speedSound', value = def$speedSound)
    if ('coeffs' %in% names(input))
      updateTextInput(session, 'coeffs', value = def$coeffs)
    if ('interceptZero' %in% names(input))
      updateCheckboxInput(session, 'interceptZero',
                          value = def$interceptZero)
    if ('tube' %in% names(input))
      updateSelectInput(session, 'tube', selected = def$tube)
    if ('summaryFun' %in% names(input))
      updateSelectInput(session, 'summaryFun', selected = def$summaryFun)
    if ('audioMethod' %in% names(input))
      updateRadioButtons(session, 'audioMethod',
                         selected = def$audioMethod)
    if ('specType' %in% names(input))
      updateRadioButtons(session, 'specType', selected = def$specType)
    if ('spec_col' %in% names(input))
      updateTextInput(session, 'spec_col', value = def$spec_col)
  }
  observeEvent(input$reset_to_def, resetSliders(), ignoreInit = TRUE)

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
    reset()

    ext = tolower(tools::file_ext(input$loadAudio$name))
    old_out_idx = which(ext == 'csv')[1]

    if (!is.na(old_out_idx)) {
      user_ann = read.csv(input$loadAudio$datapath[old_out_idx],
                          stringsAsFactors = FALSE)
      oblig_cols = c('file', 'from', 'to')
      if (nrow(user_ann) > 0 && !any(!oblig_cols %in% colnames(user_ann))) {
        idx_missing = which(apply(user_ann[, oblig_cols], 1,
                                  function(x) any(is.na(x))))
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
            myPars$out = unique(myPars$out)
          }
        }
      }
    }

    idx_audio = which(ext %in% c('wav', 'mp3'))
    if (length(idx_audio) > 0) {
      if (is.null(myPars$fileList)) {
        myPars$fileList = input$loadAudio[idx_audio, ]
        myPars$n = 1
      } else {
        sameFiles = which(myPars$fileList$name %in% input$loadAudio$name)
        if (length(sameFiles) > 0) {
          message('Uploading same audio file overwrites annotations')
          if (!is.null(myPars$out)) {
            myPars$out = myPars$out[!myPars$out$file %in%
                                      myPars$fileList$name[sameFiles]]
            if (nrow(myPars$out) == 0) myPars$out = NULL
          }
          myPars$fileList = myPars$fileList[-sameFiles, ]
        }
        myPars$n = nrow(myPars$fileList) + 1
        myPars$fileList = rbind(myPars$fileList,
                                input$loadAudio[idx_audio, ])
      }
      myPars$nFiles = nrow(myPars$fileList)
      choices = as.list(myPars$fileList$name)
      names(choices) = myPars$fileList$name
      if (identical(input$fileList, myPars$fileList$name[myPars$n]))
        readAudioFile(myPars$n)
      updateSelectInput(session, 'fileList', choices = choices,
                        selected = myPars$fileList$name[myPars$n])
    } else if(!is.na(old_out_idx)) {
      readAudioFile(myPars$n)
    }
  }
  observeEvent(input$loadAudio, loadAudio(), ignoreInit = TRUE)

  readAudioFile = function(i) {
    if (myPars$print) print('Reading audio...')
    temp = myPars$fileList[i, ]
    myPars$myAudio_filename = temp$name
    myPars$myAudio_path = temp$datapath
    myPars$myAudio_type = temp$type
    extension = tolower(tools::file_ext(temp$name))

    if (extension == 'wav') {
      myPars$temp_audio = tuneR::readWave(temp$datapath)
    } else if (extension == 'mp3') {
      myPars$temp_audio = tuneR::readMP3(temp$datapath)
    } else {
      warning('Input not recognized: must be a wav or mp3 file')
      return()
    }

    myPars$myAudio = as.numeric(myPars$temp_audio@left)
    myPars$ls = length(myPars$myAudio)
    myPars$samplingRate = myPars$temp_audio@samp.rate
    myPars$maxAmpl = 2 ^ (myPars$temp_audio@bit - 1)
    max_abs = max(abs(myPars$myAudio))

    if (isTRUE(input$normalizeInput)) {
      myPars$myAudio = myPars$myAudio / max_abs * myPars$maxAmpl
    }

    myPars$nyquist = myPars$samplingRate / 2 / 1000
    myPars$dur = length(myPars$temp_audio@left) * 1000 /
      myPars$temp_audio@samp.rate
    myPars$time = seq(1, myPars$dur, length.out = myPars$ls)
    myPars$spec_xlim = c(0, min(myPars$initDur, myPars$dur))
    myPars$regionToAnalyze = myPars$spec_xlim

    myPars$myAudio_list = list(
      sound = myPars$myAudio,
      samplingRate = myPars$samplingRate,
      scale = myPars$maxAmpl,
      scale_used = max_abs,
      timeShift = 0,
      ls = length(myPars$myAudio),
      duration = myPars$dur / 1000
    )

    max_win = round(myPars$dur / 2)
    if (input$windowLength > myPars$dur) {
      updateNumericInput(session, 'windowLength', value = max_win)
      updateNumericInput(session, 'step', value = max_win / 2)
    }
    if (input$windowLength_lpc > myPars$dur) {
      updateNumericInput(session, 'windowLength_lpc', value = max_win)
      updateNumericInput(session, 'step_lpc', value = max_win / 2)
    }

    updateSelectInput(session, 'fileList', label = NULL,
                      selected = myPars$fileList$name[myPars$n])
    file_lab = paste0('File ', myPars$n, ' of ', myPars$nFiles)
    output$fileN = renderUI(HTML(file_lab))

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
    }

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

  spec_data = reactive({
    req(myPars$myAudio)
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
        length(temp_spec) > 0) {
      if (!is.null(temp_spec$processed) &&
          is.matrix(temp_spec$processed)) {
        myPars$spec = temp_spec$processed
      }
      if (!is.null(temp_spec$reassigned)) {
        myPars$reassigned = temp_spec$reassigned
      }
    }
  }) |> shiny::debounce(500)

  observe({ spec_data() })

  trimmed_data = reactive({
    req(myPars$myAudio)
    if (myPars$print) print('Trimming the spec & osc')

    if (input$osc == 'dB') {
      myAudio_scaled = soundgen::osc(
        myPars$myAudio,
        dynamicRange = input$dynamicRange,
        dB = TRUE,
        maxAmpl = myPars$maxAmpl,
        plot = FALSE, returnWave = TRUE)
      ylim_osc = c(-2 * input$dynamicRange, 0)
    } else {
      myAudio_scaled = myPars$myAudio
      ylim_osc = c(-myPars$maxAmpl, myPars$maxAmpl)
    }

    # Trim ordinary spectrogram (may be NULL for reassigned)
    spec_trimmed = NULL
    if (!is.null(myPars$spec)) {
      x = as.numeric(colnames(myPars$spec))
      idx_x = which(x >= (myPars$spec_xlim[1] / 1.05) &
                      x <= (myPars$spec_xlim[2] * 1.05))
      y = as.numeric(rownames(myPars$spec))
      idx_y = which(y >= (input$spec_ylim[1] / 1.05) &
                      y <= (input$spec_ylim[2] * 1.05))
      spec_trimmed = myPars$spec[idx_y, idx_x, drop = FALSE]
      lxy = nrow(spec_trimmed) * ncol(spec_trimmed)
      maxPoints = 10 ^ input$spec_maxPoints
      if (lxy > maxPoints) {
        downs = sqrt(lxy / maxPoints)
        seqx = round(seq(1, ncol(spec_trimmed),
                         length.out = ncol(spec_trimmed) / downs))
        seqy = round(seq(1, nrow(spec_trimmed),
                         length.out = nrow(spec_trimmed) / downs))
        spec_trimmed = spec_trimmed[seqy, seqx, drop = FALSE]
      }
    }

    # Trim oscillogram
    idx_s = max(1,
                (myPars$spec_xlim[1] / 1.05 *
                   myPars$samplingRate / 1000)) :
      min(myPars$ls,
          (myPars$spec_xlim[2] * 1.05 *
             myPars$samplingRate / 1000))
    myAudio_trimmed = myAudio_scaled[idx_s]
    time_trimmed = myPars$time[idx_s]
    downs_osc = 10 ^ input$osc_maxPoints
    if (length(myAudio_trimmed) > downs_osc) {
      myseq = round(seq(1, length(myAudio_trimmed),
                        length.out = downs_osc))
      myAudio_trimmed = myAudio_trimmed[myseq]
      time_trimmed = time_trimmed[myseq]
    }

    list(spec = spec_trimmed,
         osc = myAudio_trimmed,
         time = time_trimmed,
         ylim_osc = ylim_osc)
  })

  observeEvent(c(myPars$spec, myPars$spec_xlim, myPars$analyzedUpTo), {
    req(myPars$spec)
    if (is.null(myPars$analyzedUpTo)) {
      myPars$regionToAnalyze = myPars$spec_xlim
      call = TRUE
    } else {
      if (myPars$analyzedUpTo < myPars$spec_xlim[2]) {
        myPars$regionToAnalyze = c(myPars$analyzedUpTo, myPars$spec_xlim[2])
        if (diff(myPars$regionToAnalyze) < 500) {
          myPars$regionToAnalyze[2] = min(myPars$regionToAnalyze[2] + 500,
                                          myPars$dur)
          if (diff(myPars$regionToAnalyze) < 500) {
            myPars$regionToAnalyze[1] = max(myPars$regionToAnalyze[2] - 500, 0)
          }
        }
        call = TRUE
      } else {
        call = FALSE
      }
    }
    if (call) extractFormants()
  }, ignoreInit = TRUE)

  observeEvent(
    c(input$windowLength_lpc, input$step_lpc, input$wn_lpc, input$zp_lpc,
      input$dynamicRange_lpc, input$silence, input$minformant, input$maxbw), {
        extractFormants()
      }, ignoreInit = TRUE
  )

  extractFormants = function() {
    if (is.null(myPars$myAudio) || is.null(myPars$regionToAnalyze)) {
      return(invisible(NULL))
    }
    if (myPars$print) print('Extracting formants...')
    if (input$coeffs != '') {
      coeffs = as.numeric(input$coeffs)
    } else {
      coeffs = NULL
    }

    sel_anal = max(1, round(myPars$regionToAnalyze[1] / 1000 *
                              myPars$samplingRate)) :
      min(myPars$ls, round(myPars$regionToAnalyze[2] / 1000 *
                             myPars$samplingRate))

    myPars$myAudio_list_sel = list(
      sound = myPars$myAudio[sel_anal],
      samplingRate = myPars$samplingRate,
      scale = myPars$maxAmpl,
      timeShift = myPars$regionToAnalyze[1]/1000,
      ls = length(myPars$myAudio),
      duration = myPars$dur / 1000
    )

    temp_anal = try(soundgen:::getFormants(
      audio = myPars$myAudio_list_sel,
      windowLength = input$windowLength_lpc,
      step = input$step_lpc,
      wn = input$wn_lpc,
      dynamicRange = input$dynamicRange_lpc,
      silence = input$silence,
      formants = list(
        coeffs = coeffs,
        minformant = input$minformant,
        maxbw = input$maxbw
      ),
      nFormants = NULL
    ))

    if (!inherits(temp_anal, 'try-error') && length(temp_anal) > 0 &&
        is.list(temp_anal)) {
      myPars$temp_anal = temp_anal
      myPars$nMeasuredFmts = length(grep('_freq', colnames(myPars$temp_anal)))
      myPars$allF_colnames = paste0('f', 1:myPars$nMeasuredFmts, '_freq')

      myPars$maxF = max(myPars$maxF, myPars$nMeasuredFmts, input$nFormants)

      myPars$ff = paste0('F', 1:min(input$nFormants, myPars$nMeasuredFmts))
      myPars$temp_anal = myPars$temp_anal[, c('time', myPars$allF_colnames)]
      colnames(myPars$temp_anal) = c('time', paste0('F', 1:myPars$nMeasuredFmts))

      for (c in colnames(myPars$temp_anal)) {
        if (any(!is.na(myPars$temp_anal[, c]))) {
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
          myPars$formantTracks = soundgen:::rbind_fill(myPars$formantTracks,
                                                       myPars$temp_anal)
        }
      })
    }
  }

  observeEvent(
    c(input$nFormants, input$silence, input$coeffs, input$minformant,
      input$maxbw, input$windowLength_lpc, input$step_lpc,
      input$wn_lpc, input$zp_lpc, input$dynamicRange_lpc), {
        myPars$analyzedUpTo = 0
      },
    ignoreInit = TRUE
  )

  observeEvent(myPars$samplingRate, {
    updateTextInput(session, 'coeffs',
                    placeholder = as.character(round(myPars$samplingRate/1000) + 3))
  }, ignoreInit = TRUE)

  output$fButtons = renderUI({
    c(
      list(actionButton(
        inputId = 'remF',
        label = bslib::tooltip(
          HTML("<img src='icons/minus.png' width = '25px'>"),
          "Remove one formant"),
        class = "buttonInline"
      )),
      lapply(1:input$nFormants, function(f) {
        is_selected = (paste0('F', f) == myPars$selectedF)
        btn_class = paste0('buttonInline fBoxBtn', if (is_selected) ' selected' else '')
        tags$div(
          class = 'fBox',
          actionButton(
            inputId = paste0('selF_', f),
            label = tagList(
              tags$div(class = "fLabel", paste0('F', f)),
              shiny::textOutput(paste0('F', f, '_val'), inline = TRUE)
            ),
            class = btn_class
          ),
          actionButton(paste0('F', f, '_NA'), label = "NA", class = "buttonInline fNA")
        )
      }),
      list(actionButton(
        inputId = 'addF',
        label = bslib::tooltip(
          HTML("<img src='icons/plus.png' width = '25px'>"),
          "Add one formant"),
        class = "buttonInline"
      )),
      list(actionButton(
        inputId = 'defaultFmtBtn',
        label = bslib::tooltip(
          HTML("<img src='icons/update.png' width = '30px'>"),
          "Reset formant values to defaults"),
        class = "buttonInline"
      ))
    )
  })

  updateFBtn = function() {
    if (is.null(myPars$currentAnn) || is.null(myPars$ann)) {
      return(invisible(NULL))
    }
    for (f in 1:input$nFormants) {
      local({
        f_local = f
        val = myPars$ann[myPars$currentAnn, paste0('F', f_local)]
        output[[paste0('F', f_local, '_val')]] = renderText({
          if (is.na(val)) "NA" else round(val)
        })
      })
    }
  }

  observeEvent(input$addF, {
    updateSliderInput(session, 'nFormants', value = input$nFormants + 1)
  }, ignoreInit = TRUE)

  observeEvent(input$remF, {
    updateSliderInput(session, 'nFormants', value = max(1, input$nFormants - 1))
  }, ignoreInit = TRUE)

  observeEvent(input$defaultFmtBtn, {
    if (!is.null(myPars$formants) && !is.null(myPars$currentAnn)) {
      for (f in 1:input$nFormants) {
        col = paste0('F', f)
        if (col %in% names(myPars$formants)) {
          myPars$ann[myPars$currentAnn, col] = myPars$formants[col]
        }
      }
      updateFBtn()
      updateVTL()
    }
  }, ignoreInit = TRUE)

  output$spectrogram = renderPlot({
    td = trimmed_data()
    if (myPars$print) print('Drawing spectrogram...')
    par(mar = c(0.2, 2, 0.5, 2))

    if (input$specType != 'reassigned') {
      req(td$spec)
      try(soundgen:::filled.contour.mod(
        x = as.numeric(colnames(td$spec)),
        y = as.numeric(rownames(td$spec)),
        z = t(td$spec),
        col = soundgen:::switchColorTheme(
          input$spec_colorTheme)(input$nColors),
        yScale = 'linear',
        xlim = myPars$spec_xlim,
        xaxt = 'n', xlab = '', ylab = '', main = '',
        ylim = input$spec_ylim
      ))
    } else {
      req(myPars$reassigned)
      reass = myPars$reassigned
      if (nrow(reass) > 0) {
        idx_x = reass[, 1] >= myPars$spec_xlim[1] &
          reass[, 1] <= myPars$spec_xlim[2]
        idx_y = reass[, 2] >= input$spec_ylim[1] &
          reass[, 2] <= input$spec_ylim[2]
        reass = reass[idx_x & idx_y, , drop = FALSE]
      }
      try(soundgen:::plotUnrasterized(
        reass,
        col = soundgen:::switchColorTheme(
          input$spec_colorTheme)(input$nColors),
        yScale = 'linear',
        xlim = myPars$spec_xlim,
        xaxt = 'n', xaxs = 'i',
        xlab = '', ylab = '', main = '',
        ylim = input$spec_ylim,
        cex = input$reass_cex
      ))
    }

    ran_x = myPars$spec_xlim[2] - myPars$spec_xlim[1]
    ran_y = input$spec_ylim[2] - input$spec_ylim[1]
    text(x = myPars$spec_xlim[1] + ran_x * .01,
         y = input$spec_ylim[2] - ran_y * .01,
         labels = myPars$myAudio_filename,
         adj = c(0, 1))
  })

  observe({
    myPars$specOver_opts = list(
      xlim = myPars$spec_xlim, ylim = input$spec_ylim,
      xaxs = "i", yaxs = "i",
      bty = 'n', xaxt = 'n', yaxt = 'n',
      xlab = '', ylab = '')
  })

  output$specOver = renderPlot({
    req(myPars$spec)
    par(mar = c(0.2, 2, 0.5, 2))
    do.call(plot, c(list(x = myPars$spec_xlim, y = input$spec_ylim, type = 'n'),
                    myPars$specOver_opts))

    if (!is.null(myPars$currentAnn)) {
      isolate({
        rect(
          xleft = myPars$ann$from[myPars$currentAnn],
          xright = myPars$ann$to[myPars$currentAnn],
          ybottom = input$spec_ylim[1], ytop = input$spec_ylim[2],
          col = rgb(.2, .2, .2, alpha = .25), border = NA
        )
      })
    }

    if (!is.null(myPars$spectrogram_brush)) {
      rect(
        xleft = myPars$spectrogram_brush$xmin,
        xright = myPars$spectrogram_brush$xmax,
        ybottom = input$spec_ylim[1], ytop = input$spec_ylim[2],
        col = rgb(.2, .2, .2, alpha = .15), border = NA
      )
    }

    if (!is.null(myPars$formantTracks)) {
      for (f in 2:(length(myPars$ff) + 1)) {
        points(myPars$formantTracks$time, myPars$formantTracks[, f] / 1000,
               pch = 16, col = input$spec_col, cex = input$spec_cex)
      }
    }

    if (!is.null(myPars$currentAnn)) {
      ff = myPars$ann[myPars$currentAnn, myPars$ff, drop = FALSE]
      text(x = rep(myPars$ann$from[myPars$currentAnn], length(ff)),
           y = as.numeric(ff) / 1000,
           labels = names(ff), adj = c(0, .5), col = 'green', cex = 1.5)
    }

    if (!is.null(myPars$spectrogram_hover)) {
      do.call(points, c(list(x = myPars$spec_xlim,
                             y = rep(myPars$spectrogram_hover$y, 2),
                             type = 'l', lty = 3), myPars$specOver_opts))
      do.call(text, list(x = myPars$spec_xlim[1],
                         y = myPars$spectrogram_hover$y,
                         labels = myPars$spectrogram_hover$freq,
                         adj = c(0, 0)))
      do.call(points, list(x = rep(myPars$spectrogram_hover$x, 2),
                           y = input$spec_ylim, type = 'l', lty = 3))
      do.call(text, list(x = myPars$spectrogram_hover$x,
                         y = input$spec_ylim[1] + .025 * diff(input$spec_ylim),
                         labels = soundgen:::convert_sec_to_hms(
                           myPars$spectrogram_hover$x / 1000, 3),
                         adj = .5))
    }
  }, bg = 'transparent')

  output$specSlider = renderPlot({
    req(myPars$spec)
    par(mar = c(0.2, 2, 0.5, 2))
    cursor_val = if (is.null(myPars$cursor) || is.na(myPars$cursor)) 0 else myPars$cursor
    # Always draw the line, even if cursor is 0
    do.call(plot, c(list(x = rep(cursor_val, 2), y = input$spec_ylim,
                         type = 'l'), myPars$specOver_opts))
  }, bg = 'transparent')

  observeEvent(input$spectrogram_hover, {
    req(myPars$spec)
    myPars$spectrogram_hover = input$spectrogram_hover
    cursor_hz = myPars$spectrogram_hover$y * 1000
    cursor_notes = soundgen:::HzToNotes(cursor_hz)
    myPars$spectrogram_hover$freq = paste0(round(cursor_hz), ' Hz (',
                                           cursor_notes, ')')
  }, ignoreInit = TRUE)

  shinyjs::onevent('mouseout', id = 'specOver', {
    myPars$spectrogram_hover = NULL
    ## shinyjs::js$clearBrush(s = '_brush')
  })

  observeEvent(input$spectrogram_click, {
    myPars$cursor = input$spectrogram_click$x
    if (!is.null(myPars$currentAnn)) {
      inside_sel = (myPars$ann$from[myPars$currentAnn] < input$spectrogram_click$x) &&
        (myPars$ann$to[myPars$currentAnn] > input$spectrogram_click$x)
      if (inside_sel) {
        myPars$ann[myPars$currentAnn,
                   myPars$selectedF] = round(input$spectrogram_click$y * 1000)
        updateFBtn()
        updateVTL()
      } else {
        myPars$spectrogram_brush = NULL
        shinyjs::js$clearBrush(s = '_brush')
      }
    } else {
      myPars$spectrogram_brush = NULL
      shinyjs::js$clearBrush(s = '_brush')
    }
  }, ignoreInit = TRUE)

  observeEvent(input$spectrogram_dblclick, {
    if (!is.null(myPars$spectrogram_brush)) openAnnotationModal(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$spectrogram_brush, {
    myPars$spectrogram_brush = input$spectrogram_brush
  }, ignoreInit = TRUE)

  output$oscillogram = renderPlot({
    td = trimmed_data()
    req(td$osc)
    if (myPars$print) print('Drawing osc...')
    par(mar = c(2, 2, 0, 2))
    plot(td$time, td$osc, type = 'l', xlim = myPars$spec_xlim, ylim = td$ylim_osc,
         axes = FALSE, xaxs = "i", yaxs = "i", bty = 'o', xlab = 'Time, ms', ylab = '')
    box()
    time_location = axTicks(1)
    time_labels = soundgen:::convert_sec_to_hms(time_location / 1000, 3)
    axis(side = 1, at = time_location, labels = time_labels)
    if (input$osc == 'dB') {
      axis(side = 4, at = seq(0, input$dynamicRange, by = 10))
      mtext("dB", side = 2, line = 3)
    }
    abline(h = 0, lty = 2)
  }, execOnResize = TRUE)

  output$ann_plot = renderPlot({
    req(myPars$spec)
    td = trimmed_data()
    if (myPars$print) print('Drawing annotations...')
    par(mar = c(0, 2, 0, 2))
    if (!is.null(myPars$ann) && nrow(myPars$ann) > 0) {
      plot(td$time, xlim = myPars$spec_xlim, ylim = c(.2, .8), type = 'n',
           xaxs = "i", yaxs = "i", bty = 'n', axes = FALSE, xlab = '', ylab = '')
      for (i in 1:nrow(myPars$ann)) {
        r = rnorm(1, 0, .05)
        highlight = is.numeric(myPars$currentAnn) && i == myPars$currentAnn
        segments(x0 = myPars$ann$from[i], x1 = myPars$ann$to[i], y0 = .5 + r, y1 = .5 + r,
                 lwd = ifelse(highlight, 3, 2), col = ifelse(highlight, 'blue', 'black'))
        segments(x0 = myPars$ann$from[i], x1 = myPars$ann$from[i], y0 = .45 + r, y1 = .55 + r,
                 lwd = ifelse(highlight, 3, 2), col = ifelse(highlight, 'blue', 'black'))
        segments(x0 = myPars$ann$to[i], x1 = myPars$ann$to[i], y0 = .45 + r, y1 = .55 + r,
                 lwd = ifelse(highlight, 3, 2), col = ifelse(highlight, 'blue', 'black'))
        middle_i = mean(as.numeric(myPars$ann[i, c('from', 'to')]))
        text(x = middle_i, y = .5 + r, labels = myPars$ann$label[i], adj = c(.5, 0), cex = 1.5)
      }
    } else {
      plot(1:10, type = 'n', bty = 'n', axes = FALSE, xlab = '', ylab = '')
      text(5, 5,
           labels = 'Select a region of spectrogram and double-click to create an annotation')
    }
  })

  observeEvent(myPars$currentAnn, {
    req(myPars$currentAnn, myPars$ann)
    if (myPars$print) print('Updating selection...')
    sel_points = as.numeric(round(myPars$ann[myPars$currentAnn, c('from', 'to')] /
                                    1000 * myPars$samplingRate))
    sel_points[1] = max(0, sel_points[1])
    sel_points[2] = min(sel_points[2], myPars$ls)
    idx_points = sel_points[1]:sel_points[2]
    myPars$selection = myPars$myAudio[idx_points]

    ann_dur = myPars$ann$to[myPars$currentAnn] - myPars$ann$from[myPars$currentAnn]
    mid_view = mean(myPars$spec_xlim)
    mid_ann = mean(as.numeric(myPars$ann[myPars$currentAnn, c('from', 'to')]))
    shift = mid_ann - mid_view

    if (myPars$ann$from[myPars$currentAnn] < myPars$spec_xlim[1] ||
        myPars$ann$to[myPars$currentAnn] > myPars$spec_xlim[2]) {
      if (diff(myPars$spec_xlim) > ann_dur) {
        myPars$spec_xlim[1] = max(0, myPars$spec_xlim[1] + shift)
        myPars$spec_xlim[2] = min(myPars$dur, myPars$spec_xlim[2] + shift)
      } else {
        half_span = ann_dur * 1.5 / 2
        myPars$spec_xlim[1] = max(0, mid_ann - half_span)
        myPars$spec_xlim[2] = min(myPars$dur, mid_ann + half_span)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ann_click, {
    req(myPars$ann)
    ds = abs(input$ann_click$x - (myPars$ann$from + myPars$ann$to) / 2)
    myPars$currentAnn = which.min(ds)
    myPars$spectrogram_brush = list(xmin = myPars$ann$from[myPars$currentAnn],
                                    xmax = myPars$ann$to[myPars$currentAnn])
    myPars$cursor = myPars$ann$from[myPars$currentAnn]
    avFmPerSel()
  }, ignoreInit = TRUE)

  observeEvent(input$ann_dblclick, {
    req(myPars$ann)
    ds = abs(input$ann_dblclick$x - (myPars$ann$from + myPars$ann$to) / 2)
    myPars$currentAnn = which.min(ds)
    openAnnotationModal(FALSE)
  }, ignoreInit = TRUE)

  output$spectrum = renderPlot({
    req(myPars$spectrum)
    if (myPars$print) print('Drawing spectrum...')
    par(mar = c(2, 2, 0.5, 0))
    xlim = input$spectrum_xlim
    ylim = c(myPars$spectrum_ampl_range[1], myPars$spectrum_ampl_range[2] + 5)
    plot(myPars$spectrum$freq, myPars$spectrum$ampl, xlab = '', ylab = '',
         xlim = xlim, ylim = ylim, xaxs = "i", type = 'l')
    spectrum_peaks()

    if (!is.null(myPars$spectrum_hover)) {
      abline(v = myPars$spectrum_hover$x, lty = 2)
      text(x = myPars$spectrum_hover$x, y = myPars$spectrum_ampl_range[1],
           labels = myPars$spectrum_hover$cursor, adj = c(0, 0))
      text(x = myPars$spectrum_hover$pf, y = myPars$spectrum_hover$pa,
           labels = myPars$spectrum_hover$peak, adj = c(0, 0))
      points(x = myPars$spectrum_hover$pf, y = myPars$spectrum_hover$pa, pch = 16)
    }

    if (!is.null(myPars$ann) && any(is.numeric(myPars$currentAnn))) {
      ff = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
    } else if (!is.null(myPars$formants) && any(!is.na(as.numeric(myPars$formants)))) {
      ff = as.numeric(myPars$formants)
    } else {
      ff = numeric(0)
    }

    if (length(ff) > 0) {
      for (i in seq_along(ff)) {
        f = ff[i] / 1000
        if (is.numeric(f) & any(!is.na(f))) {
          idx_f = which.min(abs(myPars$spectrum$freq - f))
          text(x = f, y = myPars$spectrum$ampl[idx_f] + 2,
               labels = paste0('F', i), adj = c(.5, 0))
          segments(x0 = f, y0 = ylim[1] - .04 * diff(ylim), x1 = f,
                   y1 = myPars$spectrum$ampl[idx_f], lty = 3, col = 'lightgreen')
        }
      }
    }

    if (isTRUE(input$spectrum_plotSynth) && !is.null(myPars$spectrum_synth)) {
      points(myPars$spectrum_synth$freq, myPars$spectrum_synth$ampl,
             type = 'l', lty = 2, col = 'blue')
    }
  })

  observe({
    req(myPars$spec)
    if (myPars$print) print('Extracting spectrum of selection...')
    if (!is.null(myPars$selection) && length(myPars$selection) > 0) {
      spec_temp = soundgen::spectrum(myPars$selection,
                                     samplingRate = myPars$samplingRate, plot = FALSE)
    } else {
      time_stamps = as.numeric(colnames(myPars$spec))
      idx = which(time_stamps >= myPars$spec_xlim[1] & time_stamps <= myPars$spec_xlim[2])
      if (length(idx) > 1) {
        ampl = as.numeric(rowMeans(myPars$spec[, idx, drop = FALSE]))
        ampl[ampl == 0] = 1e-16
        spec_temp = data.frame(freq = as.numeric(rownames(myPars$spec[, idx])), ampl = ampl)
      } else {
        return()
      }
    }
    freqWindow_bins = ceiling(nrow(spec_temp) / input$spectrum_len /
                                diff(input$spec_ylim))
    spec_temp_env = soundgen::getSpecEnv(spec_temp, freqWindow_bins = freqWindow_bins)
    myPars$spectrum = data.frame(freq = as.numeric(names(spec_temp_env)),
                                 ampl = 20 * log10(spec_temp_env / max(spec_temp_env)))

    isolate({
      idx = try(which(myPars$spectrum$freq < input$spec_ylim[2]))
      if (!inherits(idx, 'try-error')) {
        myPars$spectrum_ampl_range = range(myPars$spectrum$ampl[idx])
      }
    })
  })

  spectrum_peaks = reactive({
    req(myPars$spectrum)
    if (myPars$print) print('Looking for spectral peaks...')
    myPars$spectrum_peaks = soundgen::findPeaks(myPars$spectrum$ampl)
  })

  observeEvent(input$spectrum_click, {
    req(myPars$currentAnn)
    myPars$ann[myPars$currentAnn, myPars$selectedF] = round(input$spectrum_click$x * 1000)
    updateFBtn()
    updateVTL()
  }, ignoreInit = TRUE)

  observeEvent(input$spectrum_dblclick, {
    req(myPars$currentAnn)
    pf_idx = which.min(abs(input$spectrum_dblclick$x -
                             myPars$spectrum$freq[myPars$spectrum_peaks]))
    pf = myPars$spectrum$freq[myPars$spectrum_peaks[pf_idx]]
    myPars$ann[myPars$currentAnn, myPars$selectedF] = round(pf * 1000)
    updateFBtn()
    updateVTL()
  }, ignoreInit = TRUE)

  observeEvent(input$spectrum_hover, {
    req(myPars$spectrum)
    myPars$spectrum_hover = data.frame(x = input$spectrum_hover$x,
                                       y = input$spectrum_hover$y)
    cursor_hz = round(input$spectrum_hover$x * 1000)
    cursor_notes = soundgen:::HzToNotes(cursor_hz)
    myPars$spectrum_hover$cursor = paste0(cursor_hz, 'Hz (', cursor_notes, ')')

    pf_idx = which.min(abs(myPars$spectrum_hover$x -
                             myPars$spectrum$freq[myPars$spectrum_peaks]))
    myPars$spectrum_hover$pa = myPars$spectrum$ampl[myPars$spectrum_peaks[pf_idx]]
    myPars$spectrum_hover$pf = myPars$spectrum$freq[myPars$spectrum_peaks[pf_idx]]
    nearest_peak_hz = round(myPars$spectrum_hover$pf * 1000)
    nearest_peak_notes = soundgen:::HzToNotes(nearest_peak_hz)
    myPars$spectrum_hover$peak = paste0(
      nearest_peak_hz, ' Hz (', nearest_peak_notes, ')')
  }, ignoreInit = TRUE)

  output$fmtSpace = renderPlot({
    if (!is.null(myPars$currentAnn) && !is.null(myPars$ann[myPars$currentAnn, ]) &&
        (is.finite(myPars$ann[myPars$currentAnn, ]$F1) &&
         is.finite(myPars$ann[myPars$currentAnn, ]$F2))) {
      if (myPars$print) print('Drawing formant space')
      caf = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
      if (input$fmtSpacePlot == 'vowelSpace') {
        cafr = soundgen::schwa(formants = caf)$ff_relative_dF
        hillenbrand = soundgen::hillenbrand
        xlim = range(c(hillenbrand$F1Rel, cafr[1]))
        ylim = range(c(hillenbrand$F2Rel, cafr[2]))
        par(mar = c(0, 0, 0, 0))
        plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n', xlab = '',
             ylab = '', xlim = xlim, ylim = ylim, bty = 'n', xaxt = 'n', yaxt = 'n')
        text(hillenbrand$F1Rel, hillenbrand$F2Rel, labels = hillenbrand$vowel,
             cex = 1.5, col = 'blue')
        points(cafr[1], cafr[2], pch = 4, cex = 2.5, col = 'red')
      } else if (input$fmtSpacePlot == 'regression') {
        if (input$vtl_method == 'regression') {
          soundgen:::getdF(
            formants = caf, method = 'regression', interceptZero = input$interceptZero,
            tube = input$tube, speedSound = input$speedSound, plot = TRUE
          )
        } else {
          plot(1:10, type='n', bty='n', xlab='', ylab='', xaxt='n', yaxt='n')
          text(5, 9, label = 'Only VTL estimation with regression can be plotted here')
        }
      }
    } else {
      hillenbrand = soundgen::hillenbrand
      par(mar = c(0, 0, 0, 0))
      plot(hillenbrand$F1Rel, hillenbrand$F2Rel, type = 'n', xlab = '',
           ylab = '', bty = 'n', xaxt = 'n', yaxt = 'n')
      text(hillenbrand$F1Rel, hillenbrand$F2Rel,
           labels = hillenbrand$vowel, cex = 1.5, col = 'blue')
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
      dF = NA, vtl = NA,
      stringsAsFactors = FALSE)

    all_cols = paste0('F', 1:myPars$maxF)
    new[, all_cols] = NA

    if (is.null(myPars$ann)) {
      myPars$ann = new
    } else {
      myPars$ann = soundgen:::rbind_fill(myPars$ann, new)
    }

    ord = order(myPars$ann$from)
    myPars$ann = myPars$ann[ord, ]
    myPars$currentAnn = which(ord == nrow(myPars$ann))

    avFmPerSel()
    myPars$ann[myPars$currentAnn, myPars$ff] = myPars$formants[myPars$ff]
    updateFBtn()
    updateVTL()
    removeModal()
    save_backup()
  }
  observeEvent(input$ok_new, {
    new_annotation()
    myPars$hotkeys_enabled = TRUE
  }, ignoreInit = TRUE)

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

  avFmPerSel = function() {
    if (is.null(myPars$currentAnn) || is.null(myPars$formantTracks)) {
      return(invisible(NULL))
    }
    if (myPars$print) print('Averaging formants in selection...')
    isolate({
      idx = which(
        myPars$formantTracks$time >= myPars$ann$from[myPars$currentAnn] &
          myPars$formantTracks$time <= myPars$ann$to[myPars$currentAnn]
      )
    })
    fMat = myPars$formantTracks[idx, 2:(length(myPars$ff) + 1), drop = FALSE]
    myPars$formants = apply(fMat, 2, function(x)
      round(do.call(input$summaryFun, list(x, na.rm = TRUE))))
    updateFBtn()
  }
  observeEvent(myPars$formantTracks, avFmPerSel(), ignoreInit = TRUE)

  observeEvent(myPars$ann, {
    if (myPars$print) print('Drawing ann_table...')
    if (!is.null(myPars$ann)) {
      show_cols = c('from', 'to', 'label', 'dF', 'vtl',
                    paste0('F', 1:input$nFormants))
      show_cols = intersect(show_cols, colnames(myPars$ann))
      ann_for_print = myPars$ann[, show_cols]
    } else {
      ann_for_print = '...waiting for some annotations...'
    }
    output$ann_table = renderTable(
      format(ann_for_print), align = 'c', striped = FALSE, bordered = TRUE,
      hover = FALSE, width = '100%')
  }, ignoreNULL = FALSE)

  updateVTL = function(rows = myPars$currentAnn) {
    if (is.null(rows) || length(rows) == 0 || is.null(myPars$ann)) {
      return(invisible(NULL))
    }
    if (myPars$print) print('Updating VTL...')
    for (i in rows) {
      if (i <= nrow(myPars$ann) && any(!is.na(myPars$ann[i, myPars$ff]))) {
        fmts_ann = as.numeric(myPars$ann[i, myPars$ff])
        vtl_ann = soundgen::estimateVTL(
          formants = fmts_ann,
          method = input$vtl_method,
          speedSound = input$speedSound,
          interceptZero = input$interceptZero,
          tube = input$tube,
          output = 'detailed'
        )
        try({ myPars$ann$dF[i] = round(vtl_ann$dF) }, silent = TRUE)
        try({ myPars$ann$vtl[i] = round(vtl_ann$vocalTract, 2) }, silent = TRUE)
      }
    }
  }

  observeEvent(c(input$vtl_method, input$speedSound,
                 input$interceptZero, input$tube), {
                   req(myPars$ann)
                   if (nrow(myPars$ann) > 0) updateVTL(rows = 1:nrow(myPars$ann))
                 }, ignoreInit = TRUE)

  observeEvent(input$tableRow, {
    req(myPars$ann)
    if (input$tableRow > 0 && input$tableRow <= nrow(myPars$ann)) {
      myPars$currentAnn = input$tableRow
      myPars$spectrogram_brush = list(xmin = myPars$ann$from[myPars$currentAnn],
                                      xmax = myPars$ann$to[myPars$currentAnn])
      avFmPerSel()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$samplingRate_mult, {
    myPars$samplingRate_idx = 2 ^ input$samplingRate_mult
  }, ignoreInit = TRUE)

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
                            to = myPars$play$to,
                            rate = myPars$samplingRate_idx)
    } else {
      idx_start = max(1, round(myPars$play$from * myPars$samplingRate))
      idx_end = min(myPars$ls, round(myPars$play$to * myPars$samplingRate))
      if (idx_end >= idx_start) {
        audio_subset = myPars$myAudio[idx_start:idx_end]
        soundgen::playme(
          audio_subset,
          samplingRate = myPars$samplingRate * myPars$samplingRate_idx)
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

  observeEvent(input$audio_current_time, {
    # Guard against stray events arriving after playback has stopped
    if (isTRUE(myPars$play$on) && is.numeric(input$audio_current_time)) {
      myPars$cursor = input$audio_current_time * 1000
    }
  }, ignoreInit = TRUE)

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

  # Play a synthetic vowel
  synthAndPlay = function() {
    if (is.null(myPars$ann) || is.null(myPars$currentAnn)) {
      return(invisible(NULL))
    }
    if (any(!is.na(myPars$ann[myPars$currentAnn, myPars$ff]))) {
      if (myPars$print) print('Calling soundgen()...')
      pitch = list(time = c(0, .1, .9, 1),
                   value = c(input$pitch / 1.5, input$pitch,
                             input$pitch / 1.11, input$pitch / 1.5))
      if (isTRUE(input$adaptivePitch)) {
        pitch$value = pitch$value * 17 / myPars$ann$vtl[myPars$currentAnn]
      }
      ff_vector = as.numeric(myPars$ann[myPars$currentAnn, myPars$ff])
      ff_vector = ff_vector[!is.na(ff_vector)]

      temp_s = try(soundgen::soundgen(
        sylLen = 300, pitch = pitch, formants = ff_vector,
        samplingRate = myPars$samplingRate, invalidArgAction = 'ignore',
        temperature = .001,
        tempEffects = list(formDisp = 0, formDrift = 0)))

      if (!inherits(temp_s, 'try-error')) {
        myPars$synth_audio = temp_s

        spec_synth = soundgen::spectrum(temp_s,
                                        samplingRate = myPars$samplingRate,
                                        plot = FALSE)
        freqWindow_bins = round(nrow(spec_synth) / input$spectrum_len /
                                  diff(input$spec_ylim))
        spec_synth_env = soundgen::getSpecEnv(spec_synth,
                                              freqWindow_bins = freqWindow_bins)
        myPars$spectrum_synth = data.frame(
          freq = as.numeric(names(spec_synth_env)),
          ampl = 20 * log10(spec_synth_env / max(spec_synth_env)))

        if (is.numeric(myPars$spectrum_synth$ampl) &&
            !is.null(myPars$spectrum_ampl_range)) {
          myPars$spectrum_synth$ampl = myPars$spectrum_synth$ampl -
            max(myPars$spectrum_synth$ampl) + myPars$spectrum_ampl_range[2]
        }

        if (input$audioMethod == 'Browser') {
          tmp_wav = tempfile(fileext = ".wav")
          soundgen:::writeAudio(
            temp_s,
            audio = soundgen:::readAudio(temp_s,
                                         samplingRate = myPars$samplingRate),
            filename = tmp_wav,
            scale_used = max(abs(temp_s))
          )
          raw_audio = readBin(tmp_wav, "raw", file.info(tmp_wav)$size)
          audio_uri = base64enc::dataURI(raw_audio, mime = "audio/wav")
          unlink(tmp_wav)

          shinyjs::js$play_synth_uri(uri = audio_uri,
                                     rate = myPars$samplingRate_idx)
        } else {
          soundgen::playme(
            temp_s, samplingRate = myPars$samplingRate * myPars$samplingRate_idx)
        }
      }
    }
  }
  observeEvent(input$synthBtn, synthAndPlay(), ignoreInit = TRUE)

  # plot the synthetic spectrum
  observeEvent(input$spectrum_len, {
    req(myPars$synth_audio)
    spec_synth = soundgen::spectrum(myPars$synth_audio,
                                    samplingRate = myPars$samplingRate,
                                    plot = FALSE)
    freqWindow_bins = round(nrow(spec_synth) / input$spectrum_len /
                              diff(input$spec_ylim))
    spec_synth_env = soundgen::getSpecEnv(spec_synth,
                                          freqWindow_bins = freqWindow_bins)
    myPars$spectrum_synth = data.frame(
      freq = as.numeric(names(spec_synth_env)),
      ampl = 20 * log10(spec_synth_env / max(spec_synth_env)))

    if (is.numeric(myPars$spectrum_synth$ampl) &&
        !is.null(myPars$spectrum_ampl_range)) {
      myPars$spectrum_synth$ampl = myPars$spectrum_synth$ampl -
        max(myPars$spectrum_synth$ampl) + myPars$spectrum_ampl_range[2]
    }
  }, ignoreInit = TRUE)

  deleteSel = function() {
    if (is.null(myPars$currentAnn)) return(invisible(NULL))
    myPars$ann = myPars$ann[-myPars$currentAnn, ]
    myPars$selection = NULL
    myPars$currentAnn = NULL
    myPars$spectrum_synth = NULL
  }
  observeEvent(input$selection_delete, deleteSel(), ignoreInit = TRUE)

  observeEvent(input$selection_annotate, {
    req(myPars$spectrogram_brush)
    showModal(dataModal_new())
  }, ignoreInit = TRUE)

  snapAnnotation = function() {
    if (is.null(myPars$currentAnn) || is.null(myPars$spectrogram_brush)) {
      return(invisible(NULL))
    }
    new_from = round(myPars$spectrogram_brush$xmin)
    new_to   = round(myPars$spectrogram_brush$xmax)
    if (new_to > new_from) {
      myPars$ann$from[myPars$currentAnn] = new_from
      myPars$ann$to[myPars$currentAnn]   = new_to
    }
  }
  observeEvent(input$selection_snap, snapAnnotation(), ignoreInit = TRUE)

  observeEvent(input$userPressedSmth, {
    if (!isTRUE(myPars$hotkeys_enabled)) return(invisible(NULL))
    button_key = substr(input$userPressedSmth, 1, nchar(input$userPressedSmth) - 8)
    if (button_key == ' ') {
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
    } else if (button_key %in% c('Delete', 'Backspace')) {
      deleteSel()
    } else if (button_key == 'ArrowLeft') {
      shiftFrame('left', step = myPars$scrollFactor)
    } else if (button_key == 'ArrowRight') {
      shiftFrame('right', step = myPars$scrollFactor)
    } else if (button_key == 'ArrowUp') {
      changeZoom(myPars$zoomFactor)
    } else if (button_key %in% c('s', 'S')) {
      zoomToSel()
    } else if (button_key == 'ArrowDown') {
      changeZoom(1 / myPars$zoomFactor)
    } else if (button_key == '+') {
      changeZoom_freq(1 / myPars$zoomFactor_freq)
    } else if (button_key == '-') {
      changeZoom_freq(myPars$zoomFactor_freq)
    } else if (button_key %in% c('a', 'A')) {
      req(myPars$spectrogram_brush)
      showModal(dataModal_new())
    } else if (button_key == 'PageDown') {
      nextFile()
    } else if (button_key == 'PageUp') {
      lastFile()
    } else if (button_key == 'p') {
      synthAndPlay()
    } else if (button_key %in% c('t', 'T')) {
      snapAnnotation()
    }
  }, ignoreInit = TRUE)

  changeZoom_freq = function(coef) {
    newHigh = min(input$spec_ylim[2] * coef, myPars$samplingRate / 2 / 1000)
    updateSliderInput(session, 'spec_ylim', value = c(0, newHigh))
  }
  observeEvent(input$zoomIn_freq, changeZoom_freq(1 / myPars$zoomFactor_freq),
               ignoreInit = TRUE)
  observeEvent(input$zoomOut_freq, changeZoom_freq(myPars$zoomFactor_freq),
               ignoreInit = TRUE)

  observeEvent(input$spec_ylim, {
    updateSliderInput(session, 'spectrum_xlim', value = input$spec_ylim)
  }, ignoreInit = TRUE)

  changeZoom = function(coef, toCursor = FALSE) {
    if (!is.null(myPars$cursor) && toCursor) {
      if (!is.null(myPars$spectrogram_brush)) {
        midpoint = 3/4 * mean(c(
          myPars$spectrogram_brush$xmin,
          myPars$spectrogram_brush$xmax)) + 1/4 * mean(myPars$spec_xlim)
      } else if (myPars$cursor > 0) {
        midpoint = 3/4 * myPars$cursor + 1/4 * mean(myPars$spec_xlim)
      } else {
        midpoint = mean(myPars$spec_xlim) / coef
      }
    } else {
      midpoint = mean(myPars$spec_xlim)
    }
    halfRan = diff(myPars$spec_xlim) / 2 / coef
    newLeft = max(0, midpoint - halfRan)
    newRight = min(myPars$dur, midpoint + halfRan)
    myPars$spec_xlim = c(newLeft, newRight)
    if (!any(!is.finite(myPars$spec_xlim))) myPars$initDur = diff(myPars$spec_xlim)
  }
  observeEvent(input$zoomIn, changeZoom(myPars$zoomFactor, toCursor = TRUE),
               ignoreInit = TRUE)
  observeEvent(input$zoomOut, changeZoom(1 / myPars$zoomFactor),
               ignoreInit = TRUE)

  zoomToSel = function() {
    if (is.null(myPars$spectrogram_brush)) return(invisible(NULL))
    myPars$spec_xlim = round(c(myPars$spectrogram_brush$xmin,
                               myPars$spectrogram_brush$xmax))
  }
  observeEvent(input$zoomToSel, zoomToSel(), ignoreInit = TRUE)

  shiftFrame = function(direction, step = 1) {
    ran = diff(myPars$spec_xlim)
    shift = ran * step
    if (direction == 'left') {
      newLeft = max(0, myPars$spec_xlim[1] - shift)
      newRight = min(myPars$dur, newLeft + ran)
    } else {
      newRight = min(myPars$dur, myPars$spec_xlim[2] + shift)
      newLeft = max(0, newRight - ran)
    }
    myPars$spec_xlim = c(newLeft, newRight)
    myPars$cursor = myPars$spec_xlim[1]
  }
  observeEvent(input$scrollLeft, shiftFrame('left', step = myPars$scrollFactor),
               ignoreInit = TRUE)
  observeEvent(input$scrollRight, shiftFrame('right', step = myPars$scrollFactor),
               ignoreInit = TRUE)

  moveSlider = observe({
    req(myPars$dur > 0)
    if (myPars$print) print('Moving slider')
    width = round(diff(myPars$spec_xlim) / myPars$dur * 100, 2)
    left = round(myPars$spec_xlim[1] / myPars$dur * 100, 2)
    shinyjs::js$scrollBar(id = 'scrollBar', width = paste0(width, '%'),
                          left = paste0(left, '%'))
  })

  observeEvent(input$scrollBarLeft, {
    req(myPars$spec)
    spec_span = diff(myPars$spec_xlim)
    scrollBarLeft_ms = input$scrollBarLeft * myPars$dur
    myPars$spec_xlim = c(max(0, scrollBarLeft_ms),
                         min(myPars$dur, scrollBarLeft_ms + spec_span))
  }, ignoreInit = TRUE)

  observeEvent(input$scrollBarMove, {
    direction = substr(input$scrollBarMove, 1, 1)
    if (direction == 'l') shiftFrame('left', step = myPars$scrollFactor)
    else if (direction == 'r') shiftFrame('right', step = myPars$scrollFactor)
  }, ignoreInit = TRUE)

  observeEvent(input$scrollBarWheel, {
    direction = substr(input$scrollBarWheel, 1, 1)
    if (direction == 'l') shiftFrame('left', step = myPars$wheelScrollFactor)
    else if (direction == 'r') shiftFrame('right', step = myPars$wheelScrollFactor)
  }, ignoreInit = TRUE)

  observeEvent(input$zoomWheel, {
    direction = substr(input$zoomWheel, 1, 1)
    if (direction == 'l') changeZoom(1 / myPars$zoomFactor)
    else if (direction == 'r') changeZoom(myPars$zoomFactor, toCursor = TRUE)
  }, ignoreInit = TRUE)

  save_backup = function() {
    if (!is.null(myPars$ann)) {
      temp = soundgen:::rbind_fill(myPars$out, myPars$ann)
      temp = temp[order(temp$file, temp$from), ]
      write.csv(temp, backup_file, row.names = FALSE)
    }
  }

  done = function() {
    updateVTL()
    myPars$spectrum_hover = NULL
    myPars$spectrum_synth = NULL
    if (myPars$print) print('Running done()...')
    if (!is.null(myPars$ann)) {
      if (is.null(myPars$out)) {
        myPars$out = myPars$ann
      } else {
        idx = which(myPars$out$file == myPars$myAudio_filename)
        if (length(idx) > 0) myPars$out = myPars$out[-idx, ]
        myPars$out = soundgen:::rbind_fill(myPars$out, myPars$ann)
      }
      myPars$out_fTracks[[myPars$myAudio_filename]] = myPars$formantTracks
      myPars$out_specs[[myPars$myAudio_filename]] = myPars$spec
    }
    if (!is.null(myPars$out)) {
      myPars$out = myPars$out[order(myPars$out$file, myPars$out$from), ]
      save_backup()
    }
  }

  observeEvent(input$fileList, {
    done()
    myPars$n = which(myPars$fileList$name == input$fileList)
    reset()
    if (length(myPars$n) == 1 && myPars$n > 0) readAudioFile(myPars$n)
  }, ignoreInit = TRUE)

  nextFile = function() {
    if (is.null(myPars$myAudio_path)) return(invisible(NULL))
    done()
    if (myPars$n < myPars$nFiles) {
      myPars$n = myPars$n + 1
      updateSelectInput(session, 'fileList',
                        selected = myPars$fileList$name[myPars$n])
    }
  }
  observeEvent(input$nextFile, nextFile(), ignoreInit = TRUE)

  lastFile = function() {
    if (is.null(myPars$myAudio_path)) return(invisible(NULL))
    done()
    if (myPars$n > 1) {
      myPars$n = myPars$n - 1
      updateSelectInput(session, 'fileList',
                        selected = myPars$fileList$name[myPars$n])
    }
  }
  observeEvent(input$lastFile, lastFile(), ignoreInit = TRUE)

  output$saveRes = downloadHandler(
    filename = function() 'output.csv',
    content = function(filename) {
      done()
      # remove all-empty formant columns
      cols_rem = 6 + which(apply(myPars$out[, 7:ncol(myPars$out)], 2,
                                 function(x) all(is.na(x))))
      myPars$out = myPars$out[, -cols_rem]
      write.csv(myPars$out, filename, row.names = FALSE)
      if (file.exists(backup_file))
        file.remove(backup_file)
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

  observeEvent(input$terminate_no, removeModal(), ignoreInit = TRUE)

  observeEvent(input$terminate_yes, {
    settings_list = reactiveValuesToList(input)
    settings_list = lapply(settings_list, function(x)
      if(is.vector(x) && !is.null(names(x))) unname(x) else x)
    stopApp(returnValue = list(formants = myPars$out,
                               settings = settings_list))
  }, ignoreInit = TRUE)

  observeEvent(input$about, {
    if (myPars$debugQn) {
      browser()
    } else {
      showNotification(
        ui = paste0("App for measuring formants... soundgen ",
                    packageVersion('soundgen'), "..."),
        duration = 20, closeButton = TRUE, type = 'default'
      )
    }
  }, ignoreInit = TRUE)
}

