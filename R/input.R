#' Check audio input type
#'
#' Internal soundgen function.
#'
#' Checks the types of audio input to another function, which could be a folder
#' with audio files, a single file, a Wave object, or a numeric vector. The
#' purposes of this helper function are to ascertain that there are some valid
#' inputs and to make a list of valid audio files, if any.
#' @param x path to a .wav or .mp3 file, Wave object, or a numeric vector
#'   representing the waveform with specified samplingRate
#' @keywords internal
checkInputType = function(x) {
  if (is.character(x)) {
    # character means file or folder
    if (length(x) == 1 && dir.exists(x)) {
      # input is a folder
      x = dirname(paste0(x, '/arbitrary'))  # strips terminal '/', if any
      filenames = list.files(x, pattern = "*.wav|.mp3|.WAV|.MP3", full.names = TRUE)
      if (length(filenames) < 1)
        stop(paste('No wav/mp3 files found in', x))
    } else {
      # input is one or more audio files
      for (f in 1:length(x)) {
        if (!file.exists(x) ||
            !substr(x, nchar(x) - 3, nchar(x)) %in% c('.wav', '.mp3', '.WAV', '.MP3')) {
          stop('Input not recognized - must be a folder, wav/mp3 file(s), or numeric vector(s)')
        }
      }
      filenames = x
    }
    n = length(filenames)
    type = rep('file', n)
    filenames_base = filenames_noExt = basename(filenames)

    for (f in 1:n) {
      # strip extension
      filenames_noExt[f] = substr(filenames_base[f], 1, nchar(filenames_base[f]) - 4)
      filesizes = file.info(filenames)$size
      # expand from relative to full path (useful for functions that save audio
      # separately from plots)
      filenames[f] = normalizePath(filenames[f])
    }
  } else {
    # not file(s), but one or more objects (Wave / numeric)
    if (!is.list(x)) x = list(x)
    n = length(x)
    if (n == 1) {
      filenames_base = filenames_noExt = 'sound'
    } else {
      filenames_base = filenames_noExt = paste0('sound', 1:n)
    }
    filenames = NULL
    filesizes = NULL
    type = rep(NA, n)
    for (i in 1:n) {
      if (is.numeric(x[[i]]) | is.logical(x[[i]])) {
        type[i] = 'vector'  # logical means all NAs
      } else if (class(x[[i]]) == 'Wave') {
        # input is a Wave object
        type[i] = 'Wave'
      } else {
        stop(paste('Input not recognized - must be a folder, wav/mp3 file,',
                   'Wave object, or numeric vector'))
      }
    }
  }
  return(list(
    type = type,
    n = n,
    filenames = filenames,
    filenames_base = filenames_base,
    filenames_noExt = filenames_noExt,
    filesizes = filesizes
  ))
}


#' Read audio
#'
#' Internal soundgen function.
#'
#' @param x audio input (only used for Wave objects or numeric vectors)
#' @param input a list returned by \code{\link{checkInputType}}
#' @param i iteration
#' @param samplingRate sampling rate of \code{x} (only needed if \code{x} is a
#'   numeric vector, rather than an audio file or Wave object)
#' @param scale maximum possible amplitude of input used for normalization of
#'   input vector (only needed if \code{x} is a numeric vector, rather than an
#'   audio file or Wave object)
#' @param from,to if NULL (default), analyzes the whole sound, otherwise
#'   from...to (s)
#' @keywords internal
readAudio = function(x,
                     input = checkInputType(x),
                     i,
                     samplingRate = NULL,
                     scale = NULL,
                     from = NULL,
                     to = NULL) {
  failed = FALSE
  if (input$type[i] == 'file') {
    fi = input$filenames[i]
    ext_i = substr(fi, nchar(fi) - 3, nchar(fi))
    if (ext_i %in% c('.wav', '.WAV')) {
      sound_wave = try(tuneR::readWave(fi))
    } else if (ext_i %in% c('.mp3', '.MP3')) {
      sound_wave = try(tuneR::readMP3(fi))
    } else {
      warning(paste('Input', fi, 'not recognized: expected a wav/mp3 file'))
    }
    if (class(sound_wave) == 'try-error') {
      failed = TRUE
      sound = samplingRate = scale = NULL
    } else {
      sound = as.numeric(sound_wave@left)
      samplingRate = sound_wave@samp.rate
      scale = 2 ^ (sound_wave@bit - 1)
    }
  } else if (input$type[i] == 'vector') {
    if (is.null(samplingRate)) {
      samplingRate = 16000
      message('samplingRate not specified; defaulting to 16000')
    }
    sound = x
    m = max(abs(sound))
    if (is.null(scale)) {
      scale = max(m, 1)
      # message(paste('Scale not specified. Assuming that max amplitude is', scale))
    } else if (is.numeric(scale)) {
      if (scale < m) {
        scale = m
        warning(paste('Scale cannot be smaller than observed max;',
                      'resetting to', m))
      }
    }
  } else if (input$type[i] == 'Wave') {
    sound = x@left
    samplingRate = x@samp.rate
    scale = 2 ^ (x@bit - 1)
  }

  # from...to
  # from...to selection
  ls = length(sound)
  if (any(is.numeric(c(from, to)))) {
    if (!is.numeric(from)) {
      from_points = 1
    } else {
      from_points = max(1, round(from * samplingRate))
    }
    if (!is.numeric(to)) {
      to_points = ls
    }  else {
      to_points = min(ls, round(to * samplingRate))
    }
    sound = sound[from_points:to_points]
    timeShift = from_points / samplingRate
    ls = length(sound)
  } else {
    timeShift = 0
  }
  duration = ls / samplingRate

  return(list(
    sound = sound,
    samplingRate = samplingRate,
    scale = scale,
    failed = failed,
    ls = ls,
    duration = duration,
    timeShift = timeShift,
    filename = input$filenames[i],
    filename_base = input$filenames_base[i],
    filename_noExt = input$filenames_noExt[i]
  ))
}


#' Process audio
#'
#' Internal soundgen function.
#'
#' @inheritParams spectrogram
#' @param funToCall function to call (specify what to do with each audio input)
#' @param myPars a list of parameters to pass on to `funToCall`
#' @param summaryFun function(s) used to summarize the output per input
#' @param var_noSummary names of output variables that should not be summarized
#' @param reportEvery report estimated time left every ... iterations (NA = no
#'   reporting, NULL = default frequency)
#' @keywords internal
processAudio = function(x,
                        samplingRate = NULL,
                        scale = NULL,
                        from = NULL,
                        to = NULL,
                        funToCall,
                        myPars = list(),
                        var_noSummary = NULL,
                        reportEvery = NULL,
                        savePlots = NULL,
                        saveAudio = NULL) {
  input = checkInputType(x)
  input$failed = rep(FALSE, input$n)

  # savePlots
  if (is.character(savePlots)) {
    if (savePlots == '') {
      # same as the folder where the audio input lives
      if (input$type[1] == 'file') {
        savePlots = paste0(dirname(input$filenames[1]), '/')
      } else {
        savePlots = paste0(getwd(), '/')
      }
    } else {
      # make sure the last character of savePath is "/" and expand ~
      savePlots = paste0(
        dirname(paste0(savePlots, '/arbitrary')),
        '/'
      )
    }
    if (!dir.exists(savePlots)) dir.create(savePlots)
  } else {
    savePlots = NULL
  }
  input$savePlots = savePlots  # to pass on to top function like analyze()

  # saveAudio
  if (is.character(saveAudio)) {
    if (saveAudio == '') {
      # same as the folder where the audio input lives
      keypr = readline(prompt = paste(
        "NB: saveAudio='' will overwrite the originals. Proceed? (yes/no) "))
      if (substr(keypr, 1, 1) != 'y') stop('Aborting...')
      if (input$type[1] == 'file') {
        saveAudio = paste0(dirname(input$filenames[1]), '/')
      } else {
        saveAudio = paste0(getwd(), '/')
      }
    } else {
      # make sure the last character of savePath is "/" and expand ~
      saveAudio = paste0(
        dirname(paste0(saveAudio, '/arbitrary')),
        '/'
      )
    }
    if (!dir.exists(saveAudio)) dir.create(saveAudio)
  } else {
    saveAudio = NULL
  }
  input$saveAudio = saveAudio  # to pass on to top function like analyze()

  result = vector('list', input$n)
  names(result) = input$filenames_base
  if (input$type[1] == 'file') x = rep(list(NULL), input$n)
  if (!is.list(x)) x = list(x)
  time_start = proc.time()  # timing
  for (i in 1:input$n) {
    audio = readAudio(x[[i]], input, i,
                      samplingRate = samplingRate,
                      scale = scale,
                      from = from, to = to)
    # to pass savePlots and saveAudio on to funToCall without adding extra
    # args, put them in "audio"
    audio$savePlots = savePlots
    audio$saveAudio = saveAudio

    # analyze file
    if (!audio$failed) {
      an_i = try(do.call(funToCall, c(list(audio = audio), myPars)))
      if (class(an_i)[1] == 'try-error') audio$failed = TRUE
    }
    if (audio$failed) {
      if (input$n > 1) {
        warning(paste('Failed to process file', input$filenames[i]))
      } else {
        warning('Failed to process the input')
      }
      an_i = numeric(0)
      input$failed[i] = TRUE
    }
    result[[i]] = an_i

    # report time
    if ((is.null(reportEvery) || is.finite(reportEvery)) & input$n > 1) {
      reportTime(i = i, nIter = input$n, reportEvery = reportEvery,
                 time_start = time_start, jobs = input$filesizes)
    }
  }

  return(list(
    input = input,
    result = result
  ))
}
