#' Play audio
#'
#' Plays one or more sounds: wav/mp3 file(s), Wave objects, or numeric vectors.
#' This is a simple wrapper for the functionality provided by
#' \code{\link[tuneR]{play}}. Recommended players on Linux: "play" from the
#' "vox" library (default), "aplay".
#' @inheritParams spectrogram
#' @param player the name of player to use, eg "aplay", "play", "vlc", etc.
#'   Defaults to "play" on Linux, "afplay" on MacOS, and tuneR default on
#'   Windows. In case of errors, try setting another default player for
#'   \code{\link[tuneR]{play}}
#' @param from,to play a selected time range (s)
#' @export
#' @examples
#' \dontrun{
#' # Play an audio file:
#' playme('pathToMyAudio/audio.wav')
#'
#' # Create and play a numeric vector:
#' f0_Hz = 440
#' sound = sin(2 * pi * f0_Hz * (1:16000) / 16000)
#' playme(sound, 16000)
#' playme(sound, 16000, from = .1, to = .5)  # play from 100 to 500 ms
#'
#' # In case of errors, look into tuneR::play(). For ex., you might need to
#' # specify which player to use:
#' playme(sound, 16000, player = 'aplay')
#'
#' # To avoid doing it all the time, set the default player:
#' tuneR::setWavPlayer('aplay')
#' playme(sound, 16000)  # should now work without specifying the player
#' }
playme = function(x,
                  samplingRate = 16000,
                  player = NULL,
                  from = NULL,
                  to = NULL) {
  if (is.null(player)) {
    if (!is.null(tuneR::getWavPlayer())) {
      player = tuneR::getWavPlayer()
    } else {
      # try to guess what player to use
      os = Sys.info()[['sysname']]
      if (os == 'Linux' | os == 'linux') {
        player = 'play'
      } else if (os == 'Darwin' | os == 'darwin') {
        player = 'afplay'
      } else {
        # a good default on windows?
      }
    }
  }

  # check input type
  input = checkInputType(x)
  if (input$type[1] == 'file') x = rep(list(NULL), input$n)
  if (!is.list(x)) x = list(x)

  # play each input
  for (i in seq_along(x)) {
    # make input i into a Wave object
    if (input$type[i] == 'file') {
      fi = input$filenames[i]
      ext_i = substr(fi, nchar(fi) - 2, nchar(fi))
      if (ext_i %in% c('wav', 'WAV')) {
        sound_wave = try(tuneR::readWave(fi))
      } else if (ext_i %in% c('mp3', 'MP3')) {
        sound_wave = try(tuneR::readMP3(fi))
      } else {
        warning(paste('Input', fi, 'not recognized: expected a wav/mp3 file'))
        invisible()
      }
    } else if (input$type[i] == 'vector') {
      sound_wave = tuneR::Wave(
        left = x[[i]],
        samp.rate = samplingRate,
        bit = 16,
        pcm = TRUE
      )
      sound_wave = tuneR::normalize(sound_wave, unit = '32') # / 2
    } else if (input$type[i] == 'Wave') {
      sound_wave = x[[i]]
    }

    if (!inherits(sound_wave, 'try-error')) {
      # select time range
      if (!is.null(from) | !is.null(to)) {
        if (is.null(from)) from = 0
        if (is.null(to)) to = length(sound_wave@left) / sound_wave@samp.rate
        sound_wave = tuneR::extractWave(object = sound_wave,
                                        from = from,
                                        to = to,
                                        interact = FALSE,
                                        xunit = 'time')
      }
      # if shorter than 100 ms, pad with 100 ms of silence (otherwise doesn't play)
      if (length(sound_wave@left) / sound_wave@samp.rate < .1)
        sound_wave@left = c(sound_wave@left, rep(0, .1 * sound_wave@samp.rate))
      p = tuneR::play(sound_wave, player = player)
      if (p > 0) {  # error in sh
        warning(paste0(
          "Error in tuneR::play. Try setting the default audio player,",
          "eg tuneR::setWavPlayer('aplay'). See http://music.informatics.",
          "indiana.edu/courses/I546/tuneR_play.pdf"))
      }
    }
  }
  # can't get rid of printed output! sink(), capture.output, invisible don't work!!!
}


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
  file_sep = .Platform$file.sep
  if (is.character(x)) {
    # character means file or folder
    if (length(x) == 1 && dir.exists(x)) {
      # input is a folder
      x = dirname(paste0(x, file_sep, 'arbitrary'))  # strips terminal file_sep, if any
      filenames = list.files(x, pattern = "*.wav|.mp3|.WAV|.MP3", full.names = TRUE)
      if (length(filenames) < 1)
        stop(paste('No wav/mp3 files found in', x))
    } else {
      # input is one or more audio files
      for (f in seq_along(x)) {
        if (!file.exists(x[f]) ||
            !substr(x[f], nchar(x[f]) - 3, nchar(x[f])) %in% c('.wav', '.mp3', '.WAV', '.MP3')) {
          stop('Input not recognized - must be a folder, wav/mp3 file(s), or numeric vector(s)')
        }
      }
      filenames = x
    }
    n = length(filenames)
    type = rep('file', n)
    filesizes = file.info(filenames)$size
    filenames_base = basename(filenames)
    # avoid dependency with xfun::sans_ext(filenames_base)
    filenames_noExt = sub("^(.*?)([.](([[:alnum:]]+|tar[.](gz|bz2|xz)|nb[.]html)[~#]?))$",
                          "\\1", filenames_base)
    # expand from relative to full path (useful for functions that save audio
    # separately from plots)
    filenames = normalizePath(filenames)
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
      } else if (inherits(x[[i]], 'Wave')) {
        # input is a Wave object
        type[i] = 'Wave'
      } else {
        stop(paste('Input not recognized - must be a folder, wav/mp3 file,',
                   'Wave object, or numeric vector'))
      }
    }
  }
  list(
    type = type,
    n = n,
    filenames = filenames,
    filenames_base = filenames_base,
    filenames_noExt = filenames_noExt,
    filesizes = filesizes
  )
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
  right = NULL
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
    if (inherits(sound_wave, 'try-error')) {
      failed = TRUE
      sound = samplingRate = bit = scale = NULL
    } else {
      sound = as.numeric(sound_wave@left)
      right = as.numeric(sound_wave@right)
      samplingRate = sound_wave@samp.rate
      bit = sound_wave@bit
      scale = 2 ^ (sound_wave@bit - 1)
    }
  } else if (input$type[i] == 'vector') {
    if (is.null(samplingRate)) {
      samplingRate = 16000
      message('samplingRate not specified; defaulting to 16000')
    }
    sound = x
    m = suppressWarnings(max(abs(sound), na.rm = TRUE))
    bit = 1
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
    right = x@right
    samplingRate = x@samp.rate
    bit = x@bit
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
      if (from_points > ls) stop('Invalid from - greater than sound duration')
    }
    if (!is.numeric(to)) {
      to_points = ls
    }  else {
      to_points = min(ls, round(to * samplingRate))
    }
    sound = sound[from_points:to_points]
    right = right[from_points:to_points]
    timeShift = from_points / samplingRate
    ls = length(sound)
  } else {
    timeShift = 0
  }
  duration = ls / samplingRate

  list(
    sound = sound,
    right = right,
    samplingRate = samplingRate,
    bit = bit,
    scale = scale,
    scale_used = max(abs(range(sound))),
    failed = failed,
    ls = ls,
    duration = duration,
    timeShift = timeShift,
    filename = input$filenames[i],
    filename_base = input$filenames_base[i],
    filename_noExt = input$filenames_noExt[i]
  )
}


#' Write audio
#'
#' Internal soundgen function.
#'
#' Writes a .wav file to disk based on the bit/scale/samplingRate contained in
#' the internally generated \code{audio} object. The point with using this
#' function is to package tuneR::Wave + normalize + writeWave in a
#' soundgen-specific way. Unlike seewave::savewav, writeAudio does NOT normalize
#' or rescale the input.
#' @param x numeric vector
#' @param scale_used actually used scale (max(abs(x))) - overrides
#'   audio$scale_used
#' @param audio list returned by \code{\link{readAudio}} containing
#'   samplingRate, bit, scale, scale_used
#' @param filename full path and filename including .wav
#' @keywords internal
writeAudio = function(x, audio, filename, scale_used = NULL) {
  x_wave = tuneR::Wave(left = x, samp.rate = audio$samplingRate, bit = 16)
  if (is.null(scale_used)) scale_used = audio$scale_used
  x_wave_norm = tuneR::normalize(
    x_wave,
    unit = if (audio$bit == 1) '16' else as.character(audio$bit),
    level = scale_used / audio$scale,
    center = FALSE,
    rescale = TRUE
  )
  tuneR::writeWave(
    x_wave_norm,
    filename = filename
  )
}


#' Process audio
#'
#' Internal soundgen function.
#'
#' @inheritParams spectrogram
#' @param funToCall function to call (specify what to do with each audio input)
#' @param myPars a list of parameters to pass on to `funToCall`
#' @param var_noSummary names of output variables that should not be summarized
#' @keywords internal
#' @examples
#' \dontrun{
#' # How many cores should I use? Maybe ~4
#' a1 = analyze('~/Downloads/temp60/', cores = 1)  # 3:55
#' a2 = analyze('~/Downloads/temp60/', cores = 2, reportEvery = 100)  # 2:30
#' a3 = analyze('~/Downloads/temp60/', cores = 3, reportEvery = 100)  # 1:50
#' a4 = analyze('~/Downloads/temp60/', cores = 4, reportEvery = 100)  # 1:33
#' a7 = analyze('~/Downloads/temp60/', cores = 7, reportEvery = 100)  # 1:29
#' }
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
                        saveAudio = NULL,
                        cores = 1) {
  input = checkInputType(x)
  input$failed = rep(FALSE, input$n)
  file_sep = .Platform$file.sep

  # savePlots
  if (is.character(savePlots)) {
    if (savePlots == '') {
      # same as the folder where the audio input lives
      if (input$type[1] == 'file') {
        savePlots = paste0(dirname(input$filenames[1]), file_sep)
      } else {
        savePlots = paste0(getwd(), file_sep)
      }
    } else {
      # make sure the last character of savePath is "/" and expand ~
      savePlots = paste0(
        dirname(paste0(savePlots, file_sep, 'arbitrary')),
        file_sep
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
        saveAudio = paste0(dirname(input$filenames[1]), file_sep)
      } else {
        saveAudio = paste0(getwd(), file_sep)
      }
    } else {
      # make sure the last character of savePath is "/" and expand ~
      saveAudio = paste0(
        dirname(paste0(saveAudio, file_sep, 'arbitrary')),
        file_sep
      )
    }
    if (!dir.exists(saveAudio)) dir.create(saveAudio)
  } else {
    saveAudio = NULL
  }
  input$saveAudio = saveAudio  # to pass on to top function like analyze()
  if (input$type[1] == 'file') x = rep(list(NULL), input$n)
  if (!is.list(x)) x = list(x)

  if (cores > 1 & input$n > 1) {
    # parallel processing
    cl = parallel::makeCluster(cores, outfile = '') # set the number of cores to use
    # print(cl)
    doParallel::registerDoParallel(cl)
    # foreach::getDoParRegistered()
    # foreach::getDoParWorkers()
    time_start_global = proc.time()
    chunks = splitIntoChunks(1:input$n, cores)
    result = foreach::foreach(i = seq_along(chunks), .combine = 'c') %dopar% {
      # lapply(1:i, sqrt)  # fine unless we also need to report time
      time_start = proc.time()
      chunk = chunks[[i]]
      len_chunk = length(chunk)
      a = vector('list', len_chunk)
      for (t in 1:len_chunk) {
        audio = readAudio(x[[chunk[t]]], input, chunk[t],
                          samplingRate = samplingRate,
                          scale = scale,
                          from = from, to = to)
        # to pass savePlots and saveAudio on to funToCall without adding extra
        # args, put them in "audio"
        audio$savePlots = savePlots
        audio$saveAudio = saveAudio

        # process file
        if (!audio$failed) {
          an_t = try(do.call(funToCall, c(list(audio = audio), myPars)))
          if (inherits(an_t, 'try-error')) audio$failed = TRUE
        }
        if (audio$failed) {
          if (input$n > 1) {
            warning(paste('Failed to process file', input$filenames[t]))
          } else {
            warning('Failed to process the input')
          }
          an_t = numeric(0)
          # input$failed[chunk[t]] <<- TRUE  # crashes for some reason
        }
        # report time
        if ((is.null(reportEvery) || is.finite(reportEvery)) & input$n > 1) {
          reportTime(i = t, nIter = len_chunk, reportEvery = reportEvery,
                     time_start = time_start, jobs = input$filesizes[chunk],
                     prefix = paste0('Core ', i, ': '))
        }
        a[[t]] = an_t
      }
      a
    }
    parallel::stopCluster(cl)
    names(result) = input$filenames_base
    input$failed = (sapply(result, length) == 0)
    if ((is.null(reportEvery) || is.finite(reportEvery)) & input$n > 1) {
      soundgen::reportTime(i = input$n, nIter = input$n, time_start = time_start_global)
    }
  } else {
    # single core
    if (input$n > 5) {
      nCores = try(parallel::detectCores(), silent = TRUE)
      if (!inherits(nCores, 'try-error') && is.numeric(nCores) && nCores > 1) {
        msg = paste(
          "Consider using multiple cores to speed up processing with",
          "'cores = ...'. Your machine has", nCores, "cores"
        )
        message(msg)
      }
    }

    result = vector('list', input$n)
    names(result) = input$filenames_base
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

      # process file
      if (!audio$failed) {
        an_i = try(do.call(funToCall, c(list(audio = audio), myPars)))
        if (inherits(an_i, 'try-error')) audio$failed = TRUE
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

      # garbage collection to free RAM (doesn't really work, though, session still bloated)
      # gc()

      # report time
      if ((is.null(reportEvery) || is.finite(reportEvery)) & input$n > 1) {
        reportTime(i = i, nIter = input$n, reportEvery = reportEvery,
                   time_start = time_start, jobs = input$filesizes)
      }
    }
  }

  return(list(
    input = input,
    result = result
  ))
}


#' HTML for clickable plots
#'
#' Internal soundgen function
#'
#' Writes an html file for displaying clickable plots in a browser.
#' @inheritParams spectrogram
#' @param savePlots a list of full names of files (with paths and extensions)
#' @param changesAudio if TRUE, it means we modify the input audio, so the
#'   result shouldn't be saved in the original folder (if savePlots = '')
#' @param suffix an extra string to add before the extension, usually the name
#'   of the function that calls htmlPlots, eg "spectrogram"
#' @param extension file extension for the saved plots
#' @param width the default width of flex elements in html
#' @keywords internal
#' @examples
#' \dontrun{
#' ## Functions that modify audio
#' # relative paths
#' flatEnv('/home/allgoodguys/Downloads/temp', savePlots = '')
#' flatEnv('/home/allgoodguys/Downloads/temp',
#'   savePlots = '/home/allgoodguys/Downloads/temp/compr',
#'   saveAudio = '/home/allgoodguys/Downloads/temp/compr')
#'
#' # absolute paths
#' flatEnv('/home/allgoodguys/Downloads/temp', savePlots = '',
#'   saveAudio = '/home/allgoodguys/Downloads/temp/compr')
#' flatEnv('/home/allgoodguys/Downloads/temp',
#'   savePlots = '~/Downloads/temp/plots_compr')
#'
#'
#' ## Functions that only analyze audio
#' getRMS('/home/allgoodguys/Downloads/temp', savePlots = '')
#' getRMS('/home/allgoodguys/Downloads/temp',
#'   savePlots = '~/Downloads/temp/plots_rms')
#' }
htmlPlots = function(x,
                     savePlots,
                     changesAudio,
                     suffix,
                     extension = 'png',
                     width = "900px") {
  htmlFile = paste0(x$savePlots, '00_clickablePlots', '_', suffix, '.html')
  file_sep = .Platform$file.sep

  if (changesAudio) {
    # functions that modify and save audio shouldn't overwrite the original,
    # so '' doesn't put audio into the same folder
    if (is.null(x$saveAudio)) {
      # only plots saved - refer to original audio
      if (savePlots == '') {
        # relative paths for both audio and plots
        audioFiles = basename(x$filenames)
        plotFiles = paste0(x$filenames_noExt,
                           '_', suffix, '.', extension)
      } else {
        # absolute paths for both audio and plots
        audioFiles = normalizePath(x$filenames)
        plotFiles = normalizePath(paste0(x$savePlots, file_sep, x$filenames_noExt,
                                         '_', suffix, '.', extension))
      }
    } else {
      # audio also saved - refer to new audio
      if (x$savePlots == x$saveAudio) {
        # relative paths for both audio and plots (same folder)
        audioFiles = basename(x$filenames)
        plotFiles = paste0(x$filenames_noExt,
                           '_', suffix, '.', extension)
      } else {
        # absolute paths for both audio and plots (different folders)
        audioFiles = normalizePath(paste0(x$saveAudio, x$filenames_noExt, '.wav'))
        plotFiles = normalizePath(paste0(x$savePlots, file_sep, x$filenames_noExt,
                                         '_', suffix, '.', extension))
      }
    }
  } else {
    # functions that only analyze audio and save plots can write into ''
    x$savePlots = dirname(file.path(x$savePlots, 'smth'))
    # relative paths for plots ('smth' is a trick
    # because there may or may not be a / at the end)
    if (savePlots == '' | x$savePlots == dirname(x$filenames[1])) {
      plotFiles = paste0(x$filenames_noExt, '_', suffix, '.', extension)
      audioFiles = x$filenames_base
    } else {
      # absolute paths for plots
      plotFiles = normalizePath(paste0(x$savePlots, file_sep, x$filenames_noExt,
                                       '_', suffix, '.', extension))
      audioFiles = normalizePath(x$filenames)
    }
  }
  plotFiles_concat = paste0(plotFiles, collapse = "', '")
  audioFiles_concat = paste0(audioFiles, collapse = "', '")

  # create an html file to display nice, clickable spectrograms
  out_html = file(htmlFile)
  writeLines(
    c("<!DOCTYPE html>",
      "<html>",
      "<head>",
      "<title>Click to play</title>",
      "<meta charset='UTF-8'>",
      "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
      "<style>",
      "#flexbox {",
      "  display: flex;",
      "  flex-flow: row wrap;",
      "  justify-content: space-around;",
      "  align-items: stretch;",
      "}",
      "#flexbox > div {",
      paste0("  flex: 1 1 ", width, ";"),
      "  margin: 20px 5px;",
      "  border: 1px gray solid;",
      "  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 15px 0 rgba(0, 0, 0, 0.19);",
      "}",
      "#flexbox > div:hover{",
      "  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.4), 0 6px 15px 0 rgba(0, 0, 0, 0.3);",
      "}",
      "#flexbox img {",
      "  width: 100%;",
      "}",
      "</style>",
      "</head>",
      "<body>",
      "<div id='flexbox'> </div>",
      "<script>",
      paste0("var plotList = ['", plotFiles_concat, "'];"),
      paste0("var audioList = ['", audioFiles_concat, "'];"),
      "var flex = document.getElementById('flexbox');",
      "for (var i = 0; i < plotList.length; i++) {",
      "  let newDiv = document.createElement('div');",
      "  newDiv.innerHTML = '<img src=\"' + plotList[i] + '\">';",
      "  flex.appendChild(newDiv);",
      "  var mysound = audioList[i];",
      "  newDiv.onclick = (function(mysound) {",
      "    return function() {",
      "      var audioElement = document.createElement('audio');",
      "      audioElement.setAttribute('src', mysound);",
      "      audioElement.play();",
      "    };",
      "  })(mysound);",
      "}",
      "</script>",
      "</body>",
      "</html>"),
    out_html)
  close(out_html)
}
