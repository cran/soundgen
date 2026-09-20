#' Play audio
#'
#' Plays one or more sounds: wav/mp3 file(s), Wave objects, or numeric vectors.
#' Recommended players on Linux: "play" from the "SoX" library (default),
#' "aplay" (ALSA).
#' @inheritParams .roxygen_defaults
#' @param player the name of player to use, eg "aplay", "play", "vlc", etc.
#'   Defaults to options()$soundgen.player --> tuneR::getWavPlayer() -->
#'   OS-specific defaults: "play" on Linux, "afplay" on MacOS, and VLC -->
#'   FFmpeg --> PowerShell on Windows. In case of errors, try setting another
#'   default player (see examples).
#' @param from,to play a selected time range (s)
#' @return Does not return anything, only plays the audio.
#' @export
#' @examples
#' \dontrun{
#' # Play an audio file:
#' data('speechEx', package = 'soundgen')
#' playme(speechEx, from = 1, to = 2)
#' playme('pathToMyAudio/audio.wav')
#'
#' # Create and play a numeric vector:
#' f0_Hz = 440
#' sound = sin(2 * pi * f0_Hz * (1:16000) / 16000)
#' playme(sound, 16000)
#' playme(sound, 16000, from = .1, to = .5)  # play from 100 to 500 ms
#'
#' # Set default player for the current R session
#' # "play" (SoX library, default on Linux)
#' options(soundgen.player = 'play')
#' # "afplay" (ALSA, default on MacOS)
#' options(soundgen.player = 'afplay')
#' # VLC path (default on Windows)
#' options(soundgen.player = "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe")
#' # view currently set player
#' options()$soundgen.player
#'
#' # Or specify player per call
#' playme(sound, player = "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe")
#' }
playme = function(x,
                  samplingRate = NULL,
                  player = NULL,
                  from = NULL,
                  to = NULL) {
  if (is.null(samplingRate)) {
    samplingRate = 16000
    message('samplingRate not specified; defaulting to 16000')
  }
  os = tolower(Sys.info()[['sysname']])
  if (!is.null(player) && player == 'TRUE' || isTRUE(player)) player = NULL
  if (is.null(player)) {
    user_def = options()$soundgen.player
    if (!is.null(user_def)) {
      player = user_def
    } else if (!is.null(tuneR::getWavPlayer())) {
      player = tuneR::getWavPlayer()
    } else {
      # try to guess what player to use
      if (os == 'linux') {
        player = 'play'
      } else if (os == 'darwin') {
        player = 'afplay'
      } else {
        # windows: keep NULL for now, several fallback plans below...
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
      ext_i = tolower(tools::file_ext(fi))
      if (ext_i == 'wav') {
        sound_wave = try(tuneR::readWave(fi))
      } else if (ext_i == 'mp3') {
        sound_wave = try(tuneR::readMP3(fi))
      } else {
        warning(paste('Input', fi, 'not recognized: expected a wav/mp3 file'))
        next
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
      if (!is.null(from) || !is.null(to)) {
        if (is.null(from)) from = 0
        if (is.null(to)) to = length(sound_wave@left) / sound_wave@samp.rate
        sound_wave = tuneR::extractWave(object = sound_wave,
                                        from = from,
                                        to = to,
                                        interact = FALSE,
                                        xunit = 'time')
      }
      # if shorter than 100 ms, pad with 100 ms of silence (otherwise doesn't play)
      minLen = round(.1 * sound_wave@samp.rate)
      if (length(sound_wave@left) < minLen) {
        pad = rep(0, minLen)
        sound_wave@left = c(sound_wave@left, pad)
        if (sound_wave@stereo) sound_wave@right = c(sound_wave@right, pad)
      }

      # Save to temporary file
      temp_wav = tempfile(fileext = ".wav")
      tuneR::writeWave(sound_wave, temp_wav)

      # Build platform-appropriate command
      if (os == "Windows") {
        # Windows-specific handling
        if (is.null(player)) {
          # Try VLC first, then FFplay, then fall back to PowerShell
          vlc_path = "C:\\Program Files\\VideoLAN\\VLC\\vlc.exe"
          ffplay_path = "C:\\ffmpeg\\bin\\ffplay.exe"

          if (file.exists(vlc_path)) {
            player_cmd = vlc_path
            player_args = c("--intf", "dummy", "--play-and-exit", temp_wav)
          } else if (file.exists(ffplay_path)) {
            player_cmd = ffplay_path
            player_args = c("-nodisp", "-autoexit", temp_wav)
          } else {
            # Fallback: PowerShell method
            ps_command = sprintf(
              "Start-Process -FilePath '%s' -ArgumentList '%s' -WindowStyle Hidden",
              temp_wav, shQuote(temp_wav)
            )
            system2(
              "powershell",
              args = c("-Command", ps_command),
              stdout = FALSE, stderr = FALSE, wait = TRUE
            )
          }
        } else {
          # User-specified player
          player_cmd = player
          player_args = temp_wav
        }

        # Execute with output suppression
        system2(
          command = player_cmd,
          args = player_args,
          stdout = FALSE, stderr = FALSE, wait = TRUE
        )
      } else {
        # Linux/macOS handling
        cmd = sprintf('sh -c "%s \'%s\' > /dev/null 2>&1"', player, temp_wav)

        # Execute with system2 and explicit suppression
        system2("bash", args = c("-c", shQuote(cmd)),
                stdout = FALSE, stderr = FALSE, wait = TRUE)
      }

      # Alternatively, just call tuneR::play(), but then can't get rid of
      # printed output, and problems on Windows
      # p = tuneR::play(sound_wave, player = player)
      # if (p > 0) {  # error in sh
      #   warning(paste0(
      #     "Error in tuneR::play. Try setting the default audio player,",
      #     "eg tuneR::setWavPlayer('aplay'). See http://music.informatics.",
      #     "indiana.edu/courses/I546/tuneR_play.pdf"))
      # }
    }
  }
  return(invisible())
}


#' Check audio input type
#'
#' Checks the types of audio input to another function, which could be a folder
#' with audio files, a single file, a Wave object, or a numeric vector. The
#' purposes of this helper function are to ascertain that there are some valid
#' inputs and to make a list of valid audio files, if any.
#' @param x path to a folder, .wav or .mp3 file, Wave object, or a numeric vector
#'   representing the waveform with specified samplingRate
#' @return A list of meta-info needed to make the "audio" object for dot-functions.
#' @noRd
#' @examples
#' soundgen:::checkInputType(rnorm(10))
#' soundgen:::checkInputType(list(a = rnorm(10), b = runif(10)))
#' soundgen:::checkInputType('~/Downloads/temp')
#' try(soundgen:::checkInputType(getwd()))
#' try(soundgen:::checkInputType('blabla'))
checkInputType = function(x) {
  # file_sep = .Platform$file.sep
  if (is.character(x)) {
    # character means file or folder
    if (length(x) == 1 && dir.exists(x)) {
      # input is a folder
      x = dirname(file.path(x, 'arbitrary'))  # strips terminal file_sep, if any
      filenames = list.files(
        x,
        pattern = "\\.(wav|mp3|WAV|MP3)$",
        full.names = TRUE,
        ignore.case = TRUE
      )
      filenames = filenames[!file.info(filenames)$isdir %in% TRUE]
      if (length(filenames) < 1)
        stop(paste('No wav/mp3 files found in', x))
    } else {
      # input is one or more audio files
      ext = tolower(tools::file_ext(x))
      invalid = !file.exists(x) | dir.exists(x) | !ext %in% c("wav", "mp3")
      if (any(invalid)) {
        stop("Invalid audio input(s): ", paste(x[invalid], collapse = ", "))
      }
      filenames = x
    }
    n = length(filenames)
    if (n == 0) stop("No audio inputs supplied")
    type = rep('file', n)
    # expand from relative to full path (useful for functions that save audio
    # separately from plots)
    filenames = normalizePath(filenames, mustWork = FALSE)
    filesizes = file.info(filenames)$size
    filenames_base = basename(filenames)
    filenames_noExt = tools::file_path_sans_ext(filenames_base)
  } else {
    # not file(s), but one or more objects (Wave / numeric)
    if (!is.list(x)) x = list(x)
    n = length(x)
    nm = names(x)
    if (!is.null(nm)) {
      nm = trimws(nm)
      nm[!nzchar(nm) | is.na(nm)] = paste0("sound", seq_len(n))[!nzchar(nm) | is.na(nm)]
      filenames_base = filenames_noExt = make.unique(nm)
    } else {
      filenames_base = filenames_noExt = paste0("sound", seq_len(n))
    }
    filenames = NULL
    filesizes = NULL
    type = rep(NA, n)
    for (i in seq_len(n)) {
      if (is.numeric(x[[i]]) || is.logical(x[[i]])) {
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
#' @noRd
#' @return A list passed as the "audio" object to dot-functions.
readAudio = function(x,
                     input = checkInputType(x),
                     i = 1,
                     samplingRate = NULL,
                     scale = NULL,
                     from = NULL,
                     to = NULL) {
  return_fail = list(
    sound = NULL,
    right = NULL,
    samplingRate = NULL,
    bit = NULL,
    scale = NULL,
    scale_used = NULL,
    failed = TRUE,
    ls = NULL,
    duration = NULL,
    timeShift = NULL,
    filename = NULL,
    filename_base = NULL,
    filename_noExt = NULL
  )

  failed = FALSE
  right = NULL
  if (!is.null(samplingRate) && length(samplingRate) > 1)
    samplingRate = samplingRate[1]
  if (input$type[i] == 'file') {
    fi = input$filenames[i]
    ext_i = tolower(tools::file_ext(fi))
    if (ext_i == 'wav') {
      sound_wave = try(tuneR::readWave(fi))
    } else if (ext_i == 'mp3') {
      sound_wave = try(tuneR::readMP3(fi))
    } else {
      warning(paste('Input', fi, 'not recognized: expected a wav/mp3 file'))
      return(return_fail)
    }
    if (inherits(sound_wave, 'try-error')) {
      return(return_fail)
    } else {
      sound = as.numeric(sound_wave@left)
      right = as.numeric(sound_wave@right)
      samplingRate = sound_wave@samp.rate
      bit = sound_wave@bit
      # warning: assumes signed ampl ±, but 8-bit audio may be [0, 256]
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
    } else {
      stop('scale must be either NULL or numeric')
    }
  } else if (input$type[i] == 'Wave') {
    sound = as.numeric(x@left)
    right = as.numeric(x@right)
    samplingRate = x@samp.rate
    bit = x@bit
    scale = 2 ^ (x@bit - 1)
  } else {
    return(return_fail)
  }

  # from...to
  # from...to selection
  ls = length(sound)
  if (ls == 0) failed = TRUE
  if (any(is.numeric(c(from, to)))) {
    if (isTRUE(is.finite((from)))) {
      from_points = max(1, round(from * samplingRate))
      if (from_points > ls) stop('Invalid from - greater than sound duration')
    } else {
      from_points = 1
    }
    if (isTRUE(is.finite((to)))) {
      to_points = min(ls, round(to * samplingRate))
    }  else {
      to_points = ls
    }
    if (!(to_points > from_points)) stop('"from" must be smaller than "to"')
    sound = sound[from_points:to_points]
    right = right[from_points:to_points]
    timeShift = (from_points - 1) / samplingRate
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
    scale_used = max(abs(range(sound, na.rm = TRUE))),
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
#' Writes a .wav file to disk as 16-bit audio based on the scale/samplingRate
#' contained in the internally generated \code{audio} object. The point with
#' using this function is to package tuneR::Wave + normalize + writeWave in a
#' soundgen-specific way. Unlike seewave::savewav, writeAudio does NOT normalize
#' or rescale the input.
#' @param x numeric vector
#' @param scale_used actually used scale (max(abs(x))) - overrides
#'   audio$scale_used
#' @param audio list returned by \code{\link{readAudio}} containing
#'   samplingRate, bit, scale, scale_used
#' @param filename full path and filename including .wav
#' @noRd
writeAudio = function(x, audio, filename, scale_used = NULL) {
  x_wave = tuneR::Wave(left = x, samp.rate = audio$samplingRate, bit = 16)
  if (is.null(scale_used)) scale_used = audio$scale_used
  if (scale_used != 0)
    x_wave = tuneR::normalize(
      x_wave,
      unit = '16', # if (audio$bit == 1) '16' else as.character(audio$bit),
      level = scale_used / audio$scale,
      center = FALSE,
      rescale = TRUE
    )
  tuneR::writeWave(
    x_wave,
    filename = filename,
    extensible = FALSE
  )
}


#' Process audio
#' @param funToCall function to call (specify what to do with each audio input)
#' @param myPars a list of parameters to pass on to `funToCall`
#' @noRd
#' @return A list of inputs and results.
processAudio = function(x,
                        samplingRate = NULL,
                        scale = NULL,
                        from = NULL,
                        to = NULL,
                        funToCall,
                        myPars = list(),
                        reportEvery = NULL,
                        savePlots = FALSE,
                        saveAudio = FALSE,
                        suffix = 'newdir',
                        cores = 1) {
  input = checkInputType(x)
  if (input$n == 0) return(list(input = input, result = NA))
  funToCall = match.fun(funToCall)
  input$failed = rep(FALSE, input$n)
  # exclude the pars passed on to htmlPlots (they shouldn't be here, but just in case)
  myPars = myPars[which(!names(myPars) %in% c('savePlots', 'saveAudio', 'embed', '...'))]

  # dir in which to save the plots and modified audio (if any)
  # NB: functions that modify and save audio shouldn't overwrite the original,
  # so the audio and plots are always put to a new folder called {suffix}
  if (input$type[1] == 'file') {
    x = rep(list(NULL), input$n)
    path = dirname(input$filenames[1])
  } else {
    path = getwd()
  }
  input$path_output = file.path(path, suffix)

  if (is.character(savePlots)) {
    savePlots = TRUE
    warning('savePlots must be logical; resetting to TRUE')
  }
  savePlots = isTRUE(savePlots)

  if (is.character(saveAudio)) {
    saveAudio = TRUE
    warning('saveAudio must be logical; resetting to TRUE')
  }
  saveAudio = isTRUE(saveAudio)

  if ((savePlots || saveAudio) && !dir.exists(input$path_output))
    dir.create(input$path_output)
  if (!is.list(x)) x = list(x)

  if (cores > 1 && input$n > 1) {
    time_start_global = proc.time()
    chunks = splitIntoChunks(input$n, cores)

    # Create a PSOCK cluster – should work both on Unix and on Windows OSs
    cl = parallel::makeCluster(cores, outfile = '')
    on.exit(parallel::stopCluster(cl))

    # Make sure workers have access to all needed functions and objects
    parallel::clusterExport(cl, c(
      "x", "samplingRate", "scale", "from", "to", "funToCall", "myPars",
      "input", "chunks", "savePlots", "saveAudio", "reportEvery"),
      envir = environment())
    parallel::clusterEvalQ(cl, library(soundgen))  # the rest of soundgen

    result = parallel::parLapply(cl, seq_along(chunks), function(i) {
      time_start = proc.time()
      chunk = chunks[[i]]
      len_chunk = length(chunk)
      if (len_chunk == 0) return()
      a = vector('list', len_chunk)
      for (t in seq_len(len_chunk)) {
        audio = try(readAudio(x[[chunk[t]]], input, chunk[t],
                              samplingRate = samplingRate,
                              scale = scale,
                              from = from, to = to))
        if (inherits(audio, 'try-error')) {
          an_t = 'failed'
          # input$failed[chunk[t]] = TRUE # overwritten anyway
        } else {
          audio$path_output = input$path_output
          audio$savePlots = savePlots
          audio$saveAudio = saveAudio

          if (!audio$failed) {
            an_t = try(do.call(funToCall, c(list(audio = audio), myPars)))
            if (inherits(an_t, 'try-error')) {
              audio$failed = TRUE
            }
          }
          if (audio$failed) {
            if (input$n > 1) {
              warning(paste('Failed to process file', input$filenames[chunk[t]]))
            } else {
              warning('Failed to process the input')
            }
            an_t = 'failed' # numeric(0)
          }
        }

        # Real‑time progress – visible because outfile=''
        if ((is.null(reportEvery) || is.finite(reportEvery)) &&
            !isFALSE(reportEvery) && input$n > 1) {
          reportTime(i = t, nIter = len_chunk, reportEvery = reportEvery,
                     time_start = time_start, jobs = input$filesizes[chunk],
                     prefix = paste0('Core ', i, ': '))
        }
        if (!is.null(an_t)) a[[t]] = an_t
      }
      a
    })

    # Flatten list of lists
    result = do.call(c, result)
    names(result) = input$filenames_base
    input$failed = vapply(result,
                          function(x) is.character(x) &&
                            length(x) == 1 && x == 'failed',
                          logical(1))
    if ((is.null(reportEvery) || is.finite(reportEvery)) &&
        !isFALSE(reportEvery) && input$n > 1) {
      reportTime(i = input$n, nIter = input$n,
                 time_start = time_start_global)
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
    for (i in seq_len(input$n)) {
      audio = try(readAudio(x[[i]], input, i,
                            samplingRate = samplingRate,
                            scale = scale,
                            from = from, to = to))
      if (inherits(audio, 'try-error')) {
        an_i = 'failed'
        input$failed[i] = TRUE
      } else {
        # to pass the desired path to output on to funToCall without adding
        # extra args, put them in "audio"
        audio$path_output = input$path_output
        audio$savePlots = savePlots
        audio$saveAudio = saveAudio

        # process file
        if (!audio$failed) {
          an_i = try(do.call(funToCall, c(list(audio = audio), myPars)))
          if (inherits(an_i, 'try-error')) {
            audio$failed = TRUE
          }
        }
        if (audio$failed) {
          if (input$n > 1) {
            warning(paste('Failed to process file', input$filenames[i]))
          } else {
            warning('Failed to process the input')
          }
          an_i = 'failed' # numeric(0)
          input$failed[i] = TRUE
        }
      }
      if (!is.null(an_i)) result[[i]] = an_i

      # report time
      if ((is.null(reportEvery) || is.finite(reportEvery)) &&
          !isFALSE(reportEvery) && input$n > 1) {
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
#' Writes an html file for displaying clickable plots in a browser. Only works
#' for audio files, not a list of vectors, and requires that all files should
#' have unique basenames (e.g., not "1.wav" and "1.mp3").
#' @param audio a list returned by \code{\link{checkInputType}}
#' @param changesAudio if TRUE, it means we modify the input audio
#' @param suffix some label for the function that calls htmlPlots
#' @param extension file extension for the saved plots
#' @param width the default width of flex elements in html
#' @param embed if TRUE, plots and audio are embedded in html for easy sharing
#' @noRd
#' @return Does not return anything, just writes an html file.
htmlPlots = function(audio,
                     changesAudio = FALSE,
                     suffix = basename(audio$path_output),
                     extension = 'png',
                     width = "900px",
                     embed = FALSE) {
  if (embed) {
    # check if package "base64enc" is available
    if (!requireNamespace("base64enc", quietly = TRUE))
      stop('Please install the "base64enc" package to embed plots and audio: ',
           '`install.packages("base64enc")`')
  }

  # normalize extension to avoid, e.g., "..png"
  extension = tryCatch(as.character(extension), error = function(e) 'png')
  if (length(extension) == 0 || is.na(extension[1]) || !nzchar(trimws(extension[1]))) {
    extension = 'png'
  } else {
    extension = tolower(trimws(extension[1]))
  }
  extension = sub("^\\.+", "", extension)
  if (!nzchar(extension)) extension = 'png'

  mime_image = function(ext) {
    if (length(ext) != 1 || is.na(ext)) return('image/png')
    ext = tolower(trimws(ext))
    ext = sub("^\\.+", "", ext)
    if (!nzchar(ext)) return('image/png')

    switch(ext,
           png = 'image/png',
           jpg = , jpeg = 'image/jpeg',
           gif = 'image/gif',
           svg = 'image/svg+xml',
           webp = 'image/webp',
           bmp = 'image/bmp',
           ico = 'image/x-icon',
           'image/png')
  }

  mime_audio = function(ext) {
    if (length(ext) != 1 || is.na(ext)) return('audio/wav')
    ext = tolower(trimws(ext))
    ext = sub("^\\.+", "", ext)
    if (!nzchar(ext)) return('audio/wav')

    switch(ext,
           wav = 'audio/wav',
           mp3 = 'audio/mpeg',
           m4a = , mp4 = 'audio/mp4',
           ogg = , oga = 'audio/ogg',
           webm = 'audio/webm',
           flac = 'audio/flac',
           'audio/wav')
  }

  html_escape = function(x) {
    x = as.character(x)
    x[is.na(x)] = ""
    x = gsub("&", "&amp;", x, fixed = TRUE)
    x = gsub("<", "&lt;", x, fixed = TRUE)
    x = gsub(">", "&gt;", x, fixed = TRUE)
    x = gsub('"', "&quot;", x, fixed = TRUE)
    x = gsub("'", "&#39;", x, fixed = TRUE)
    x
  }

  js_escape = function(x) {
    x = as.character(x)
    x[is.na(x)] = ""

    # Backslash first, then quotes and line terminators
    x = gsub("\\", "\\\\", x, fixed = TRUE)
    x = gsub("'", "\\'", x, fixed = TRUE)
    x = gsub('"', "\\\"", x, fixed = TRUE)
    x = gsub("\n", "\\n", x, fixed = TRUE)
    x = gsub("\r", "\\r", x, fixed = TRUE)
    x = gsub("\u2028", "\\u2028", x, fixed = TRUE)
    x = gsub("\u2029", "\\u2029", x, fixed = TRUE)

    # Avoid breaking out of inline <script> or HTML attributes
    x = gsub("<", "\\x3c", x, fixed = TRUE)
    x = gsub(">", "\\x3e", x, fixed = TRUE)
    x
  }

  js_array = function(x) {
    if (length(x) == 0) return("[]")
    paste0("[", paste0("'", js_escape(x), "'", collapse = ", "), "]")
  }

  url_encode_component = function(x) {
    x = as.character(x)
    x[is.na(x)] = ""
    x = enc2utf8(x)

    x = vapply(x, function(u) {
      utils::URLencode(u, reserved = TRUE, repeated = TRUE)
    }, character(1), USE.NAMES = FALSE)

    # Extra safety for characters that can break JS/HTML or URL parsing
    x = gsub("\\", "%5C", x, fixed = TRUE)
    x = gsub("'", "%27", x, fixed = TRUE)
    x = gsub('"', "%22", x, fixed = TRUE)
    x = gsub("<", "%3C", x, fixed = TRUE)
    x = gsub(">", "%3E", x, fixed = TRUE)
    x = gsub(" ", "%20", x, fixed = TRUE)
    x = gsub("\n", "%0A", x, fixed = TRUE)
    x = gsub("\r", "%0D", x, fixed = TRUE)
    x = gsub("\u2028", "%E2%80%A8", x, fixed = TRUE)
    x = gsub("\u2029", "%E2%80%A9", x, fixed = TRUE)
    x = gsub("#", "%23", x, fixed = TRUE)
    x = gsub("?", "%3F", x, fixed = TRUE)
    x = gsub("&", "%26", x, fixed = TRUE)
    x = gsub("+", "%2B", x, fixed = TRUE)
    x
  }

  img_mime = mime_image(extension)
  img_mime_js = js_escape(img_mime)

  basenames = basename(audio$filenames)
  htmlFile = file.path(audio$path_output, paste0('00_', suffix, '.html'))
  plotFiles = file.path(audio$path_output,paste0(
    basename(audio$filenames_noExt), '.', extension))

  if (changesAudio) {
    # Modified audio is written by writeAudio() as .wav
    audioFiles = file.path(
      audio$path_output,
      paste0(basename(audio$filenames_noExt), '.wav')
    )
  } else {
    # just refer to the original files
    audioFiles = audio$filenames
  }
  # NB: plotFiles and audioFiles contain full absolute paths

  audio_ext = if (changesAudio) {
    rep('wav', length(audioFiles))
  } else {
    tolower(tools::file_ext(audioFiles))
  }
  if (is.null(audio_ext)) audio_ext = character(0)
  audioMime = vapply(audio_ext, mime_audio, character(1), USE.NAMES = FALSE)

  # For javascript: concatenate plotFiles and audioFiles into an array with
  # relative names (or embeddings). Plots are always in the same folder as html,
  # so we can use relative paths, and audio is either in the same folder or one
  # level up
  if (embed) {
    # exclude missing files; otherwise, base64encode crashes
    idx_excl_plots = which(!unlist(lapply(plotFiles, file.exists)))
    idx_excl_audio = which(!unlist(lapply(audioFiles, file.exists)))
    idx_excl = c(idx_excl_plots, idx_excl_audio, which(audio$failed))
    if (length(idx_excl) > 0) {
      plotFiles = plotFiles[-idx_excl]
      audioFiles = audioFiles[-idx_excl]
      basenames = basenames[-idx_excl]
      audioMime = audioMime[-idx_excl]
    }

    # just strings with embeddings
    plotUrls = unlist(lapply(plotFiles, base64enc::base64encode))
    audioUrls = unlist(lapply(audioFiles, base64enc::base64encode))
  } else {
    # refer to files using relative, percent-encoded URLs
    plotUrls = url_encode_component(basename(plotFiles))

    if (changesAudio) {
      audioUrls = url_encode_component(basename(audioFiles))
    } else {
      audioUrls = paste0("../", url_encode_component(basename(audioFiles)))
      # use "/" for relative URLs even on Windows, '\' is an escape character
    }
  }

  plotList_js = js_array(plotUrls)
  audioList_js = js_array(audioUrls)
  basenamesList_js = js_array(html_escape(basenames))
  audioMimeList_js = js_array(audioMime)

  suffix_html = html_escape(
    if (length(suffix) == 0 || is.na(suffix[1])) "" else suffix[1]
  )

  # create an html file
  out_html = file(htmlFile, encoding = "UTF-8")
  on.exit(close(out_html))
  writeLines(
    c("<!DOCTYPE html>",
      "<html>",
      "<head>",
      paste0("<title>", suffix_html, "</title>"),
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
      "audio {",
      "  display: block;",
      "  margin: auto;",
      "  padding: 5px 0px;",
      "  width: 90%;",
      "}",
      "</style>",
      "</head>",
      "<body>",
      "<div id='flexbox'> </div>",
      "<script>",
      paste0("var plotList = ", plotList_js, ";"),
      paste0("var audioList = ", audioList_js, ";"),
      paste0("var basenamesList = ", basenamesList_js, ";"),
      paste0("var audioMimeList = ", audioMimeList_js, ";"),
      "var flex = document.getElementById('flexbox');",
      "for (var i = 0; i < plotList.length; i++) {",
      "  let newDiv = document.createElement('div');",
      "  newDiv.innerHTML =",
      "    '<h3>&nbsp;' + basenamesList[i] + '</h3>' +",
      if (embed) {
        paste0("    '<img src=\"data:", img_mime_js, ";base64,' + plotList[i] + '\">' +")
      } else {
        "    '<img src=\"' + plotList[i] + '\">' +"
      },
      if (embed) {
        "    '<audio controls> <source src=\"data:' + audioMimeList[i] + ';base64,' + audioList[i] + '\" type=\"' + audioMimeList[i] + '\"> </audio>';"
      } else {
        "    '<audio controls> <source src=\"' + audioList[i] + '\" type=\"' + audioMimeList[i] + '\"> </audio>';"
      },
      "  flex.appendChild(newDiv);",
      "}",
      "</script>",
      "</body>",
      "</html>"),
    out_html)
}
