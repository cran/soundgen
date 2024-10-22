#' RMS amplitude
#'
#' Calculates root mean square (RMS) amplitude in overlapping windows, providing
#' an envelope of RMS amplitude - a measure of sound intensity. Longer windows
#' provide smoother, more robust estimates; shorter windows and more overlap
#' improve temporal resolution, but they also increase processing time and make
#' the contour less smooth.
#'
#' Note that you can also get similar estimates per frame from
#' \code{\link{analyze}} on a normalized scale of 0 to 1, but \code{getRMS} is
#' much faster, operates on the original scale, and plots the amplitude contour.
#' If you need RMS for the entire sound instead of per frame, you can simply
#' calculate it as \code{sqrt(mean(x^2))}, where \code{x} is your waveform.
#' Having RMS estimates per frame gives more flexibility: RMS per sound can be
#' calculated as the mean / median / max of RMS values per frame.
#'
#' @seealso \code{\link{analyze}} \code{\link{getLoudness}}
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param stereo 'left' = only left channel, 'right' = only right channel,
#'   'average' = take the mean of the two channels, 'both' = return RMS for both
#'   channels separately
#' @param killDC if TRUE, removed DC offset (see also \code{\link{flatEnv}})
#' @param normalize if TRUE, the RMS amplitude is returned as proportion of
#'   the maximum possible amplitude as given by \code{scale}
#' @param windowDC the window for calculating DC offset, ms
#' @param plot if TRUE, plot a contour of RMS amplitude
#' @param xlab,ylab,main general graphical parameters
#' @param type,col,lwd graphical parameters pertaining to the RMS envelope
#' @param ... other graphical parameters
#'
#' @return Returns a list containing: \describe{
#'   \item{$detailed: }{a list of RMS amplitudes per frame for each sound, on the scale of input; names give time stamps for the center of each frame, in ms.}
#'   \item{$summary: }{a dataframe with summary measures, one row per sound}
#' }
#'
#' @export
#' @examples
#' s = soundgen() + .25  # with added DC offset
#' # osc(s)
#' r = getRMS(s, samplingRate = 16000, from = .05,
#'   windowLength = 40, overlap = 50, killDC = TRUE,
#'   plot = TRUE, type = 'l', lty = 2, main = 'RMS envelope')
#' r
#' # short window = jagged envelope
#' r = getRMS(s, samplingRate = 16000,
#'   windowLength = 5, overlap = 0, killDC = TRUE,
#'   plot = TRUE, col = 'blue', pch = 13, main = 'RMS envelope')
#'
#'  # stereo
#'  wave_stereo = tuneR::Wave(
#'    left = runif(1000, -1, 1) * 16000,
#'    right = runif(1000, -1, 1) / 3 * 16000,
#'    bit = 16, samp.rate = 4000)
#'  getRMS(wave_stereo)$summary
#'  getRMS(wave_stereo, stereo = 'right')$summary
#'  getRMS(wave_stereo, stereo = 'average')$summary
#'  getRMS(wave_stereo, from = .05,
#'    stereo = 'both', plot = TRUE)$summary
#'
#' \dontrun{
#' r = getRMS('~/Downloads/temp', savePlots = '~/Downloads/temp/plots')
#' r$summary
#'
#' # Compare:
#' analyze('~/Downloads/temp', pitchMethods = NULL,
#'         plot = FALSE)$ampl_mean
#' # (per STFT frame, but should be very similar)
#'
#' User-defined summary functions:
#' ran = function(x) diff(range(x))
#' meanSD = function(x) {
#'   paste0('mean = ', round(mean(x), 2), '; sd = ', round(sd(x), 2))
#' }
#' getRMS('~/Downloads/temp', summaryFun = c('mean', 'ran', 'meanSD'))$summary
#' }
getRMS = function(x,
                  samplingRate = NULL,
                  scale = NULL,
                  from = NULL,
                  to = NULL,
                  windowLength = 50,
                  step = NULL,
                  overlap = 70,
                  stereo = c('left', 'right', 'average', 'both')[1],
                  killDC = FALSE,
                  normalize = TRUE,
                  windowDC = 200,
                  summaryFun = 'mean',
                  reportEvery = NULL,
                  cores = 1,
                  plot = FALSE,
                  savePlots = NULL,
                  main = NULL,
                  xlab = '',
                  ylab = '',
                  type = 'b',
                  col = 'green',
                  lwd = 2,
                  width = 900,
                  height = 500,
                  units = 'px',
                  res = NA,
                  ...) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'savePlots', 'reportEvery', 'cores', 'summaryFun')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    scale = scale,
                    from = from,
                    to = to,
                    funToCall = '.getRMS',
                    savePlots = savePlots,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores
  )

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = FALSE,
                  suffix = "rms", width = paste0(width, units)))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in 1:pa$input$n) {
      temp[[i]] = summarizeAnalyze(
        data.frame(ampl = pa$result[[i]]),
        summaryFun = summaryFun,
        var_noSummary = NULL)
    }
    mysum_all = cbind(data.frame(file = pa$input$filenames_base),
                      do.call('rbind', temp))
  } else {
    mysum_all = NULL
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(list(
    detailed = pa$result,
    summary = mysum_all
  ))
}


#' RMS amplitude per sound
#'
#' Internal soundgen function called by \code{\link{getRMS}}.
#' @param audio a list returned by \code{readAudio}
#' @inheritParams getRMS
#' @keywords internal
.getRMS = function(audio,
                   windowLength = 50,
                   step = NULL,
                   overlap = 75,
                   stereo = c('left', 'right', 'average', 'both')[1],
                   killDC = FALSE,
                   normalize = TRUE,
                   windowDC = 200,
                   plot = TRUE,
                   main = NULL,
                   xlab = '',
                   ylab = '',
                   type = 'b',
                   col = 'green',
                   lwd = 2,
                   width = 900,
                   height = 500,
                   units = 'px',
                   res = NA,
                   ...) {
  if (overlap < 0 | overlap > 100) {
    warning('overlap must be >0 and <= 100%; resetting to 70')
    overlap = 70
  }
  if (is.null(step)) step = windowLength * (1 - overlap / 100)
  if (!is.numeric(windowLength) | windowLength <= 0 |
      windowLength > (audio$duration * 1000)) {
    windowLength = min(50, audio$duration / 2 * 1000)
    warning(paste0('"windowLength" must be between 0 and sound_duration ms;
            defaulting to ', windowLength, ' ms'))
  }
  windowLength_points = floor(windowLength / 1000 * audio$samplingRate / 2) * 2
  step_points = round(step / 1000 * audio$samplingRate)
  step = step_points / audio$samplingRate * 1000
  windowLength = windowLength_points / audio$samplingRate * 1000
  # step_points can only be an integer, introducing small timing errors in long sounds
  if (stereo == 'right') audio$sound = audio$right

  # DC offset
  if (killDC) {
    audio$sound = killDC(audio$sound, windowLength = windowDC,
                         samplingRate = audio$samplingRate)
    if (!is.null(audio$right) & stereo %in% c('average', 'both'))
      audio$right = killDC(audio$right, windowLength = windowDC,
                           samplingRate = audio$samplingRate)
  }

  # calculate RMS per frame
  myseq = seq(1, max(1, (audio$ls - windowLength_points)), step_points)
  r = vapply(myseq, function(x) {
    sqrt(mean(audio$sound[x:(windowLength_points + x - 1)] ^ 2))
  }, numeric(1))
  names(r) = myseq / audio$samplingRate * 1000 + windowLength / 2 + audio$timeShift * 1000
  if (normalize) r = r / audio$scale

  # same for the right channel
  if (!is.null(audio$right) & stereo %in% c('average', 'both')) {
    r2 = vapply(myseq, function(x) {
      sqrt(mean(audio$right[x:(windowLength_points + x - 1)] ^ 2))
    }, numeric(1))
    names(r2) = names(r)
    if (normalize) r2 = r2 / audio$scale
  }

  if (stereo == 'average') r = (r + r2) / 2
  if (stereo == 'both') r = list(left = r, right = r2)

  # plotting
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_rms.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    if (is.null(main)) {
      if (audio$filename_noExt == 'sound') {
        main = ''
      } else {
        main = audio$filename_noExt
      }
    }

    if (stereo != 'both') {
      .osc(audio)
      points(as.numeric(names(r)), r * audio$scale, type = type,
             col = col, lwd = lwd, ...)
    } else if (stereo == 'both' & !is.null(audio$right)) {
      time = ((1:audio$ls) / audio$samplingRate + audio$timeShift) * 1000
      op = par(c('mar', 'mfrow')) # save user's original pars
      layout(matrix(c(2, 1), nrow = 2, byrow = TRUE))
      par(mfrow = c(2, 1), mar = c(0, par()$mar[2:4]))
      plot(time, audio$sound, type = 'n', main = main, xlab = xlab, ylab = ylab,
           xaxt = 'n', ylim = c(-audio$scale, audio$scale), ...)
      time_location = axTicks(1)
      time_labels = convert_sec_to_hms(time_location / 1000, 3)
      # axis(side = 1, at = time_location, labels = time_labels)
      points(time, audio$sound, type = 'l')
      points(as.numeric(names(r$left)), r$left * audio$scale, type = type,
             col = col, lwd = lwd, ...)
      text(0, audio$scale, adj = c(1, 1), labels = 'L', col = 'blue', cex = 1.5)

      par(mar = c(op$mar[1:2], 0, op$mar[4]))
      plot(time, audio$right, type = 'n', main = main, xlab = xlab, ylab = ylab,
           xaxt = 'n', ylim = c(-audio$scale, audio$scale), ...)
      time_location = axTicks(1)
      time_labels = convert_sec_to_hms(time_location / 1000, 3)
      axis(side = 1, at = time_location, labels = time_labels)
      points(time, audio$right, type = 'l')
      points(as.numeric(names(r$right)), r$right * audio$scale, type = type,
             col = col, lwd = lwd, ...)
      text(0, audio$scale, adj = c(1, 1), labels = 'R', col = 'blue', cex = 1.5)
      # restore original pars
      par('mar' = op$mar, 'mfrow' = op$mfrow)
    }
    if (is.character(audio$savePlots)) dev.off()
  }
  r
}


#' Normalize folder
#'
#' Normalizes the amplitude of all wav/mp3 files in a folder based on their peak
#' or RMS amplitude or subjective loudness. This is good for playback
#' experiments, which require that all sounds should have similar intensity or
#' loudness.
#'
#' Algorithm: first all files are rescaled to have the same peak amplitude of
#' \code{maxAmp} dB. If \code{type = 'peak'}, the process ends here. If
#' \code{type = 'rms'}, there are two additional steps. First the original RMS
#' amplitude of all files is calculated per frame by \code{\link{getRMS}}. The
#' "quietest" sound with the lowest summary RMS value is not modified, so its
#' peak amplitude remains \code{maxAmp} dB. All the remaining sounds are
#' rescaled linearly, so that their summary RMS values becomes the same as that
#' of the "quietest" sound, and their peak amplitudes become smaller,
#' \code{<maxAmp}. Finally, if \code{type = 'loudness'}, the subjective
#' loudness of each sound is estimated by \code{\link{getLoudness}}, which
#' assumes frequency sensitivity typical of human hearing. The following
#' normalization procedure is similar to that for \code{type = 'rms'}.
#'
#' @seealso \code{\link{getRMS}} \code{\link{analyze}} \code{\link{getLoudness}}
#'
#' @inheritParams getRMS
#' @param myfolder full path to folder containing input audio files
#' @param type normalize so the output files has the same peak amplitude
#'   ('peak'), root mean square amplitude ('rms'), or subjective loudness in
#'   sone ('loudness')
#' @param maxAmp maximum amplitude in dB (0 = max possible, -10 = 10 dB below
#'   max possible, etc.)
#' @param summaryFun should the output files have the same mean / median / max
#'   etc rms amplitude or loudness? (summaryFun has no effect if type = 'peak')
#' @param saveAudio full path to where the normalized files should be saved
#'   (defaults to 'myfolder/normalized')
#' @export
#' @examples
#' \dontrun{
#' # put a few short audio files in a folder, eg '~/Downloads/temp'
#' getRMS('~/Downloads/temp', summaryFun = 'mean')$summary  # different
#' normalizeFolder('~/Downloads/temp', type = 'rms', summaryFun = 'mean',
#'   saveAudio = '~/Downloads/temp/normalized')
#' getRMS('~/Downloads/temp/normalized', summaryFun = 'mean')$summary  # same
#' # If the saved audio files are treated as stereo with one channel missing,
#' # try reconverting with ffmpeg (saving is handled by tuneR::writeWave)
#' }
normalizeFolder = function(
    myfolder,
    type = c('peak', 'rms', 'loudness')[1],
    maxAmp = 0,
    summaryFun = 'mean',
    windowLength = 50,
    step = NULL,
    overlap = 70,
    killDC = FALSE,
    windowDC = 200,
    saveAudio = NULL,
    reportEvery = NULL
) {
  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "*.wav|.mp3|.WAV|.MP3",
                         full.names = TRUE)
  filesizes = file.info(filenames)$size
  if (length(filenames) < 1) {
    stop(paste('No wav/mp3 files found in', myfolder))
  }
  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  n = length(filenames)

  ## load all the files
  print('Loading...')
  files = vector('list', n)
  for (i in 1:n) {
    ext = substr(filenames[i], (nchar(filenames[i]) - 2), nchar(filenames[i]))
    if (ext %in% c('wav', 'WAV')) {
      files[[i]] = try(tuneR::readWave(filenames[i]))
    } else if (ext %in% c('mp3', 'MP3')) {
      files[[i]] = try(tuneR::readMP3(filenames[i]))
    } else {
      message(paste('Error processing file', filenames[i]))
    }
    if (inherits(files[[i]], 'try-error')) {
      message(paste('Error processing file', filenames[i]))
    }
  }

  ## process all the files
  print('Processing...')
  # for either peak or RMS normalization, start by peak normalization to maxAmp dB
  level = 10 ^ (maxAmp / 20)
  for (i in 1:n) {
    if (!inherits(files[[i]], 'try-error'))
      files[[i]] = tuneR::normalize(
        files[[i]],
        unit = as.character(files[[i]]@bit),
        rescale = TRUE, level = level)
  }

  # for RMS- or loudness-normalization, perform additional steps
  if (type %in% c('rms', 'loudness')) {
    perSound = vector('list', n)
    if (type == 'rms') {
      for (i in 1:n) {
        # calculate the RMS amplitude of each file
        if (!inherits(files[[i]], 'try-error'))
          perSound[[i]] = getRMS(
            files[[i]],
            windowLength = windowLength,
            step = step,
            overlap = overlap,
            scale = 2^(files[[i]]@bit - 1),
            killDC = killDC,
            windowDC = windowDC,
            plot = FALSE)$detailed
      }
    } else if (type == 'loudness') {
      for (i in 1:n) {
        # estimate subjective loudness of each file
        if (!inherits(files[[i]], 'try-error'))
          perSound[[i]] = getLoudness(
            files[[i]],
            scale = 2^(files[[i]]@bit - 1),
            windowLength = windowLength,
            step = step,
            plot = FALSE)$loudness
      }
    }

    # summary measure per file
    summaryPerSound = sapply(perSound, summaryFun)
    names(summaryPerSound) = basename(filenames)

    # find the quietest file
    ref = which.min(summaryPerSound)

    # the quietest file is untouched, but all others are rescaled to have the
    # same RMS/loudness as the quietest one
    for (i in 1:n) {
      if (!inherits(files[[i]], 'try-error') & i != ref) {
        if (type == 'rms') {
          rescale = summaryPerSound[ref] / summaryPerSound[i]
        } else if (type == 'loudness') {
          rescale = (summaryPerSound[ref] / summaryPerSound[i]) ^ (5 / 3)
        }
        files[[i]]@left = as.integer(round(files[[i]]@left * rescale))
      }
    }
  }

  # save the rescaled files
  if (is.null(saveAudio)) saveAudio = paste0(myfolder, '/normalized')
  if (!is.na(saveAudio)) {
    print('Saving...')
    if (!dir.exists(saveAudio)) dir.create(saveAudio)
    for (i in 1:n) {
      if (!inherits(files[[i]], 'try-error')) {
        file_wo_ext = sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(filenames[i]))
        tuneR::writeWave(
          files[[i]],
          filename = paste0(saveAudio, '/', file_wo_ext, '.wav')
        )
      }
    }
  }

  # report time
  if (is.null(reportEvery) || is.finite(reportEvery)) {
    reportTime(i = n, nIter = n, reportEvery = reportEvery,
               time_start = time_start, jobs = filesizes)
  }
}


#' Flat envelope / compressor
#'
#' Applies a compressor - that is, flattens the amplitude envelope of a
#' waveform, reducing the difference in amplitude between loud and quiet
#' sections. This is achieved by dividing the waveform by some function of its
#' smoothed amplitude envelope (Hilbert, peak or root mean square).
#'
#' @inheritParams spectrogram
#' @inheritParams segment
#' @param compression the amount of compression to apply: 0 = none, 1 = maximum
#' @param method hil = Hilbert envelope, rms = root mean square amplitude, peak
#'   = peak amplitude per window
#' @param windowLength the length of smoothing window, ms
#' @param windowLength_points the length of smoothing window, points. If
#'   specified, overrides \code{windowLength}
#' @param killDC if TRUE, dynamically removes DC offset or similar deviations of
#'   average waveform from zero (see examples)
#' @param dynamicRange parts of sound quieter than \code{-dynamicRange} dB will
#'   not be amplified
#' @param plot if TRUE, plots the original sound, the smoothed envelope, and
#'   the compressed sound
#' @param saveAudio full path to the folder in which to save the compressed
#'   sound(s)
#' @param col the color of amplitude contours
#' @param ... other graphical parameters passed to \code{points()} that control
#'   the appearance of amplitude contours, eg \code{lwd, lty}, etc.
#'
#' @return If the input is a single audio (file, Wave, or numeric vector),
#'   returns the compressed waveform as a numeric vector with the original
#'   sampling rate and scale. If the input is a folder with several audio files,
#'   returns a list of compressed waveforms, one for each file.
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' b = flatEnv(a, 1000, plot = TRUE, windowLength_points = 5)    # too short
#' c = flatEnv(a, 1000, plot = TRUE, windowLength_points = 450)  # too long
#' d = flatEnv(a, 1000, plot = TRUE, windowLength_points = 100)  # about right
#'
#' \dontrun{
#' s = soundgen(sylLen = 1000, ampl = c(0, -40, 0), plot = TRUE)
#' # playme(s)
#' s_flat1 = flatEnv(s, 16000, dynamicRange = 60, plot = TRUE,
#'                   windowLength = 50, method = 'hil')
#' s_flat2 = flatEnv(s, 16000, dynamicRange = 60, plot = TRUE,
#'                   windowLength = 10, method = 'rms')
#' s_flat3 = flatEnv(s, 16000, dynamicRange = 60, plot = TRUE,
#'                   windowLength = 10, method = 'peak')
#' # playme(s_flat2)
#'
#' # Remove DC offset
#' s1 = c(rep(0, 50), runif(1000, -1, 1), rep(0, 50)) +
#'      seq(.3, 1, length.out = 1100)
#' s2 = flatEnv(s1, 16000, plot = TRUE, windowLength_points = 50, killDC = FALSE)
#' s3 = flatEnv(s1, 16000, plot = TRUE, windowLength_points = 50, killDC = TRUE)
#'
#' # Compress and save all audio files in a folder
#' s4 = flatEnv('~/Downloads/temp',
#'              method = 'peak', compression = .5,
#'              saveAudio = '~/Downloads/temp/compressed',
#'              savePlots = '~/Downloads/temp/compressed',
#'              col = 'green', lwd = 5)
#' osc(s4[[1]])
#' }
flatEnv = function(x,
                   samplingRate = NULL,
                   scale = NULL,
                   compression = 1,
                   method = c('hil', 'rms', 'peak')[1],
                   windowLength = 50,
                   windowLength_points = NULL,
                   killDC = FALSE,
                   dynamicRange = 40,
                   reportEvery = NULL,
                   cores = 1,
                   saveAudio = NULL,
                   plot = FALSE,
                   savePlots = NULL,
                   col = 'blue',
                   width = 900,
                   height = 500,
                   units = 'px',
                   res = NA,
                   ...) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'reportEvery', 'cores', 'savePlots', 'saveAudio')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    scale = scale,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    funToCall = '.flatEnv',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = TRUE,
                  suffix = "compressor", width = paste0(width, units)))
  }

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' @rdname flatEnv
#' @export
compressor = flatEnv


#' Flat envelope per sound
#'
#' Internal soundgen function
#'
#' @param audio a list returned by \code{readAudio}
#' @inheritParams flatEnv
#' @keywords internal
.flatEnv = function(audio,
                    compression = 1,
                    method = c('hil', 'rms', 'peak')[1],
                    windowLength = 50,
                    windowLength_points = NULL,
                    killDC = FALSE,
                    dynamicRange = 40,
                    plot = FALSE,
                    col = 'blue',
                    width = 900,
                    height = 500,
                    units = 'px',
                    res = NA,
                    ...) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(audio$samplingRate)) {
        windowLength_points = windowLength / 1000 * audio$samplingRate
      } else {
        stop(paste(
          'Please specify either windowLength (ms) plus samplingRate (Hz)',
          'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  # audio$scale = original scale (eg -1 to +1 gives m = 1)
  throwaway_lin = 10 ^ (-dynamicRange / 20) * audio$scale
  # from dB to linear + normalize

  # remove DC offset
  if (killDC) {
    soundFlat = killDC(sound = audio$sound,
                       windowLength_points = windowLength_points,
                       plot = FALSE)
  } else {
    soundFlat = audio$sound
  }

  # get smoothed amplitude envelope
  env = getEnv(sound = soundFlat,
               windowLength_points = windowLength_points,
               method = method)
  env[env < 0] = 0

  # don't amplify very quiet sections
  idx = which(env > throwaway_lin)
  # flatten amplitude envelope
  soundFlat[idx] = soundFlat[idx] * (1 - compression) +
    soundFlat[idx] / env[idx] * audio$scale * compression
  # re-normalize to original scale
  soundFlat = soundFlat / max(abs(soundFlat))
  if (!is.null(audio$scale_used))
    soundFlat = soundFlat * audio$scale_used

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_compressor.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    ylim = range(c(audio$sound, soundFlat))
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(audio$sound, type = 'l', main = 'Original', ylim = ylim)
    points(env, type = 'l', col = col, ...)

    env_new = getEnv(sound = soundFlat,
                     windowLength_points = windowLength_points,
                     method = method)
    plot(soundFlat, type = 'l', main = 'Compressed', ylim = ylim)
    points(env_new, type = 'l', col = col, ...)
    par(mfrow = op)
    if (is.character(audio$savePlots)) dev.off()
  }

  if (!is.null(audio$saveAudio)) {
    if (!dir.exists(audio$saveAudio)) dir.create(audio$saveAudio)
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(soundFlat, audio = audio, filename = filename)
  }

  soundFlat
}


#' Transplant envelope
#'
#' Extracts a smoothed amplitude envelope of the \code{donor} sound and applies
#' it to the \code{recipient} sound. Both sounds are provided as numeric
#' vectors; they can differ in length and sampling rate. Note that the result
#' depends on the amount of smoothing (controlled by \code{windowLength}) and
#' the chosen method of calculating the envelope. Very similar to
#' \code{\link[seewave]{setenv}}, but with a different smoothing algorithm and
#' with a choice of several types of envelope: hil, rms, or peak.
#'
#' @seealso \code{\link{flatEnv}} \code{\link[seewave]{setenv}}
#'
#' @param donor the sound that "donates" the amplitude envelope
#' @param recipient the sound that needs to have its amplitude envelope adjusted
#' @param samplingRateD,samplingRateR sampling rate of the donor and recipient,
#'   respectively (only needed for vectors, not files)
#' @inheritParams flatEnv
#' @return Returns the recipient sound with the donor's amplitude envelope - a
#'   numeric vector with the same sampling rate as the recipient
#' @export
#' @examples
#' donor = rnorm(500) * seq(1, 0, length.out = 500)
#' recipient = soundgen(sylLen = 600, addSilence = 50)
#' transplantEnv(donor, samplingRateD = 200,
#'                recipient, samplingRateR = 16000,
#'                windowLength = 50, method = 'hil', plot = TRUE)
#' transplantEnv(donor, samplingRateD = 200,
#'                recipient, samplingRateR = 16000,
#'                windowLength = 10, method = 'peak', plot = TRUE)
transplantEnv = function(donor,
                         samplingRateD = NULL,
                         recipient,
                         samplingRateR = samplingRateD,
                         windowLength = 50,
                         method = c('hil', 'rms', 'peak')[3],
                         killDC = FALSE,
                         dynamicRange = 80,
                         plot = FALSE) {
  # Read inputs
  donor = readAudio(
    donor,
    input = checkInputType(donor),
    samplingRate = samplingRateD
  )
  recipient = readAudio(
    recipient,
    input = checkInputType(recipient),
    samplingRate = samplingRateR
  )
  windowLength_points_donor = windowLength / 1000 * donor$samplingRate
  windowLength_points_recip = windowLength / 1000 * recipient$samplingRate
  throwaway_lin = 10 ^ (-dynamicRange / 20) * recipient$scale

  # get the amplitude envelope of the recipient
  env_recipient = getEnv(sound = recipient$sound,
                         windowLength_points = windowLength_points_recip,
                         method = method)
  len_recipient = length(env_recipient)
  # don't amplify very quiet sections
  env_recip_cut = env_recipient
  env_recip_cut[env_recip_cut < throwaway_lin] = throwaway_lin
  # plot(env_recip_cut, type = 'l')

  # get the amplitude envelope of the donor
  env_donor = getEnv(sound = donor$sound,
                     windowLength_points = windowLength_points_donor,
                     method = method)
  env_donor1 = env_donor / max(env_donor)  # normalize
  env_donor1 = approx(env_donor1, n = len_recipient)$y
  # plot(env_donor1, type = 'l')

  # flatten the envelope of the recipient and apply the donor's envelope
  out = recipient$sound / env_recip_cut * env_donor1
  out = out / max(abs(out)) * recipient$scale
  # plot(out, type = 'l')
  if (plot) {
    len_donor = length(env_donor)
    max_donor = max(abs(donor$sound))
    max_recipient = max(c(abs(recipient$sound), abs(out)))

    op = par('mfrow')
    par(mfrow = c(1, 3))

    .osc(donor, main = 'Donor', ylim = c(-max_donor, max_donor),
         xlab = '', ylab = 'Amplitude', midline = FALSE)
    par(new = TRUE)
    .osc(list(sound = env_donor,
              samplingRate = donor$samplingRate,
              ls = len_donor,
              filename_noExt = ''),
         lty = 1, col = 'blue', xlab = '', ylab = '', midline = FALSE,
         xaxt = 'n', yaxt = 'n', ylim = c(-max_donor, max_donor))

    .osc(recipient,  main = 'Recipient', ylim = c(-max_recipient, max_recipient),
         xlab = 'Time, s', ylab = '', midline = FALSE)
    par(new = TRUE)
    .osc(list(sound = env_recipient,
              samplingRate = recipient$samplingRate,
              ls = len_recipient,
              filename_noExt = ''),
         lty = 1, col = 'blue', xlab = '', ylab = '', midline = FALSE,
         xaxt = 'n', yaxt = 'n', ylim = c(-max_recipient, max_recipient))

    .osc(list(sound = out,
              samplingRate = recipient$samplingRate,
              ls = len_recipient,
              filename_noExt = ''),
         main = 'Output', ylim = c(-max_recipient, max_recipient),
         xlab = '', ylab = '', midline = FALSE)
    par(new = TRUE)
    .osc(list(
      sound = getEnv(out, windowLength_points_recip, method),
      samplingRate = recipient$samplingRate,
      ls = len_recipient,
      filename_noExt = ''),
      lty = 1, col = 'blue', xlab = '', ylab = '', midline = FALSE,
      xaxt = 'n', yaxt = 'n', ylim = c(-max_recipient, max_recipient))

    par(mfrow = op)
  }
  out
}


#' Get amplitude envelope
#'
#' Returns the smoothed amplitude envelope of a waveform on the original scale.
#' Unlike seewave::env, this function always returns an envelope of the same
#' length as the original sound, regardless of the amount of smoothing.
#' @param sound numeric vector
#' @param windowLength_points the length of smoothing window, in points
#' @param method 'peak' for peak amplitude per window, 'rms' for root mean
#'   square amplitude, 'mean' for mean (for DC offset removal), 'hil' for
#'   Hilbert, 'raw' for low-pass filtering the actual sound
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' windowLength_points = 50
#' scale = max(abs(a))
#' plot(a, type = 'l', ylim = c(-scale, scale))
#' points(soundgen:::getEnv(a, windowLength_points, 'rms'),
#'        type = 'l', col = 'red')
#' points(soundgen:::getEnv(a, windowLength_points, 'peak'),
#'        type = 'l', col = 'green')
#' points(soundgen:::getEnv(a, windowLength_points, 'hil'),
#'        type = 'l', col = 'blue')
#' points(soundgen:::getEnv(a, windowLength_points, 'mean'),
#'        type = 'l', lty = 3, lwd = 3)
getEnv = function(
    sound,
    windowLength_points,
    method = c('rms', 'hil', 'peak', 'raw', 'mean')[1]
) {
  if (!method %in%  c('rms', 'hil', 'peak', 'raw', 'mean'))
    stop("Valid values for method are c('rms', 'hil', 'peak', 'mean')")
  windowLength_points = round(windowLength_points)
  sound = c(rep(0, windowLength_points),
            sound,
            rep(0, windowLength_points))
  len = length(sound)
  if (method == 'peak') sound_abs = abs(sound)  # avoid repeated calculations

  # moving window operations: get per window, then upsample
  if (windowLength_points >= len / 2) {
    # short sound relative to window - just take beginning and end (2 points)
    s = c(1, len)
  } else {
    s = c(1,
          seq(from = floor(windowLength_points / 2),
              to = length(sound) - floor(windowLength_points / 2),
              by = windowLength_points),
          length(sound))
  }
  # s is a sequence of starting indices for windows over which we average
  envShort = rep(NA, length(s) - 1)
  for (i in 1:(length(s) - 1)) {
    seg = s[i] : s[i+1]
    if (method == 'peak') {
      # get moving peak amplitude
      envShort[i] = max(sound_abs[seg])
    } else if (method == 'mean' || method == 'raw') {
      envShort[i] = mean(sound[seg])
    } else if (method == 'rms') {
      envShort[i] = mean(sqrt(sound[seg] ^ 2))
    } else if (method == 'hil') {
      envShort[i] = mean(Mod(seewave::hilbert(sound[seg],
                                              f = 1,  # not actually needed
                                              fftw = FALSE)))
    }
  }
  # upsample and smooth
  env = getSmoothContour(
    anchors = data.frame(time = seq(0, 1, length.out = length(envShort)),
                         value = envShort),
    len = length(sound),
    valueFloor = 0,
    loessSpan = 1
  )

  # remove the zero-padded regions
  env = env[(windowLength_points + 1):(len - windowLength_points)]
  # osc(env)
  env
}


#' Kill DC
#'
#' Removes DC offset or similar disbalance in a waveform dynamically, by
#' subtracting a smoothed ~moving average. Simplified compared to a true moving
#' average, but very fast (a few ms per second of 44100 audio).
#' @inheritParams flatEnv
#' @param plot if TRUE, plots the original sound, smoothed moving average, and
#'   modified sound
#' @keywords internal
#' @examples
#' # remove static DC offset
#' a = rnorm(500) + .3
#' b = soundgen:::killDC(a, windowLength_points = 500, plot = TRUE)
#'
#' # remove trend
#' a = rnorm(500) + seq(0, 1, length.out = 500)
#' b = soundgen:::killDC(a, windowLength_points = 100, plot = TRUE)
#'
#' # can also be used as a high-pass filter
#' a = rnorm(500) + sin(1:500 / 50)
#' b = soundgen:::killDC(a, windowLength_points = 25, plot = TRUE)
killDC = function(sound,
                  windowLength = 200,
                  samplingRate = 16000,
                  windowLength_points = NULL,
                  plot = FALSE) {
  if (!is.numeric(windowLength_points)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(samplingRate)) {
        windowLength_points = windowLength / 1000 * samplingRate
      } else {
        stop(paste('Please specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (windowLength_points)'))
      }
    }
  }

  env = getEnv(sound = sound,
               windowLength_points = windowLength_points,
               method = 'mean')
  soundNorm = sound - env

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(sound, type = 'l', main = 'Original')
    points(env, type = 'l', lty = 1, col = 'blue')
    points(rep(0, length(sound)), type = 'l', lty = 2)

    plot(soundNorm, type = 'l', main = 'Env removed')
    points(rep(0, length(sound)), type = 'l', col = 'blue')
    par(mfrow = op)
  }
  soundNorm
}
