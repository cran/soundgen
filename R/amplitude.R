#' RMS amplitude
#'
#' Calculates root mean square (RMS) amplitude in overlapping windows, providing
#' an envelope of sound intensity. Longer windows provide smoother, more robust
#' estimates; shorter windows and more overlap improve temporal resolution, but
#' they also increase processing time and make the contour less smooth.
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
#' @inheritParams .roxygen_defaults
#' @inheritParams analyze
#' @param windowLength length of analysis window, ms (longer windows = more
#'   smoothing)
#' @param stereo 'left' = only left channel, 'right' = only right channel,
#'   'average' = take the mean of the two channels, 'both' = return RMS for both
#'   channels separately
#' @param killDC if TRUE, removes DC offset (see also \code{\link{flatEnv}})
#' @param normalize if TRUE, the RMS amplitude is returned as proportion of
#'   the maximum possible amplitude as given by \code{scale}
#' @param windowDC the window for calculating DC offset, ms
#' @param plot if TRUE, plot a contour of RMS amplitude
#' @param xlab,ylab,main general graphical parameters
#' @param type,col,lwd graphical parameters pertaining to the RMS envelope
#' @param ... other graphical parameters
#'
#' @return A list containing: \describe{
#'   \item{$detailed: }{a list of RMS amplitudes per frame for each sound, on
#'   the scale of input; names give time stamps for the center of each frame, in
#'   ms.}
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
#'
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
#' r = getRMS('~/Downloads/temp', savePlots = TRUE)
#' r$summary
#'
#' # Compare:
#' analyze('~/Downloads/temp', pitchMethods = NULL,
#'         plot = FALSE)$summary$ampl_mean
#' # (per STFT frame, but should be very similar)
#'
#' # User-defined summary functions:
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
                  stereo = c('left', 'right', 'average', 'both'),
                  killDC = FALSE,
                  normalize = TRUE,
                  windowDC = 200,
                  summaryFun = 'mean',
                  reportEvery = NULL,
                  cores = 1,
                  plot = FALSE,
                  savePlots = FALSE,
                  embed = FALSE,
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
  stereo = match.arg(stereo)
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'savePlots', 'embed', 'reportEvery', 'cores', 'summaryFun')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    scale = scale,
                    from = from,
                    to = to,
                    funToCall = '.getRMS',
                    suffix = "rms",
                    savePlots = savePlots,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores
  )

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, width = paste0(width, units), embed = embed))
  }

  # prepare output
  if (!is.null(summaryFun) && any(!is.na(summaryFun))) {
    temp = vector('list', pa$input$n)
    for (i in seq_len(pa$input$n)) {
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
#' @noRd
.getRMS = function(audio,
                   windowLength = 50,
                   step = NULL,
                   overlap = 70,
                   stereo = 'left',
                   killDC = FALSE,
                   normalize = TRUE,
                   windowDC = 200,
                   plot = FALSE,
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
  val = validateWlOvlp(audio, windowLength, step, overlap)
  wl = val$wl; step = val$step; step_points = val$step_points

  # step_points can only be an integer, introducing small timing errors in long sounds
  if (stereo == 'right') {
    if (!is.null(audio$right)) {
      audio$sound = audio$right
    } else {
      warning("Input is mono; stereo = 'right' requested but no right channel exists")
    }
  }

  # DC offset
  if (killDC) {
    audio$sound = killDC(audio$sound, windowLength = windowDC,
                         samplingRate = audio$samplingRate)
    if (!is.null(audio$right) && stereo %in% c('average', 'both'))
      audio$right = killDC(audio$right, windowLength = windowDC,
                           samplingRate = audio$samplingRate)
  }

  # calculate RMS per frame
  myseq = seq(1, max(1, (audio$ls - wl)), step_points)
  r = vapply(myseq, function(x) {
    sqrt(mean(audio$sound[x:(wl + x - 1)] ^ 2))
  }, numeric(1))
  names(r) = myseq / audio$samplingRate * 1000 + windowLength / 2 +
    audio$timeShift * 1000
  if (normalize) r = r / audio$scale

  # same for the right channel
  if (!is.null(audio$right) && stereo %in% c('average', 'both')) {
    r2 = vapply(myseq, function(x) {
      sqrt(mean(audio$right[x:(wl + x - 1)] ^ 2))
    }, numeric(1))
    names(r2) = names(r)
    if (normalize) r2 = r2 / audio$scale
  }

  if (stereo == 'average') r = (r + r2) / 2
  if (stereo == 'both') r = list(left = r, right = r2)

  # plotting
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
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
      .osc(audio[which(names(audio) != 'savePlots')])
      points(as.numeric(names(r)), r * audio$scale, type = type,
             col = col, lwd = lwd, ...)
    } else if (stereo == 'both' && !is.null(audio$right)) {
      time = ((1:audio$ls) / audio$samplingRate + audio$timeShift) * 1000
      op = par(c('mar', 'mfrow')) # save user's original pars
      on.exit(par(op), add = TRUE)
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
    }
  }
  r
}


#' Normalize folder
#'
#' Normalizes the amplitude of all wav/mp3 files in a folder based on their peak
#' or RMS amplitude or subjective loudness. This is good for playback
#' experiments, which require that all sounds should have similar intensity or
#' loudness. If \code{preserveRelativeDif} is set to \code{TRUE}, all recordings
#' in the target folder get a boost in amplitude, so the loudest one reaches
#' \code{maxAmp}, but the relative differences between the recordings are
#' preserved (i.e., all files are boosted by the same amount - as much as
#' possible to avoid clipping the loudest one).
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
#' normalization procedure is similar to that for \code{type = 'rms'}. NB:
#' because loudness is not a simple linear function of SPL, loudness
#' normalization is only approximate; reiterate the process several times to
#' improve the precision of loudness normalization.
#'
#' @seealso \code{\link{getRMS}} \code{\link{analyze}} \code{\link{getLoudness}}
#'
#' @inheritParams getRMS
#' @param myfolder full path to folder containing input audio files
#' @param type normalize so the output files have the same peak amplitude
#'   ('peak'), root mean square amplitude ('rms'), or subjective loudness in
#'   sone ('loudness')
#' @param maxAmp maximum amplitude in dB (0 = max possible, -10 = 10 dB below
#'   max possible, etc.)
#' @param summaryFun should the output files have the same mean / median / max
#'   etc RMS amplitude or loudness? (summaryFun has no effect if type = 'peak')
#' @param preserveRelativeDif (only for \code{type = "peak"}) if FALSE
#'   (default), all files are normalized to the same level; if TRUE, the peak
#'   amplitude of the loudest file is set to \code{maxAmp}, while the remaining
#'   files are quieter so the original relative peak amplitude across files is
#'   preserved
#' @param savePath full path to where the normalized files should be saved;
#'   defaults to NULL = 'myfolder/normalized'; NA = do not save the processed
#'   files
#'
#' @return Does not return anything, only saves the normalized audio files.
#' @export
#' @examples
#' \dontrun{
#' # put a few short audio files in a folder, eg '~/Downloads/temp'
#' target = '~/Downloads/temp'
#' save_in_folder = paste0(target, '/normalized')
#' getRMS(target, summaryFun = 'mean')$summary  # different
#' normalizeFolder(target, type = 'rms', summaryFun = 'mean',
#'   savePath = save_in_folder)
#' getRMS(save_in_folder, summaryFun = 'mean')$summary  # same
#' # If the saved audio files are treated as stereo with one channel missing,
#' # try reconverting with ffmpeg (saving is handled by tuneR::writeWave)
#' }
normalizeFolder = function(
    myfolder,
    type = c('peak', 'rms', 'loudness'),
    maxAmp = 0,
    summaryFun = 'mean',
    preserveRelativeDif = FALSE,
    windowLength = 50,
    step = NULL,
    overlap = 70,
    killDC = FALSE,
    windowDC = 200,
    cores = 1,
    savePath = NULL,
    reportEvery = NULL
) {
  type = match.arg(type)

  # preserveRelativeDif is only mathematically sound for peak normalization
  if (preserveRelativeDif && type != 'peak') {
    warning('preserveRelativeDif is only applicable for type = "peak". Resetting to FALSE.')
    preserveRelativeDif = FALSE
  }

  time_start = proc.time()  # timing
  filenames = list.files(myfolder, pattern = "\\.(wav|mp3|WAV|MP3)$",
                         full.names = TRUE)
  n = length(filenames)
  if (n == 0) stop('No audio files found in the specified folder.')

  # in order to provide more accurate estimates of time to completion,
  # check the size of all files in the target folder
  filesizes = file.info(filenames)$size

  ## load all the files
  print('Loading...')
  files = vector('list', n)
  for (i in 1:n) {
    ext = tolower(tools::file_ext(filenames[i]))
    if (ext == 'wav') {
      files[[i]] = try(tuneR::readWave(filenames[i]))
    } else if (ext == 'mp3') {
      files[[i]] = try(tuneR::readMP3(filenames[i]))
    } else {
      message(paste('Error processing file', filenames[i]))
      files[[i]] = structure("Invalid extension", class = "try-error")
    }
    if (inherits(files[[i]], 'try-error')) {
      message(paste('Error processing file', filenames[i]))
    }
  }

  # Track valid files to prevent crashes from try-error objects downstream
  is_valid = !sapply(files, inherits, 'try-error')
  if (sum(is_valid) == 0) stop('No valid audio files could be loaded.')

  ## process all the files
  print('Processing...')
  # for either peak or RMS normalization, start by peak normalization to maxAmp dB
  level = 10 ^ (maxAmp / 20)
  if (preserveRelativeDif) {
    # maxAmp only for the loudest file, the rest with preserved relative peak ampl
    peak_per_file = rep(NA, n)
    scale_per_file = rep(NA, n)
    peak_per_file[is_valid] = sapply(files[is_valid], function(x) max(abs(x@left)))
    scale_per_file[is_valid] = sapply(files[is_valid], function(x) 2^(x@bit - 1))

    peak_rel = peak_per_file / scale_per_file
    multFactor = peak_rel / max(peak_rel, na.rm = TRUE)
  } else {
    multFactor = rep(1, n)
  }

  for (i in 1:n) {
    if (is_valid[i]) {
      files[[i]] = tuneR::normalize(
        files[[i]],
        unit = as.character(files[[i]]@bit),
        rescale = TRUE, level = level * multFactor[i])
    }
  }

  # for RMS- or loudness-normalization, perform additional steps
  if (type %in% c('rms', 'loudness')) {
    files_valid = files[is_valid]

    if (type == 'rms') {
      # calculate the RMS amplitude of each file
      perSound = getRMS(
        files_valid,
        windowLength = windowLength,
        step = step,
        overlap = overlap,
        killDC = killDC,
        windowDC = windowDC,
        cores = cores,
        reportEvery = NA,
        plot = FALSE)$detailed
    } else if (type == 'loudness') {
      # estimate subjective loudness of each file
      # Note: removed scale argument to let getLoudness extract it natively from Wave object
      perSound = getLoudness(
        files_valid,
        windowLength = windowLength,
        step = step,
        cores = cores,
        reportEvery = NA,
        plot = FALSE)$loudness
    }

    # summary measure per file
    # Robust extraction that handles NA values gracefully
    if (is.character(summaryFun)) {
      summaryPerSound = sapply(perSound, function(x) do.call(summaryFun, list(x, na.rm = TRUE)))
    } else {
      summaryPerSound = sapply(perSound, summaryFun)
    }
    names(summaryPerSound) = basename(filenames[is_valid])

    # find the quietest file
    ref = which.min(summaryPerSound)

    # the quietest file is untouched, but all others are rescaled to have the
    # same RMS/loudness as the quietest one
    for (i in seq_along(files_valid)) {
      if (i != ref) {
        rescale = summaryPerSound[ref] / summaryPerSound[i]
        files_valid[[i]]@left = as.integer(round(files_valid[[i]]@left * rescale))

        # Stereo normalization fix: scale right channel if it exists
        if (!is.null(files_valid[[i]]@right) &&
            length(files_valid[[i]]@right) > 0 &&
            is.numeric(files_valid[[i]]@right)) {
          files_valid[[i]]@right = as.integer(round(files_valid[[i]]@right * rescale))
        }
      }
    }
    # put the modified files back into the main list
    files[is_valid] = files_valid
  }

  # save the rescaled files
  if (is.null(savePath)) savePath = file.path(myfolder, 'normalized')
  if (!is.na(savePath)) {
    print('Saving...')
    if (!dir.exists(savePath)) dir.create(savePath)
    for (i in seq_len(n)) {
      if (is_valid[i]) {
        file_wo_ext = sub("([^.]+)\\.[[:alnum:]]+$", "\\1", basename(filenames[i]))
        tuneR::writeWave(
          files[[i]],
          filename = paste0(savePath, '/', file_wo_ext, '.wav')
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
#' @seealso \code{\link{transplantEnv}}
#'
#' @inheritParams .roxygen_defaults
#' @param compression the amount of compression to apply: 0 = none, 1 = maximum
#' @param method hil = Hilbert envelope, rms = root mean square amplitude, peak
#'   = peak amplitude per window
#' @param windowLength the length of smoothing window, ms
#' @param killDC if TRUE, dynamically removes DC offset or similar deviations of
#'   average waveform from zero (see examples)
#' @param dynamicRange parts of sound quieter than \code{-dynamicRange} dB will
#'   not be amplified
#' @param plot if TRUE, plots the original sound, the smoothed envelope, and
#'   the compressed sound
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
#' b = flatEnv(a, 1000, plot = TRUE, windowLength = 5)    # too short
#' c = flatEnv(a, 1000, plot = TRUE, windowLength = 450)  # too long
#' d = flatEnv(a, 1000, plot = TRUE, windowLength = 100)  # about right
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
#' s2 = flatEnv(s1, 16000, plot = TRUE, windowLength = 50, killDC = FALSE)
#' s3 = flatEnv(s1, 16000, plot = TRUE, windowLength = 50, killDC = TRUE)
#'
#' # Compress and save all audio files in a folder
#' s4 = flatEnv('~/Downloads/temp',
#'              method = 'peak', compression = .5,
#'              saveAudio = TRUE,
#'              savePlots = TRUE,
#'              col = 'green', lwd = 5)
#' osc(s4[[1]])
#' }
flatEnv = function(x,
                   samplingRate = NULL,
                   scale = NULL,
                   compression = 1,
                   method = c('hil', 'rms', 'peak'),
                   windowLength = 50,
                   killDC = FALSE,
                   dynamicRange = 40,
                   reportEvery = NULL,
                   cores = 1,
                   saveAudio = FALSE,
                   plot = FALSE,
                   savePlots = FALSE,
                   embed = FALSE,
                   col = 'blue',
                   width = 900,
                   height = 500,
                   units = 'px',
                   res = NA,
                   ...) {
  # match args
  method = match.arg(method)
  myPars = c(as.list(environment()), list(...))
  if (!is.finite(compression) || compression < 0 || compression > 1)
    stop('compression must be a number between 0 and 1')

  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'reportEvery', 'cores',
    'savePlots', 'saveAudio', 'embed')]
  if (isTRUE(savePlots) && !saveAudio)
    message(paste('Saving plots, but not audio - the html notebook will play',
                  'the original audio; set saveAudio = TRUE to save the modified sounds'))

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    scale = scale,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    suffix = 'compressor',
                    funToCall = '.flatEnv',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, changesAudio = saveAudio,
                  width = paste0(width, units), embed = embed))
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
#' @param audio a list returned by \code{readAudio}
#' @param wl the length of smoothing window, points. If
#'   specified, overrides \code{windowLength}
#' @noRd
.flatEnv = function(audio,
                    compression = 1,
                    method = 'hil',
                    windowLength = 50,
                    wl = NULL,
                    killDC = FALSE,
                    dynamicRange = 40,
                    plot = FALSE,
                    col = 'blue',
                    width = 900,
                    height = 500,
                    units = 'px',
                    res = NA,
                    ...) {
  if (!is.numeric(wl)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(audio$samplingRate)) {
        wl = round(windowLength / 1000 * audio$samplingRate)
      } else {
        stop(paste(
          'Please specify either windowLength (ms) plus samplingRate (Hz)',
          'or the length of smoothing window in points (wl)'))
      }
    }
  }
  if (!is.finite(wl) || wl < 2)
    stop('windowLength must be positive and at least 2 samples long')

  # audio$scale = original scale (eg -1 to +1 gives m = 1)
  throwaway_lin = 10 ^ (-dynamicRange / 20) * audio$scale
  if (!is.finite(throwaway_lin) || throwaway_lin <= 0) throwaway_lin = 1e-12
  # from dB to linear + normalize

  # remove DC offset
  if (killDC) {
    soundFlat = killDC(audio$sound,
                       wl = wl,
                       plot = FALSE)
  } else {
    soundFlat = audio$sound
  }

  # get smoothed amplitude envelope
  env = getEnv(soundFlat,
               wl = wl,
               method = method)
  env[env < 0] = 0

  # don't amplify very quiet sections
  idx = which(env > throwaway_lin)
  # flatten amplitude envelope
  soundFlat[idx] = soundFlat[idx] * (1 - compression) +
    soundFlat[idx] / env[idx] * audio$scale * compression

  # re-normalize to original scale
  ms = max(abs(soundFlat))
  if (is.finite(ms) && ms > 0)
    soundFlat = soundFlat / max(abs(soundFlat))
  if (!is.null(audio$scale_used))
    soundFlat = soundFlat * audio$scale_used

  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    ylim = range(c(audio$sound, soundFlat))
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(audio$sound, type = 'l', main = 'Original', ylim = ylim)
    points(env, type = 'l', col = col, ...)

    env_new = getEnv(soundFlat,
                     wl = wl,
                     method = method)
    plot(soundFlat, type = 'l', main = 'Compressed', ylim = ylim)
    points(env_new, type = 'l', col = col, ...)
    par(mfrow = op)
  }

  # save audio
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(soundFlat, audio = audio, filename = filename)
  }

  soundFlat
}


#' Transplant envelope
#'
#' Extracts a smoothed amplitude envelope of the \code{donor} sound and applies
#' it to the \code{recipient} sound. The sounds can differ in length and
#' sampling rate. Note that the result depends on the amount of smoothing
#' (controlled by \code{windowLength}) and the chosen method of calculating the
#' envelope. This is similar to "setenv" from the seewave package, but with a
#' different smoothing algorithm and with a choice of several types of envelope:
#' rms, hil, peak, etc. - see \code{\link{flatEnv}}.
#'
#' @seealso \code{\link{flatEnv}}
#'
#' @param donor the sound that "donates" the amplitude envelope
#' @param recipient the sound that needs to have its amplitude envelope adjusted
#' @param samplingRateD,samplingRateR sampling rate of the donor and recipient,
#'   respectively (only needed for vectors, not files); they don't hav to match
#' @inheritParams flatEnv
#' @return The recipient sound with the donor's amplitude envelope - a numeric
#'   vector with the same sampling rate and length as the recipient.
#' @export
#' @examples
#' donor = c(rep(0, 50), rnorm(500)) * seq(1, 0, length.out = 550)
#' data('speechEx', package = 'soundgen')
#' recipient = speechEx@left[1000:4000]
#' transplantEnv(donor, samplingRateD = 200,
#'                recipient, samplingRateR = 16000,
#'                windowLength = 50, method = 'hil', plot = TRUE)
#' transplantEnv(donor, samplingRateD = 200,
#'                recipient, samplingRateR = 16000,
#'                windowLength = 10, method = 'peak', plot = TRUE)
transplantEnv = function(donor,
                         recipient,
                         samplingRateR = NULL,
                         samplingRateD = samplingRateR,
                         windowLength = 30,
                         method = c('rms', 'hil', 'peak', 'mean'),
                         killDC = FALSE,
                         dynamicRange = 30,
                         plot = FALSE) {
  method = match.arg(method)
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
  wl_donor = windowLength / 1000 * donor$samplingRate
  wl_recip = windowLength / 1000 * recipient$samplingRate
  throwaway_lin = 10 ^ (-dynamicRange / 20) * recipient$scale
  if (!is.finite(throwaway_lin) || throwaway_lin <= 0) throwaway_lin = 1e-12

  # get the amplitude envelope of the recipient
  env_recipient = getEnv(recipient$sound,
                         wl = wl_recip,
                         method = method)
  len_recipient = length(env_recipient)

  # don't amplify very quiet sections
  env_recipient = pmax(env_recipient, throwaway_lin)

  # get the amplitude envelope of the donor
  env_donor = getEnv(donor$sound,
                     wl = wl_donor,
                     method = method)

  # normalize
  max_donor = max(env_donor)
  if (max_donor <= 0) {
    # just silence
    return(rep(0, donor$ls))
  }
  env_donor1 = env_donor / max_donor
  env_donor1 = pmax(0, spline(env_donor1, n = len_recipient)$y)
  # plot(env_donor1, type = 'l')

  # flatten the envelope of the recipient and apply the donor's envelope
  out = recipient$sound / env_recipient * env_donor1

  # normalize
  ms = max(abs(out))
  if (ms > 0)
    out = out / max(abs(out)) * recipient$scale
  # plot(out, type = 'l')

  if (plot) {
    len_donor = length(env_donor)
    max_donor = max(abs(donor$sound))
    max_recipient = max(c(abs(recipient$sound), abs(out)))

    op = par('mfrow')
    on.exit(par(mfrow = op), add = TRUE)
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
      sound = getEnv(out, wl = wl_recip, method = method),
      samplingRate = recipient$samplingRate,
      ls = len_recipient,
      filename_noExt = ''),
      lty = 1, col = 'blue', xlab = '', ylab = '', midline = FALSE,
      xaxt = 'n', yaxt = 'n', ylim = c(-max_recipient, max_recipient))
  }
  invisible(out)
}


#' Get amplitude envelope
#'
#' Calculates a smoothed envelope of a waveform based on peaks, running average,
#' root mean square (intensity), or envelope of the analytic signal with Hilbert
#' transform. Algorithm: calculates one envelope value per frame of length
#' \code{wl}, then upsamples to the original sampling rate with
#' \code{\link[stats]{spline}}.
#'
#' @param x numeric vector at least \code{wl} long
#' @param method "peak" for peak amplitude per window, "rms" for root mean
#'   square amplitude, "mean" for mean (for DC offset removal), "hil" for
#'   Hilbert envelope
#' @param wl the length of smoothing window (samples)
#' @param overlap overlap between successive windows, 0 to 100\%
#' @param step step between successive windows (samples); overrides overlap
#' @param upsample if TRUE, upsamples the envelope to the length of the original
#'   sound; if FALSE, returns one sample per window
#' @return The envelope as a numeric vector on the original scale. If
#'   \code{upsample = TRUE}, it has the same length as the input, regardless of
#'   the amount of smoothing.
#' @export
#' @examples
#' a = rnorm(500) * seq(1, 0, length.out = 500)
#' wl = 50
#' scale = max(abs(a))
#' plot(a, type = 'l', ylim = c(-scale, scale))
#' lines(getEnv(a, 'rms', wl), col = 'red')
#' lines(getEnv(a, 'peak', wl), col = 'green')
#' lines(getEnv(a, 'hil', wl), col = 'blue')
#' lines(getEnv(a, 'mean', wl), lty = 3, lwd = 3)
#'
#' # No upsampling (short output)
#' getEnv(1:16, 'mean', wl = 5, upsample = FALSE)
#' env_short = getEnv(a, 'rms', wl = wl, overlap = 50, upsample = FALSE)
#' plot(a, type = 'l')
#' lines(seq(1, length(a), length.out = length(env_short)), env_short, col = 'red')
getEnv = function(
    x,
    method = c('rms', 'hil', 'peak', 'mean'),
    wl = 200,
    overlap = 0,
    step = NULL,
    upsample = TRUE
) {
  method = match.arg(method)
  wl = round(wl)
  if (!is.finite(wl) || wl < 2)
    stop('wl must a positive integer >=2')
  if (!is.finite(overlap) || length(overlap) > 1 ||
      any(overlap < 0) || any(overlap >= 100))
    stop('overlap must be >=0 and < 100%')
  if (is.null(step))
    step = round(wl * (1 - overlap / 100))
  len_orig = length(x)

  # handle very short sounds
  if (len_orig < wl) {
    if (all(is.na(x))) {
      if (upsample) return(rep(NA_real_, len_orig)) else return(NA_real_)
    }
    if (method == 'peak') val = max(abs(x), na.rm = TRUE)
    else if (method == 'mean') val = mean(x, na.rm = TRUE)
    else if (method == 'rms') val = sqrt(mean(x^2, na.rm = TRUE))
    else if (method == 'hil') val = mean(hilbert_approx(x)$envelope, na.rm = TRUE)
    if (upsample) {
      return(rep(val, len_orig))
    } else {
      return(val)
    }
  }

  # pad with 0 (only really needed for hilbert)
  if (method == 'hil') {
    x = c(rep(0, wl),
          x,
          rep(0, wl))
  }
  len = length(x)

  # calculate the relevant stats for the entire envelope at once - more efficient
  if (method == 'peak') {
    x_abs = abs(x)  # avoid repeated calculations
  } else if (method == 'hil') {
    # calculate the entire envelope at once instead of per segment
    x_hil = hilbert_approx(x)$envelope
  } else if (method == 'rms') {
    x_sq = x^2
  }

  # prepare indices of analysis frames
  # s is a sequence of starting indices for windows over which we average
  s = seq(1, len - wl + 1, by = step)
  if (length(s) == 0) s = 1

  # moving window operations
  len_s = length(s)
  envShort = rep(NA, len_s)
  for (i in seq_len(len_s)) {
    seg = s[i] : (s[i] + wl - 1)
    if (method == 'peak') {
      # get moving peak amplitude
      envShort[i] = max(x_abs[seg])
    } else if (method == 'mean') {
      envShort[i] = mean(x[seg])
    } else if (method == 'rms') {
      envShort[i] = sqrt(mean(x_sq[seg]))
    } else if (method == 'hil') {
      envShort[i] = mean(x_hil[seg])
    }
  }

  # upsample and smooth
  if (upsample) {
    if (len_s < 1) {
      env = rep(NA, len_orig)
    } else if (len_s == 1) {
      # a single frame - repeat the one available value
      env = rep(envShort[1], len_orig)
    } else {
      # calculate the center of each window in the original coordinate system
      x_coords = s + (wl - 1) / 2
      if (method == 'hil') {
        # adjust for zero-padding
        x_coords = x_coords - wl
      }

      if (len_s == 2) {
        # two frames - interpolate linearly
        env = pmax(0, approx(x = x_coords, y = envShort, xout = 1:len_orig, rule = 2)$y)
      } else {
        # >2 frames - interpolate with spline
        env = pmax(0, spline(x = x_coords, y = envShort, xout = 1:len_orig)$y)
      }
    }
    return(env)
  } else {
    if (len_s > 2 && method == 'hil') {
      # remove the first and last points (zp)
      return(envShort[2:(len_s - 2)])
    }
    return(envShort)
  }
}


#' Kill DC
#'
#' Removes DC offset or similar imbalance in a waveform dynamically, by
#' subtracting a smoothed ~moving average. Simplified compared to a true moving
#' average, but very fast (a few ms per second of 44100 audio).
#' @inheritParams flatEnv
#' @param plot if TRUE, plots the original sound, smoothed moving average, and
#'   modified sound
#' @return The DC-corrected waveform as a numeric vector.
#' @noRd
#' @examples
#' # remove static DC offset
#' a = rnorm(500) + .3
#' b = soundgen:::killDC(a, wl = 500, plot = TRUE)
#'
#' # remove trend
#' a = rnorm(500) + seq(0, 1, length.out = 500)
#' b = soundgen:::killDC(a, wl = 100, plot = TRUE)
#'
#' # can also be used as a high-pass filter
#' a = rnorm(500) + sin(1:500 / 50)
#' b = soundgen:::killDC(a, wl = 25, plot = TRUE)
killDC = function(x,
                  windowLength = 50,
                  samplingRate = 16000,
                  wl = NULL,
                  plot = FALSE) {
  if (!is.numeric(wl)) {
    if (is.numeric(windowLength)) {
      if (is.numeric(samplingRate)) {
        wl = round(windowLength / 1000 * samplingRate)
      } else {
        stop(paste('Please specify either windowLength (ms) plus samplingRate (Hz)',
                   'or the length of smoothing window in points (wl)'))
      }
    }
  }
  if (!is.finite(wl) || wl < 2)
    stop('wl must a positive integer >=2')

  env = getEnv(x = x,
               wl = wl,
               method = 'mean')
  xNorm = x - env

  if (plot) {
    op = par('mfrow')
    par(mfrow = c(1, 2))
    plot(x, type = 'l', main = 'Original')
    points(env, type = 'l', lty = 1, col = 'blue')
    points(rep(0, length(x)), type = 'l', lty = 2)

    plot(xNorm, type = 'l', main = 'Env removed')
    points(rep(0, length(x)), type = 'l', col = 'blue')
    par(mfrow = op)
  }
  xNorm
}
