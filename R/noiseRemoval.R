#' Noise removal
#'
#' Removes noise by log-spectral subtraction. If a recording is affected by a
#' steady noise with a relatively stable amplitude and spectrum (e.g.,
#' microphone hiss, crickets, MRI buzz, etc.), its log-spectrum can be simply
#' subtracted from that of the signal. Algorithm: STFT to produce a
#' log-spectrogram, subtract normalized noise spectrum, iSTFT to reconstitute
#' the signal. Most of the work is done by \code{\link{addFormants}}.
#'
#' @seealso \code{\link{addFormants}}
#'
#' @inheritParams .roxygen_defaults
#' @param from,to if specified (in seconds), only this section of input is
#'   denoised
#' @param noise a numeric vector of length two specifying the location of pure
#'   noise in input audio (in s); a matrix representing pure noise as a spectrum
#'   with frequency bins in rows; path to file, Wave object, or numeric vector
#'   (with the same sampling rate as \code{x}) representing pure noise
#' @param dB controls the amount of noise removal: larger values are more
#'   aggressive
#' @param specificity a way to sharpen or blur the noise spectrum (we take noise
#'   spectrum ^ specificity) : 1 = no change, >1 = sharper (the loudest noise
#'   frequencies are preferentially removed), <1 = blurred (even quiet noise
#'   frequencies are removed)
#' @param ... extra graphical parameters passed to \code{\link{spectrogram}}
#'
#' @return The denoised audio as a numeric vector (multiple inputs return a list).
#' @export
#'
#' @examples
#' s = soundgen(noise = list(time = c(-100, 400), value = -10),
#'   formantsNoise = list(f1 = list(freq = 3000, width = 25)),
#'   addSilence = 50, temperature = .001, plot = TRUE)
#' # Option 1: use part of the recording as noise profile
#' s1 = noiseRemoval(s, samplingRate = 16000, noise = c(0.05, 0.15),
#'   dB = 40, plot = TRUE)
#'
#' \dontrun{
#' # Option 2: use a separate recording as noise profile
#' noise = soundgen(pitch = NA, noise = 0,
#'   formantsNoise = list(f1 = list(freq = 3000, width = 25)))
#' spectrogram(noise, 16000)
#' s2 = noiseRemoval(s, samplingRate = 16000, noise = noise,
#'   dB = 40, plot = TRUE)
#'
#' # Option 3: provide noise spectrum as a matrix
#' spec_noise = spectrogram(
#'       noise, samplingRate = 16000,
#'       output = 'original', plot = FALSE)
#' s3 = noiseRemoval(s, samplingRate = 16000, noise = spec_noise,
#'   dB = 40, plot = TRUE)
#'
#' # play with gain and specificity
#' s4 = noiseRemoval(s, samplingRate = 16000, noise = c(0.05, 0.15),
#'   dB = 60, specificity = 2, plot = TRUE)
#'
#' # remove noise only from a section of the audio
#' s5 = noiseRemoval(s, samplingRate = 16000, from = .3, to = .5,
#'   noise = c(0.05, 0.15), dB = 60, plot = TRUE)
#' }
noiseRemoval = function(x,
                        samplingRate = NULL,
                        from = NULL,
                        to = NULL,
                        noise,
                        dB = 6,
                        specificity = 1,
                        windowLength = 50,
                        step = NULL,
                        overlap = 75,
                        dynamicRange = 120,
                        normalize = c('orig', 'max', 'none'),
                        reportEvery = NULL,
                        cores = 1,
                        play = FALSE,
                        saveAudio = FALSE,
                        plot = FALSE,
                        savePlots = FALSE,
                        embed = FALSE,
                        width = 900,
                        height = 500,
                        units = 'px',
                        res = NA,
                        ...) {
  # match args
  normalize = match.arg(normalize)
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery',
    'cores', 'savePlots', 'saveAudio', 'embed')]

  if (!is.null(from) && !is.null(to) && !(to > from))
    stop('If specified, "to" must be greater than "from"')

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    funToCall = '.noiseRemoval',
                    suffix = 'noiseRemoval',
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1)
    try(htmlPlots(pa$input, width = paste0(width, units),
                  changesAudio = saveAudio, embed = embed))

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


#' Noise removal per sound
#' @noRd
.noiseRemoval = function(audio,
                         from = NULL,
                         to = NULL,
                         noise,
                         dB = 6,
                         specificity = 1,
                         windowLength = 50,
                         step = NULL,
                         overlap = 75,
                         dynamicRange = 120,
                         normalize = 'orig',
                         play = FALSE,
                         plot = FALSE,
                         width = 900,
                         height = 500,
                         units = 'px',
                         res = NA,
                         ...) {
  val = validateWlOvlp(audio, windowLength, step, overlap)
  step = val$step

  if (inherits(noise, 'matrix')) {
    # noise is a spectrogram or spectrum
    expected_n_rows = val$wl %/% 2 + 1
    if (nrow(noise) != expected_n_rows)
      stop(paste('If "noise" is a matrix, it must have', expected_n_rows,
                 'rows to match the STFT windowLength'))
    spec_noise = rowMeans(noise)
  } else if (is.numeric(noise)) {
    len_noise = length(noise)
    if (len_noise == 2) {
      # noise is part of the input (time values: from...to, ms)
      noise_from_idx = max(1, round(noise[1] * audio$samplingRate))
      noise_to_idx = min(audio$ls, round(noise[2] * audio$samplingRate))
      audio_noise = audio$sound[noise_from_idx:noise_to_idx]
      spec_noise = stft_simple(audio_noise, samplingRate = audio$samplingRate,
                               wl = val$wl, step = val$step_points)
      spec_noise = rowMeans(Mod(spec_noise[seq_len(nrow(spec_noise) %/% 2 + 1), ]))
    } else if (len_noise > 2) {
      # noise is a separate recording passed as a vector
      spec_noise = stft_simple(noise, samplingRate = audio$samplingRate,
                               wl = val$wl, step = val$step_points)
      spec_noise = rowMeans(Mod(spec_noise[seq_len(nrow(spec_noise) %/% 2 + 1), ]))
    } else {
      stop(paste('if "noise" is numeric, it can be either a vector of length 2',
                 '(time of noise fragment in input) or >2 if it is a separate sound passed',
                 'as a vector with the same sampling rate as x'))
    }
  } else {
    # try to read noise with readAudio (eg an audio file or a Wave object)
    noise_audio = readAudio(noise)
    if (noise_audio$samplingRate != audio$samplingRate) {
      stop('The separate noise recording must have the same sampling rate as the input audio.')
    }
    spec_noise = stft_simple(noise_audio$sound, samplingRate = audio$samplingRate,
                             wl = val$wl, step = val$step_points)
    spec_noise = rowMeans(Mod(spec_noise[seq_len(nrow(spec_noise) %/% 2 + 1), ]))
  }
  if (!any(spec_noise != 0)) {
    warning('Nothing to do: "noise" is silent')
    return(audio$sound)
  }

  # select the audio to denoise
  if (is.null(from) && is.null(to)) {
    audio_to_filt = audio$sound
  } else {
    if (is.null(from)) {
      from_idx = 1
    } else {
      from_idx = max(1, round(from * audio$samplingRate))
    }
    if (is.null(to)) {
      to_idx = audio$ls
    } else {
      to_idx = min(audio$ls, round(to * audio$samplingRate))
    }
    audio_to_filt = audio$sound[from_idx:to_idx]
  }
  soundFiltered = .addFormants(
    readAudio(audio_to_filt, samplingRate = audio$samplingRate),
    windowLength = windowLength,
    step = val$step,
    formantFilter = spec_noise ^ specificity,
    dB = dB,
    action = 'remove',
    dynamicRange = dynamicRange,
    normalize = normalize)
  # spectrogram(soundFiltered, audio$samplingRate)

  # cross-fade with the original if only part of it was denoised
  if (!is.null(from)) {
    soundFiltered = crossFade(
      audio$sound[1:min(audio$ls, from_idx + val$wl/4 - 1)],
      soundFiltered,
      samplingRate = audio$samplingRate,
      crossLen_points = val$wl/4)
  }
  if (!is.null(to)) {
    soundFiltered = crossFade(
      soundFiltered,
      audio$sound[max(1, to_idx - val$wl/4 + 1):audio$ls],
      samplingRate = audio$samplingRate,
      crossLen_points = val$wl/4)
  }
  # trim to no more than original length
  soundFiltered = soundFiltered[1:min(length(soundFiltered), audio$ls)]

  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    .spectrogram(
      readAudio(soundFiltered, samplingRate = audio$samplingRate),
      dynamicRange = dynamicRange, plot = TRUE, ...)
  }

  if (isTRUE(play)) {
    playme(soundFiltered, audio$samplingRate)
  }  else if (is.character(play)) {
    playme(soundFiltered, audio$samplingRate, player = play)
  }
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(soundFiltered, audio = audio, filename = filename)
  }
  return(soundFiltered)
}
