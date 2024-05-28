#' Noise removal
#'
#' Removes noise by spectral substraction. If a recording is affected by a
#' steady noise with a relatively stable amplitude and spectrum (e.g.,
#' microphone hiss, crickets, MRI buzz, etc.), its spectrum can be simply
#' subtracted from the signal. Algorithm: STFT to produce a spectrogram, divide
#' by normalized noise spectrum, inverse STFT to reconstitute the signal. Most
#' of the work is done by \code{\link{addFormants}}.
#'
#' @inheritParams spectrogram
#' @inheritParams addFormants
#' @param noise a numeric vector of length two specifying the location of pure
#'   noise in input audio (in s); a matrix representing pure noise as a spectrum
#'   with frequency bins in rows; any input accepted by
#'   \code{\link{spectrogram}} if pure noise is found in a separate recording
#'   (eg path to file, numeric vector, etc.)
#' @param specificity a way to sharpen or blur the noise spectrum (we take noise
#'   spectrum ^ specificity) : 1 = no change, >1 = sharper (the loudest noise
#'   frequencies are preferentially removed), <1 = blurred (even quiet noise
#'   frequencies are removed)
#' @seealso \code{\link{addFormants}}
#' @return Returns the denoised audio
#' @export
#'
#' @examples
#' s = soundgen(noise = list(time = c(-100, 400), value = -20),
#'   formantsNoise = list(f1 = list(freq = 3000, width = 25)),
#'   addSilence = 50, temperature = .001, plot = TRUE)
#' # Option 1: use part of the recording as noise profile
#' s1 = noiseRemoval(s, samplingRate = 16000, noise = c(0.05, 0.15),
#'   dB = 40, plot = TRUE)
#'
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
#' \dontrun{
#' # play with gain and sensitivity
#' noiseRemoval(s, samplingRate = 16000, noise = c(0.05, 0.15),
#'   dB = 60, specificity = 2, plot = TRUE)
#'
#' # remove noise only from a section of the audio
#' noiseRemoval(s, samplingRate = 16000, from = .3, to = .4,
#'   noise = c(0.05, 0.15), dB = 60, plot = TRUE, play = TRUE)
#' }
noiseRemoval = function(x,
                        samplingRate = NULL,
                        scale = NULL,
                        noise,
                        dB = 6,
                        specificity = 1,
                        windowLength = 50,
                        step = windowLength / 2,
                        dynamicRange = 120,
                        normalize = c('max', 'orig', 'none')[2],
                        reportEvery = NULL,
                        cores = 1,
                        play = FALSE,
                        saveAudio = NULL,
                        plot = FALSE,
                        savePlots = NULL,
                        width = 900,
                        height = 500,
                        units = 'px',
                        res = NA,
                        ...) {
  # match args
  myPars = c(as.list(environment()), list(...))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery',
    'cores', 'savePlots', 'saveAudio')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    scale = scale,
                    saveAudio = saveAudio,
                    savePlots = savePlots,
                    funToCall = '.noiseRemoval',
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # htmlPlots
  if (!is.null(pa$input$savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, savePlots = savePlots, changesAudio = TRUE,
                  suffix = "noiseRemoval", width = paste0(width, units)))
  }

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  invisible(result)
}


.noiseRemoval = function(audio,
                         from = NULL,
                         to = NULL,
                         noise,
                         dB = 6,
                         specificity = 1,
                         windowLength = 50,
                         step = windowLength / 2,
                         dynamicRange = 120,
                         normalize = c('max', 'orig', 'none')[2],
                         reportEvery = NULL,
                         cores = 1,
                         play = FALSE,
                         saveAudio = NULL,
                         plot = FALSE,
                         savePlots = NULL,
                         width = 900,
                         height = 500,
                         units = 'px',
                         res = NA,
                         ...) {
  # audio = list(sound = sn, samplingRate = 16000)
  if (is.numeric(noise) & length(noise) == 2) {
    # noise is part of the input
    noise_from_idx = round(noise[1] * audio$samplingRate)
    noise_to_idx = round(noise[2] * audio$samplingRate)
    audio_noise = audio
    audio_noise$sound = audio$sound[noise_from_idx:noise_to_idx]
    audio_noise$ls = length(audio_noise$sound)
    audio_noise$duration = audio_noise$ls / audio_noise$samplingRate
    spec_noise = try(rowMeans(.spectrogram(
      audio_noise,
      windowLength = windowLength,
      step = step,
      output = 'original',
      plot = FALSE)))
  } else if (inherits(noise, 'matrix')) {
    # noise if a spectrogram or spectrum
    spec_noise = rowMeans(noise)
  } else {
    # noise is a separate recording
    spec_noise = try(rowMeans(spectrogram(
      noise,
      samplingRate = audio$samplingRate,
      windowLength = windowLength,
      step = step,
      output = 'original',
      plot = FALSE)))
  }
  if (inherits(spec_noise, 'try-error')) {
    warning('Failed to obtained noise profile')
    return(NA)
  }

  # select the audio to denoise
  if (is.null(from) & is.null(to)) {
    audio_to_filt = audio$sound
  } else {
    if (is.null(from)) {
      from_idx = 1
    } else {
      from_idx = round(from * audio$samplingRate)
    }
    if (is.null(to)) {
      to = audio$ls
    } else {
      to_idx = round(to * audio$samplingRate)
    }
    audio_to_filt = audio$sound[from_idx:to_idx]
  }
  soundFiltered = addFormants(
    audio_to_filt,
    samplingRate = audio$samplingRate,
    from = from,
    to = to,
    spectralEnvelope = spec_noise ^ specificity,
    dB = dB,
    action = 'remove',
    normalize = normalize)
  # spectrogram(soundFiltered, audio$samplingRate)

  # cross-fade with the original if only part of it was denoised
  if (!is.null(from)) {
    soundFiltered = crossFade(audio$sound[1:from_idx],
                              soundFiltered,
                              samplingRate = audio$samplingRate,
                              crossLen = windowLength / 4)
  }
  if (!is.null(to)) {
    soundFiltered = crossFade(soundFiltered,
                              audio$sound[to_idx:audio$ls],
                              samplingRate = audio$samplingRate,
                              crossLen = windowLength / 4)
  }
  # trim to no more than original length
  soundFiltered = soundFiltered[1:min(length(soundFiltered), audio$ls)]

  # PLOTTING
  if (is.character(audio$savePlots)) {
    plot = TRUE
    png(filename = paste0(audio$savePlots, audio$filename_noExt, "_noiseRemoval.png"),
        width = width, height = height, units = units, res = res)
  }
  if (plot) {
    spectrogram(soundFiltered, audio$samplingRate, ...)
    if (is.character(audio$savePlots)) dev.off()
  }

  if (play) playme(soundFiltered, audio$samplingRate)
  if (!is.null(audio$saveAudio)) {
    if (!dir.exists(audio$saveAudio)) dir.create(audio$saveAudio)
    filename = paste0(audio$saveAudio, '/', audio$filename_noExt, '.wav')
    writeAudio(soundFiltered, audio = audio, filename = filename)
  }
  return(soundFiltered)
}

