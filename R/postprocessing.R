#' Fade
#'
#' Applies fade-in and/or fade-out of variable length, shape, and steepness. The
#' resulting effect softens the attack and release of a waveform.
#'
#' @seealso \code{\link{crossFade}}
#'
#' @inheritParams .roxygen_defaults
#' @param fadeIn,fadeOut length of segments for fading in and out, ms (0 = no
#'   fade)
#' @param shape controls the type of fade function: 'lin' = linear, 'exp' =
#'   exponential, 'log' = logarithmic, 'cos' = cosine, 'logistic' = logistic
#'   S-curve, 'gaussian' = Gaussian curve
#' @param steepness scaling factor regulating the steepness of fading curves
#'   (except for shapes 'lin' and 'cos'): 0 = linear, >1 = steeper than default
#' @param plot if TRUE, produces an oscillogram of the waveform after fading
#' @return A numeric vector of the same length as input
#' @export
#' @examples
#' # Fading a real sound: say we want fast attack and slow release
#' s = soundgen(attackLen = 0, windowLength = 10,
#'              sylLen = 500, addSilence = 0)
#' # playme(s)
#' s1 = fade(s, fadeIn = 40, fadeOut = 350,
#'           samplingRate = 16000, shape = 'cos', plot = TRUE)
#' # playme(s1)
#'
#' # Illustration of fade shapes
#' x = runif(4000, min = -1, max = 1)  # make sure to zero-center input!!!
#' # plot(x, type = 'l')
#' y = fade(x, samplingRate = 1000, fadeIn = 1000, fadeOut = 0, plot = TRUE)
#' y = fade(x, samplingRate = 1000, fadeIn = 1000, fadeOut = 1500,
#'          shape = 'exp', steepness = 1, plot = TRUE)
#' y = fade(x, samplingRate = 1000, fadeIn = 1500, fadeOut = 500,
#'          shape = 'log', steepness = 1, plot = TRUE)
#' y = fade(x, samplingRate = 1000, fadeIn = 1500, fadeOut = 500,
#'          shape = 'log', steepness = 3, plot = TRUE)
#' y = fade(x, samplingRate = 1000, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'cos', plot = TRUE)
#' y = fade(x, samplingRate = 1000, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'logistic', steepness = 1, plot = TRUE)
#' y = fade(x, samplingRate = 1000, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'logistic', steepness = 3, plot = TRUE)
#' y = fade(x, samplingRate = 1000, fadeIn = 1500, fadeOut = 1500,
#'          shape = 'gaussian', steepness = 1.5, plot = TRUE)
#'
#' \dontrun{
#' fade('~/Downloads/temp', fadeIn = 500, fadeOut = 500, savePlots = TRUE)
#' }
fade = function(
    x,
    fadeIn = 50,
    fadeOut = 50,
    samplingRate = NULL,
    scale = NULL,
    shape = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian'),
    steepness = 1,
    reportEvery = NULL,
    cores = 1,
    saveAudio = FALSE,
    plot = FALSE,
    savePlots = FALSE,
    embed = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA
) {
  # match args
  shape = match.arg(shape)
  myPars = as.list(environment())
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'reportEvery', 'cores',
    'savePlots', 'saveAudio', 'embed')]

  pa = processAudio(x = x,
                    samplingRate = samplingRate,
                    scale = scale,
                    funToCall = '.fade',
                    suffix = 'fade',
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


#' Fade per sound
#' @param audio a list returned by \code{readAudio}
#' @param fadeIn_points,fadeOut_points length of segments for fading in and out,
#'   points (if specified, override \code{fadeIn/fadeOut})
#' @noRd
.fade = function(
    audio,
    fadeIn = 1000,
    fadeOut = 1000,
    fadeIn_points = NULL,
    fadeOut_points = NULL,
    samplingRate = NULL,
    shape = c('lin', 'exp', 'log', 'cos', 'logistic', 'gaussian'),
    steepness = 1,
    plot = FALSE,
    width = 900,
    height = 500,
    units = 'px',
    res = NA) {
  shape = match.arg(shape)

  if (steepness < 0) {
    steepness = 1
    warning('steepness must be non-negative; resetting to 1')
  } else if (steepness == 0) {
    shape = 'lin'
  }
  len = if (is.null(audio$ls)) length(audio$sound) else audio$ls

  # convert fade from ms to points
  if (is.null(fadeIn_points)) {
    if (is.null(fadeIn) || is.null(audio$samplingRate)) {
      fadeIn_points = 0
    } else {
      fadeIn_points = round(fadeIn * audio$samplingRate / 1000)
    }
  }
  if (is.null(fadeOut_points)) {
    if (is.null(fadeOut) || is.null(audio$samplingRate)) {
      fadeOut_points = 0
    } else {
      fadeOut_points = round(fadeOut * audio$samplingRate / 1000)
    }
  }

  # make sure fadeIn/Out region is no longer than the entire sound,
  # scale if they overlap so the fade-in/out meet in the middle of the sound
  fadeIn_points = max(0, round(fadeIn_points))
  fadeOut_points = max(0, round(fadeOut_points))
  if (fadeIn_points + fadeOut_points > len) {
    ratio = len / (fadeIn_points + fadeOut_points)
    fadeIn_points = round(fadeIn_points * ratio)
    fadeOut_points = len - fadeIn_points
  }

  # prepare a fade-in/out curve of requisite length
  time_in = seq(0, 1, length.out = fadeIn_points)
  time_out = seq(1, 0, length.out = fadeOut_points)
  if (shape == 'lin') {
    fi = time_in
    fo = time_out
  } else if (shape == 'exp') {
    m = exp(steepness * 3)  # to avoid taking min/max within zeroOne
    fi = zeroOne(exp(time_in * steepness * 3), xmin = 1, xmax = m)
    fo = zeroOne(exp(time_out * steepness * 3), xmin = 1, xmax = m)
  } else if (shape == 'log') {
    m = exp(steepness * 3)
    fi = 1 - rev(zeroOne(exp(time_in * steepness * 3), xmin = 1, xmax = m))
    fo = 1 - rev(zeroOne(exp(time_out * steepness * 3), xmin = 1, xmax = m))
  } else if (shape == 'cos') {
    fi = (1 - cos(time_in * pi)) / 2
    fo = (1 - cos(time_out * pi)) / 2
  } else if (shape == 'logistic') {
    xmin = 1 - 1 / (1 + exp(6 * steepness * (0 - .5)))
    xmax = 1 - 1 / (1 + exp(6 * steepness * (1 - .5)))
    fi = zeroOne(1 - 1 / (1 + exp(6 * steepness * (time_in - .5))),
                 xmin = xmin, xmax = xmax)
    fo = zeroOne(1 - 1 / (1 + exp(6 * steepness * (time_out - .5))),
                 xmin = xmin, xmax = xmax)
  } else if (shape == 'gaussian') {
    xmin = dnorm(x = -1, mean = 0, sd = .4 / steepness)
    xmax = dnorm(x = 0, mean = 0, sd = .4 / steepness)
    fi = zeroOne(dnorm(x = -rev(time_in), mean = 0, sd = .4 / steepness),
                 xmin = xmin, xmax = xmax)
    fo = rev(zeroOne(dnorm(x = time_out, mean = 0, sd = .4 / steepness),
                     xmin = xmin, xmax = xmax))
  }
  # plot(fi, type = 'l', xlim = c(1, max(fadeIn, fadeOut)))
  # points(fo, type = 'l', col = 'red')

  # apply the fade
  if (fadeIn_points > 0) {
    idx_in = 1:fadeIn_points
    audio$sound[idx_in] = audio$sound[idx_in] * fi
  }
  if (fadeOut_points > 0) {
    idx_out = (len - fadeOut_points + 1):len
    audio$sound[idx_out] = audio$sound[idx_out] * fo
  }

  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    .osc(audio[!names(audio) %in% c('savePlots')], maxPoints = Inf)
    # plot(audio$sound, type = 'l', xlab = '', ylab = '')
    abline(v = fadeIn_points / audio$samplingRate * 1000, col = 'blue')
    abline(v = (len - fadeOut_points)  / audio$samplingRate * 1000, col = 'blue')
  }
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(audio$sound, audio = audio, filename = filename)
  }
  audio$sound
}


#' Join two waveforms by cross-fading
#'
#' \code{crossFade} joins two input vectors (waveforms) by overlapping and
#' cross-fading them. This is standard for joining two separately generated
#' sounds without audible artifacts.
#'
#' @seealso \code{\link{fade}}
#'
#' @param x,y two numeric vectors (waveforms) to be joined
#' @param crossLen overlap in ms
#' @param crossLen_points overlap in points (overrides crossLen)
#' @param samplingRate the sampling rate of input vectors, Hz (needed only if
#'   crossLen is given in ms rather than points)
#' @param shape controls the type of fade function: 'equalPower' = equal power
#'   (default), 'lin' = linear, 'exp' = exponential, 'log' = logarithmic, 'cos'
#'   = cosine, 'logistic' = logistic S-curve, 'gaussian' = Gaussian curve
#' @param cutToZC if TRUE, input vectors are truncated so that \code{x} ends
#'   with a zero crossing and \code{y} starts with a zero crossing, both on
#'   an upward portion of the soundwave
#' @param steepness scaling factor regulating the steepness of fading curves
#'   (except for shapes 'lin' and 'cos'): 0 = linear, >1 = steeper than default
#' @param SNR if a number, the two sounds are normalized so that the RMS
#'   amplitude of \code{x} is SNR dB above that of \code{y} (NULL = no
#'   normalization; negative values make \code{x} quieter than \code{y})
#' @export
#' @return A numeric vector with the original sampling rate. The length is going
#'   to be \code{length(x) + length(y) - crossLen_points}, or a bit less if
#'   cutting to nearest zero crossings.
#' @examples
#' sound1 = sin(1:100 / 9)
#' sound2 = sin(7:106 / 3)
#' plot(c(sound1, sound2), type = 'b')
#' # an ugly discontinuity at 100 that will make an audible click
#'
#' sound = crossFade(sound1, sound2, crossLen_points = 25)
#' plot(sound, type = 'b') # a nice, smooth transition
#' length(sound) # but note that cross-fading costs us crossLen_points = 25
#' # points even without cutting to zc, and much more if we trim to
#' # zero crossings and then overlap
#'
#' sound = crossFade(sound1, sound2, crossLen_points = 25,
#'   SNR = 6)  # make sound1 6 dB louder (RMS) than sound2
#' plot(sound, type = 'b') # a nice, smooth transition
#'
#' # Actual sounds, alternative shapes of fade-in/out
#' sound3 = soundgen(formants = 'a', pitch = 200, sylLen = 300,
#'                   addSilence = 0, attackLen = c(50, 0))
#' sound4 = soundgen(formants = 'u', pitch = 200, sylLen = 300,
#'                   addSilence = 0, attackLen = c(0, 50))
#'
#' # simple concatenation (with a click)
#' cf1 = c(sound3, sound4)
#' osc(cf1, 16000, from = .25, to = .35)
#' # playme(cf1, 16000)
#'
#' # concatenation from zc to zc (no click, but a rough transition)
#' cf2 = crossFade(sound3, sound4, crossLen_points = 0, cutToZC = TRUE)
#' osc(cf2, 16000, from = .25, to = .35)
#' # playme(cf2, 16000)
#'
#' # crossFade over 35 ms - brief, but smooth
#' cf3 = crossFade(sound3, sound4, crossLen = 35, samplingRate = 16000)
#' osc(cf3, 16000, from = .25, to = .35)
#' # playme(cf3, 16000)
#'
#' # equal-power cross-fade over 300 ms (shortens the sound by ~300 ms)
#' cf4 = crossFade(sound3, sound4, samplingRate = 16000,
#'                  crossLen = 300, shape = 'equalPower')
#' osc(cf4, 16000)
#' # playme(cf4, 16000)
crossFade = function(
    x,
    y,
    crossLen = 15,
    crossLen_points = NULL,
    samplingRate = NULL,
    shape = c('equalPower', 'lin', 'exp', 'log', 'cos', 'logistic', 'gaussian'),
    cutToZC = FALSE,
    steepness = 1,
    SNR = NULL) {
  shape = match.arg(shape)
  len1 = length(x)
  len2 = length(y)

  # rescale y so that its RMS amplitude is SNR dB below x
  if (!is.null(SNR) && is.finite(SNR)) {
    a_x = sqrt(mean(x^2))
    a_y = sqrt(mean(y^2))
    if (a_y > 0)
      y = y * (a_x / a_y) / 10^(SNR/20)
  }

  if (cutToZC) {
    # cut to the nearest zero crossings
    zc1 = findZeroCrossing(x, location = len1)
    if (!is.na(zc1)) {
      # up to the last point before the last zero-crossing in sound 1 on the
      # upward curve + one extra zero (to have a nice, smooth transition line:
      # last negative in s1 - zero - first positive in s2)
      x = c(x[1:zc1], 0)
    }
    zc2 = findZeroCrossing(y, location = 1)
    if (!is.na(zc2)) {
      y = y[(zc2 + 1):len2]
      # from the first positive point on the upward curve. Note the +1 - next
      # point after the first zero crossing in s2
    }
  }

  # check whether there is enough data to cross-fade. Note that x or y may even
  # become shorter than crossLen_points after we shortened them to the nearest
  # zero crossing
  if (is.null(crossLen_points)) {
    if (!is.null(crossLen) && !is.null(samplingRate)) {
      crossLen_points = floor(crossLen * samplingRate / 1000)
    } else {
      stop('either crossLen + samplingRate or crossLen_points must be provided')
    }
  }
  crossLen_points = max(0, min(crossLen_points, len1 - 1, len2 - 1))

  # concatenate or cross-fade
  if (crossLen_points < 2) {
    # for segments that are too short,
    # just concatenate from zero crossing to zero crossing
    out = c(x, y)
  } else {
    # for segments that are long enough, cross-fade properly
    idx1 = len1 - crossLen_points
    if (shape == 'equalPower') {
      time_in = seq(0, 1, length.out = crossLen_points)
      fi = sin(pi/2 * time_in)
      fo = cos(pi/2 * time_in)
      cross = fo * x[(idx1 + 1):len1] +
        fi * y[1:crossLen_points]
    } else {
      multipl = .fade(list(sound = rep(1, crossLen_points), samplingRate = samplingRate),
                      fadeIn_points = crossLen_points,
                      fadeOut_points = 0,
                      shape = shape,
                      steepness = steepness)
      cross = rev(multipl) * x[(idx1 + 1):len1] +
        multipl * y[1:crossLen_points]
    }
    out = c(x[1:idx1],
             cross,
             y[(crossLen_points + 1):len2])
  }
  return(out)
}


#' Find zero crossing
#'
#' \code{findZeroCrossing} looks for the last negative point before a zero
#' crossing as close as possible to the specified location. Since this is
#' primarily intended for joining waveforms without a click, this function only
#' looks at upward segments of a waveform (see example).
#'
#' @param ampl a vector of amplitudes oscillating around zero, such as a sound
#'   waveform
#' @param location the index indicating the desired location of a zero crossing
#' @return The index of the last negative value before zero crossing closest to
#'   specified location.
#' @noRd
#' @examples
#' ampl = sin(1:100/2)
#' plot(ampl, type = 'b')
#' lines(1:100, rep(0,100), lty = 2)
#' zc = vector()
#' for (i in seq_along(ampl)){
#'   zc[i] = soundgen:::findZeroCrossing (ampl, i)
#'   # find zc closest to each of 100 points
#' }
#' for (z in unique(zc)){
#'   points(z, ampl[z], col = 'red', pch = 17)
#'   # only on upward segments
#' }
#' zc # see which zc is closest to each point
findZeroCrossing = function(ampl, location) {
  len = length(ampl)
  if (len < 1 || location < 1 || location > len)
    return(NA)
  if (len == 1 && location == 1)
    return(1)
  zc_left = zc_right = NA

  # left of location
  if (location > 1) {
    i = location
    while (i > 1) {
      if (ampl[i] > 0 && ampl[i - 1] < 0) {
        zc_left = i - 1
        break
      }
      i = i - 1
    }
  }

  # right of location
  if (location < len)
    i = location
  while (i < len) {
    if (ampl[i + 1] > 0 && ampl[i] < 0) {
      zc_right = i
      break
    }
    i = i + 1
  }

  if (is.na(zc_left) && is.na(zc_right)) return (NA)
  zc_nearest = which.min(abs(location - c(zc_left, zc_right)))
  if (zc_nearest == 1) {
    return(zc_left)
  } else if (zc_nearest == 2) {
    return(zc_right)
  } else {
    return(NA) # zc not found
  }
}


#' Reverb & echo
#'
#' Adds reverberation and/or echo to a sound parametrically or based on the
#' impulse response recorded in a particular real environment. Algorithm for
#' parametric reverb: adds time-shifted copies of the signal weighted by a decay
#' function. In simple terms: we specify how much and when the sound rebounds
#' back (e.g., from the walls of a room) and add these time-shifted copies to
#' the original - optionally, with some spectral filtering.
#' @inheritParams .roxygen_defaults
#' @param echoDelay the delay at which the echo appears, ms
#' @param echoLevel the rate at which the echo weakens at each repetition, dB
#'   (NULL = no echo)
#' @param reverbDelay the time of maximum reverb density, ms
#' @param reverbSpread standard deviation of reverb spread around time
#'   \code{reverbDelay}, ms
#' @param reverbLevel the maximum amplitude of reverb, dB below input (NULL = no
#'   reverb)
#' @param reverbDensity the number of echos or "voices" added
#' @param reverbType so far only "gaussian" has been implemented
#' @param IR recording of an impulse response in a particular environment -
#'   ideally, an instant loud click, but just clapping your hands or snapping
#'   your fingers also works as a first approximation
#' @param filter (optional) a spectral filter to apply to the created reverb and
#'   echo (see \code{addFormants} for acceptable formats)
#' @param dynamicRange the precision with which the reverb and echo are
#'   calculated, dB
#' @param output "audio" = returns just the processed audio, "detailed" =
#'   returns a list with reverb window, the added reverb/echo, etc.
#' @param seed if NULL, the algorithm is stochastic; provide a positive integer
#'   for reproducibility
#'
#' @return If \code{output = "audio"}, returns the processed waveform as a
#'   numeric vector. If \code{output = "detailed"}, returns a list: $rvb_win =
#'   Gaussian window used to create the reverberation, $rvb = reverberation
#'   effect, $echo = echo effect, $effect = both reverb and echo mixed together,
#'   $audio = processed audio.
#'
#' @export
#' @examples
#' data('speechEx', package = 'soundgen')
#' sr = speechEx@samp.rate
#' s = speechEx@left[1:16000]
#' # playme(s)
#'
#' # parametric reverb
#' s_rev = reverb(s, sr)
#' # playme(s_rev)
#'
#' # reverb of a particular real place based on its recorded IR
#' data('IR', package = 'soundgen')
#' s_ir = reverb(s, sr, IR = IR)
#' # playme(s_ir)
#'
#' \dontrun{
#' # double echo, no reverb
#' s1 = reverb(s, sr, reverbLevel = NULL,
#'             echoDelay = c(250, 800), echoLevel = c(-15, -25))
#' playme(s1)
#' spectrogram(s1, 16000, ylim = c(0, 4))
#'
#' # only reverb (indoors)
#' s2 = reverb(s, sr, echoDelay = NULL,
#'             reverbDelay = 70, reverbSpread = 130,
#'             reverbLevel = -20, reverbDensity = 20)
#' playme(s2)
#' spectrogram(s2, 16000, ylim = c(0, 4))
#'
#' # reverb (caves)
#' s3 = reverb(s, sr, echoDelay = NULL,
#'             reverbDelay = 600, reverbSpread = 1500,
#'             reverbLevel = -10, reverbDensity = 100)
#' playme(s3)
#' spectrogram(s3, 16000, ylim = c(0, 4))
#'
#' # both echo and reverb with high frequencies emphasized
#' s4 = reverb(s, sr,
#'             echoDelay = 250, echoLevel = -20,
#'             reverbDelay = 70, reverbSpread = 120,
#'             reverbLevel = -25, reverbDensity = 50,
#'             filter = list(formants = NULL, lipRad = 3))
#' playme(s4)
#' spectrogram(s4, 16000, ylim = c(0, 4))
#'
#' # recorded IR
#' s5 = soundgen(nSyl = 2, sylLen = 250, pitch = c(500, 300))
#' playme(s5)
#' spectrogram(s5, 16000, yScale = 'ERB')
#' s5r = reverb(s5, 16000, IR = IR)
#' playme(s5r)
#' spectrogram(s5r, 16000, yScale = 'ERB')
#'
#' # add reverb to all files in a folder, save the result
#' reverb('~/Downloads/temp', saveAudio = TRUE)
#' }
reverb = function(x,
                  samplingRate = NULL,
                  echoDelay = 200,
                  echoLevel = -20,
                  reverbDelay = 70,
                  reverbSpread = 130,
                  reverbLevel = -25,
                  reverbDensity = 50,
                  reverbType = 'gaussian',
                  IR = NULL,
                  filter = list(),
                  dynamicRange = 80,
                  output = c('audio', 'detailed'),
                  play = FALSE,
                  saveAudio = FALSE,
                  reportEvery = NULL,
                  cores = 1,
                  seed = NULL) {
  # match args
  output = match.arg(output)
  myPars = c(as.list(environment()))
  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'reportEvery', 'cores', 'saveAudio')]
  pa = processAudio(x,
                    samplingRate = samplingRate,
                    funToCall = '.reverb',
                    suffix = 'reverb',
                    saveAudio = saveAudio,
                    myPars = myPars,
                    reportEvery = reportEvery,
                    cores = cores)

  # prepare output
  if (pa$input$n == 1) {
    result = pa$result[[1]]
  } else {
    result = pa$result
  }
  if (output == 'audio') {
    if (pa$input$n == 1) {
      result = result$audio
    } else {
      result = lapply(result, function(x) x$audio)
    }
  }
  invisible(result)
}


#' Add reverb to a sound
#' @noRd
.reverb = function(audio,
                   echoDelay = 200,
                   echoLevel = -20,
                   reverbDelay = 70,
                   reverbSpread = 130,
                   reverbLevel = -25,
                   reverbDensity = 50,
                   reverbType = 'gaussian',
                   IR = NULL,
                   filter = list(),
                   dynamicRange = 80,
                   output = 'audio',
                   play = FALSE,
                   seed = NULL) {
  win = rvb = echo = effect = numeric(0)
  if (!is.null(seed) && seed > 0) set.seed(seed)

  # user-provided impulse responses overrides all echo and reverb settings
  if (!is.null(IR)) {
    # overrides all echo and reverb settings
    reverbLevel = NA  # no parametric reverb
    echoLevel = NA    # no parametric echo
    # try to read IR
    ir = readAudio(IR)
    # resample to the same rate as target audio if they are not the same
    if (audio$samplingRate != ir$samplingRate)
      ir$sound = .resample(ir, samplingRate_new = audio$samplingRate)
    # apply reverb based on IR
    audio$sound = signal::fftfilt(ir$sound, audio$sound)
    # re-normalize
    audio$sound = audio$sound / max(abs(audio$sound)) * audio$scale_used
  }

  ## reverb
  addRvb = FALSE
  if (is.numeric(reverbDelay) && is.numeric(reverbLevel)) {
    if (reverbLevel > 0) stop('reverbLevel must be negative')
    addRvb = TRUE
    if (reverbType == 'gaussian') {
      # Gaussian over 3 SD
      rvb_len_ms = reverbDelay + 3 * reverbSpread
      nFr_rvb = round(rvb_len_ms * audio$samplingRate / 1000)
      win_mean = reverbDelay * audio$samplingRate / 1000
      win_sd = reverbSpread * audio$samplingRate / 1000
      win = dnorm(1:nFr_rvb, mean = win_mean, sd = win_sd)
      max_win = dnorm(win_mean, win_mean, win_sd)  # density at mean
      # win = win / max(win) * dynamicRange - dynamicRange + reverbLevel
      win = win / max_win * 120 - 120 + reverbLevel
      # 120 dB is used to set up the slope of decay, otherwise it would depend
      # on dynamicRange

      # "discretize" the win - only keep a few delay points
      idx_keep = sort(sample(1:nFr_rvb, size = min(nFr_rvb, reverbDensity)))
      # only keep as much of win as exceeds dynamicRange (don't bother to add
      # very quiet reverb)
      idx_keep = idx_keep[win[idx_keep] > (-dynamicRange)]
      if (length(idx_keep) == 0) {
        # nothing to do
        addRvb = FALSE
      } else {
        nFr_rvb = tail(idx_keep, 1)
      }
      win = 10 ^ (win / 20)  # from dB to linear
      # plot(win, type = 'l')
    } else {
      stop("Only reverbType = 'gaussian' has been implemented so far")
      # exponential decay: halves (-6 dB) every reverbDelay
      n_halves = min(4, round((dynamicRange + reverbLevel) / 6))
      rvb_len_ms = n_halves * reverbDelay
      nFr_rvb = ceiling(rvb_len_ms * audio$samplingRate / 1000)
      len_halflife = ceiling(reverbDelay * audio$samplingRate / 1000)
      reverbLevel_lin = 10 ^ (reverbLevel / 20)
      win = 2 ^ (-(1:nFr_rvb) / len_halflife) * reverbLevel_lin
      # plot(win, type = 'l')

      # add some noise
      if (FALSE) {
        # rvb_wiggle = getRandomWalk(len = nFr_rvb,
        #   rw_range = reverbSpread * reverbLevel_lin * 2) - reverbSpread * reverbLevel_lin
        lw = rvb_len_ms / 2  # relatively smooth wiggle (1 value per 2 ms)
        # sd_w = seq(reverbSpread, reverbSpread / n_halves, length.out = lw) * win[1]
        rvb_wiggle = rnorm(lw, mean = 0, sd = sd_w)
        rvb_wiggle = approx(rvb_wiggle, n = nFr_rvb)$y
        # plot(rvb_wiggle, type = 'l')
        win = win + rvb_wiggle
        # plot(win, type = 'l')
      }
    }
  }
  # create reverb by adding up time-shifted signal weighted by decay window
  if (addRvb) {
    rvb = rep(0, audio$ls + nFr_rvb - 1)
    for (i in idx_keep) {
      idx_i = i:(i + audio$ls - 1)
      rvb[idx_i] = rvb[idx_i] + audio$sound * win[i]
    }
    # playme(rvb)
    # spectrogram(rvb, 16000, ylim = c(0, 4))
    # rvb = rvb_pad[1:len]  # range(rvb)
  } else {
    rvb = numeric(0)
  }

  ## echo
  addEcho = all(is.numeric(echoLevel)) &&
    any(echoLevel > (-dynamicRange)) &&
    any(echoDelay > 0)
  if (addEcho) {
    if (any(echoLevel > 0)) stop('echoLevel must be negative')
    le = length(echoDelay)
    if (le > 1 && length(echoLevel) == 1) echoLevel = rep(echoLevel, le)
    echo = NULL
    for (e in 1:le) {
      nFr_echo = dynamicRange / (-echoLevel[e])
      step_echo = ceiling(echoDelay[e] * audio$samplingRate / 1000)
      echo_e = rep(0, audio$ls + nFr_echo * step_echo - 1)
      for (i in 1:nFr_echo) {
        idx_start = i * step_echo
        idx_i = idx_start:(idx_start + audio$ls - 1)
        echo_e[idx_i] = echo_e[idx_i] + audio$sound * 10 ^ (echoLevel[e] * i / 20)
      }
      if (is.null(echo)) {
        echo = echo_e
      } else {
        echo = addVectors(echo, echo_e, normalize = FALSE)
      }
    }
    # playme(echo)
  } else {
    echo = numeric(0)
  }

  if (addRvb || addEcho) {
    effect = addVectors(rvb, echo, normalize = FALSE)
    if (length(filter) > 1) {
      effect = do.call(.addFormants, c(list(
        audio = readAudio(effect, samplingRate = audio$samplingRate),
        normalize = 'orig'),
        filter)
      )
      # playme(effect, audio$samplingRate)
      # spectrogram(effect, audio$samplingRate, ylim = c(0, 4))
    }

    out = addVectors(audio$sound, effect) * audio$scale_used
    # playme(out, audio$samplingRate)
    # spectrogram(out, audio$samplingRate, ylim = c(0, 4))
  } else {
    out = audio$sound
  }

  if (isTRUE(play)) {
    playme(out, audio$samplingRate)
  } else if (is.character(play)) {
    playme(out, audio$samplingRate, player = play)
  }

  # save audio
  if (isTRUE(audio$saveAudio)) {
    filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".wav"))
    writeAudio(out, audio = audio, filename = filename)
  }

  list(
    rvb_win = win,
    rvb = rvb,
    echo = echo,
    effect = effect,
    audio = out
  )
}
