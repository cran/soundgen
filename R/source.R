#' Generate noise
#'
#' Generates noise of length \code{len} and with spectrum defined by rolloff
#' parameters OR by a specified filter \code{formantFilter}. This function is
#' called internally by \code{\link{soundgen}}, but it may be more convenient to
#' call it directly when synthesizing non-biological noises defined by specific
#' spectral and amplitude envelopes rather than formants: the wind, whistles,
#' impact noises, etc. See \code{\link{beat}} for similarly simplified functions
#' for tonal non-biological sounds.
#'
#' Algorithm: paints a spectrogram with desired characteristics, sets phase to
#' zero, and generates a time sequence via inverse FFT.
#'
#' @seealso \code{\link{soundgen}} \code{\link{beat}}
#'
#' @inheritParams soundgen
#' @param len length of output, samples
#' @param formantFilter (optional): as an alternative to using rolloffNoise,
#'   we can provide the exact filter - a vector of non-negative numbers
#'   specifying the desired spectrum on a linear scale up to Nyquist frequency.
#'   The length doesn't matter as it can be interpolated internally. A matrix
#'   specifying time-varying filter for each STFT step is also accepted:
#'   frequencies in rows, STFT frames in columns. The easiest way to obtain
#'   formantFilter is to call \link{getFormantFilter} or to use (smoothed)
#'   spectrum / spectrogram of an existing sound
#'
#' @return The generated waveform as a numeric vector.
#' @keywords internal
#' @examples
#' # .5 s of white noise
#' samplingRate = 16000
#' noise1 = soundgen:::generateNoise(len = samplingRate * .5,
#'   samplingRate = samplingRate)
#' meanSpectrum(noise1, samplingRate)
#' # playme(noise1, samplingRate)
#'
#' # Percussion (run a few times to notice stochasticity due to temperature = .25)
#' noise2 = soundgen:::generateNoise(len = samplingRate * .15, noise = c(0, -80),
#'   rolloffNoise = c(4, -6), attackLen = 5)
#' noise3 = soundgen:::generateNoise(len = samplingRate * .25, noise = c(0, -40),
#'   rolloffNoise = c(4, -20), attackLen = 5)
#' # playme(c(noise2, noise3), samplingRate)
#'
#' \dontrun{
#' playback = list(TRUE, FALSE, 'aplay', 'vlc')[[1]]
#' # 1.2 s of noise with rolloff changing from 0 to -12 dB above 2 kHz
#' noise = generateNoise(len = samplingRate * 1.2,
#'   rolloffNoise = c(0, -12), noiseFlatSpec = 2000,
#'   samplingRate = samplingRate, play = playback)
#' # spectrogram(noise, samplingRate)
#'
#' # Similar, but using the dataframe format to specify a more complicated
#' # contour for rolloffNoise:
#' noise = generateNoise(len = samplingRate * 1.2,
#'   rolloffNoise = data.frame(time = c(0, .3, 1), value = c(-12, 0, -12)),
#'   noiseFlatSpec = 2000, samplingRate = samplingRate, play = playback)
#' # spectrogram(noise, samplingRate)
#'
#' # To create a sibilant [s], specify a single strong, broad formant at ~7 kHz:
#' wl = 1024
#' formantFilter = getFormantFilter(
#'   nr = wl %/% 2 + 1, nc = 1, samplingRate = samplingRate,
#'  formants = list('f1' = data.frame(time = 0, freq = 7000,
#'                                    amp = 50, width = 2000)))
#' noise = fade(generateNoise(len = samplingRate,
#'   samplingRate = samplingRate, formantFilter = as.numeric(formantFilter),
#'   play = playback), samplingRate = samplingRate)
#' # plot(formantFilter, type = 'l')
#' meanSpectrum(noise, samplingRate)
#'
#' # Low-frequency, wind-like noise
#' formantFilter = getFormantFilter(
#'   nr = 50, nc = 1, lipRad = 0,
#'   samplingRate = samplingRate, formants = list('f1' = list(
#'     freq = 250, amp = 30, width = 150)),
#'     formantDepStoch = 0, plot = TRUE)
#' noise = fade(generateNoise(len = samplingRate,
#'   samplingRate = samplingRate, formantFilter = as.numeric(formantFilter),
#'   play = playback))
#' spectrogram(noise, samplingRate, ylim = c(0, 2))
#'
#' # Manual filter, e.g. for a kettle-like whistle (narrow-band noise)
#' formantFilter = c(rep(0, 100), 120, rep(0, 100))  # any length is fine
#' # plot(formantFilter, type = 'b')  # narrow-band filter at Nyquist / 2, here 4 kHz
#' noise = fade(generateNoise(len = samplingRate, formantFilter = formantFilter,
#'   samplingRate = samplingRate, play = playback))
#' spectrogram(noise, samplingRate)
#'
#' # Compare to a similar sound created with soundgen()
#' # (aperiodic noise only, a single formant at 4 kHz)
#' noise_s = soundgen(pitch = NULL,
#'   noise = data.frame(time = c(0, 1000), value = c(0, 0)),
#'   formants = list(f1 = data.frame(freq = 4000, amp = 80, width = 20)),
#'   play = playback)
#' }
generateNoise = function(
    len,
    rolloffNoise = -4,
    noiseFlatSpec = 1200,
    rolloffNoiseExp = 0,
    formantFilter = NULL,
    noise = NULL,
    attackLen = 10,
    samplingRate = 16000,
    windowLength = 50,
    step = NULL,
    overlap = 75,
    wn = "gaussian",
    smoothing = list(),
    play = FALSE) {
  if (len < 2) return(numeric(len))
  noise = reformatAnchors(noise)
  val = validateWlOvlp(list(samplingRate = samplingRate, dur = len/samplingRate),
                       windowLength, step, overlap)
  step_points = val$step_points; wl = val$wl

  # convert anchors to a smooth contour of waveform amplitudes
  if (is.list(noise)) {
    waveformStrength = do.call(getSmoothContour, c(smoothing, list(
      anchors = noise,
      len = len,
      valueFloor = permittedValues['noiseAmpl', 'low'],
      valueCeiling = permittedValues['noiseAmpl', 'high']
    )))
    # convert anchor amplitudes from dB to linear multipliers
    waveformStrength = 10 ^ (waveformStrength / 20)
    if (sum(is.na(waveformStrength)) > 0) {
      return(rep(0, len))
    }
    # plot(waveformStrength)
  } else {
    waveformStrength = rep(1, len)
  }

  # set up spectral filter
  nr = wl %/% 2 + 1 # +1 if using istft_simple()
  nc = max(1, ceiling((len - wl) / step_points) + 1)
  # NB: add an extra wl - otherwise, shorter than needed after iSTFT
  if (is.null(formantFilter)) {
    # basic linear rolloff above noiseFlatSpec Hz
    freqs = (seq_len(nr) - 1) * samplingRate / wl
    deltas = pmax(0, freqs - noiseFlatSpec) / 1000
    if (is.list(rolloffNoise) ||
        (is.numeric(rolloffNoise) && length(rolloffNoise) > 1)) {
      # Johnson_2012_Acoustic-and-Auditory-Phonetics, Fig. 7.1:
      # spectrum of aspiration noise drops linearly beyond ~ 1 kHz
      rolloffNoise = do.call(getSmoothContour, c(smoothing, list(
        anchors = rolloffNoise,
        len = nc,
        valueFloor = permittedValues['rolloffNoise', 'low'],
        valueCeiling = permittedValues['rolloffNoise', 'high']
      )))
      formantFilter = 10 ^ outer(deltas, rolloffNoise / 20)

    } else {
      # rolloffNoise is a scalar
      formantFilter = matrix(10 ^ outer(deltas, rolloffNoise / 20),
                             nrow = nr, ncol = nc)
    }

    # exponential rolloff starting from noiseFlatSpec Hz (Klatt & Klatt, 1990)
    if ((is.list(rolloffNoiseExp) && any(rolloffNoiseExp$value != 0)) ||
        (is.numeric(rolloffNoiseExp) && any(rolloffNoiseExp != 0))) {
      n_octaves = log2(pmax(1, freqs / noiseFlatSpec))  # 0 under noiseFlatSpec
      if ((is.list(rolloffNoiseExp) && length(rolloffNoiseExp$value) > 1) ||
          (is.numeric(rolloffNoiseExp) && length(rolloffNoiseExp) > 1)) {
        rolloffNoiseExp = do.call(getSmoothContour, c(smoothing, list(
          anchors = rolloffNoiseExp,
          len = nc,
          valueFloor = permittedValues['rolloffNoiseExp', 'low'],
          valueCeiling = permittedValues['rolloffNoiseExp', 'high']
        )))
        k = log(10) / 20
        formantFilter = formantFilter * exp(outer(
          n_octaves, rolloffNoiseExp * k, `*`))
        # faster than a loop or 20^outer(...)
      } else {
        r_exp = 10 ^ (n_octaves * rolloffNoiseExp / 20)
        formantFilter = formantFilter * r_exp
      }
    }
    # image(t(formantFilter))
  } else {
    # user-specified exact spectral envelope
    if (any(is.na(formantFilter))) stop('missing values in formantFilter')
    if (is.vector(formantFilter)) {
      if (length(formantFilter) != nr) {
        # interpolate to correct freq resolution
        formantFilter = do.call(getSmoothContour, c(smoothing, list(
          anchors = formantFilter, len = nr)))
        # formantFilter = approx(formantFilter, n = nr, method = 'linear')$y
      }
      formantFilter = matrix(formantFilter, nrow = nr, ncol = nc)
    } else {  # interpolate the matrix
      if (ncol(formantFilter) != nc || nrow(formantFilter) != nr) {
        message('Incorrect dimensions of formantFilter matrix. Interpolating...')
        # bilinear interpolation of formantFilter
        formantFilter = interpolMatrix(formantFilter,
                                       nr = nr, nc = nc,
                                       interpol = 'approx')
      }
    }
  }
  # image(t(formantFilter))
  # plot(formantFilter[, 1], type = 'l')
  # plot(log10(formantFilter[, 1]) * 20, type = 'l')

  if (sum(formantFilter) == 0) {
    # zero filter - nothing to synthesize
    warning('These settings will result in silence!')
    waveform = rep(0, len)
  } else {
    ## instead of synthesizing the time series and then doing stft-istft,
    # we can simply synthesize spectral noise, convert to complex
    # (setting imaginary=0 or random), and then do inverse STFT just once

    # set up a spectrogram with complex Gaussian noise
    len_mat = nr * nc
    z1 = matrix(
      complex(real = rnorm(len_mat),
              imaginary = rnorm(len_mat)),
      nrow = nr,
      ncol = nc
    )
    z1[1, ] = 0  # avoid random drift of DC
    # or, to keep it: z1[1, ] = Re(z1[1, ])
    if (wl %% 2 == 0) {
      z1[nr, ] = Re(z1[nr, ])  # should be real at Nyquist
    }

    # multiply by filter
    z1_filtered = z1 * formantFilter
    # phase = guessPhase_spsi(formantFilter, wl, step_points)
    # z1_filtered = formantFilter * exp(1i * phase) # produces clicks instead of steady noise
    # image(t(Mod(z1_filtered)))

    # do inverse FFT
    waveform = istft_simple(z1_filtered, wl = wl,
                            step = step_points, wn = wn)
    waveform = matchLengths(waveform, len = len)  # pad with 0s or trim
    waveform = waveform / max(abs(waveform)) * waveformStrength # normalize

    # add attack
    if (is.numeric(attackLen)) {
      if (any(attackLen > 0)) {
        l = floor(attackLen * samplingRate / 1000)
        if (length(l) == 1) l = c(l, l)
        waveform = .fade(
          list(sound = waveform, samplingRate = samplingRate),
          fadeIn_points = l[1],
          fadeOut_points = l[2]
        )
      }
    }
  }

  if (isTRUE(play)) {
    playme(waveform, samplingRate = samplingRate)
  } else if (is.character(play)) {
    playme(waveform, samplingRate = samplingRate, player = play)
  }
  # spectrogram(waveform, samplingRate = samplingRate)
  # meanSpectrum(waveform, samplingRate, windowLength = windowLength, yScale = 'max0')
  invisible(waveform)
}



#' Generate harmonics
#' Returns one continuous, unfiltered, voiced syllable consisting of several
#' sine waves.
#' @param pitch a contour of fundamental frequency (numeric vector). NB: for
#'   computational efficiency, provide the pitch contour at a reduced sampling
#'   rate pitchSamplingRate, eg 3500 points/s. The pitch contour will be
#'   upsampled before synthesis.
#' @param specEnv a matrix representing the filter (only needed for formant
#'   locking)
#' @param pitchDriftDep scale factor regulating the effect of temperature on the
#'   amount of slow random drift of f0 (like jitter, but slower): the higher,
#'   the more f0 "wiggles" at a given temperature
#' @param pitchDriftFreq scale factor regulating the effect of temperature on
#'   the frequency of random drift of f0 (like jitter, but slower): the higher,
#'   the faster f0 "wiggles" at a given temperature
#' @param amplDriftDep drift of amplitude mirroring pitch drift
#' @param subDriftDep drift of subharmonic frequency and bandwidth mirroring
#'   pitch drift
#' @param rolloffDriftDep drift of rolloff mirroring pitch drift
#' @param randomWalk_trendStrength try 0 to 1 - the higher, the more likely rw
#'   is to get high in the middle and low at the beginning and end (i.e. max
#'   effect amplitude in the middle of a sound)
#' @param rolloff_perAmpl as amplitude goes down from max to
#'   \code{-dynamicRange}, \code{rolloff} increases by \code{rolloff_perAmpl}
#'   dB/octave. The effect is to make loud parts sound brighter by increasing
#'   energy in higher frequencies
#' @param normalize if TRUE, normalizes to -1...+1 prior to applying the
#'   amplitude envelope. W/o this, sounds with stronger harmonics are louder
#' @noRd
#' @examples
#' rolloffExact1 = c(.2, .2, 1, .2, .2)
#' s1 = soundgen:::generateHarmonics(pitch = seq(400, 530, length.out = 1500),
#'                        rolloffExact = rolloffExact1)
#' spectrogram(s1, 16000, ylim = c(0, 4))
#' # playme(s1, 16000)
#'
#' rolloffExact2 = matrix(c(.2, .2, 1, .2, .2,
#'                          1, .5, .2, .1, .05), ncol = 2)
#' s2 = soundgen:::generateHarmonics(pitch = seq(400, 530, length.out = 1500),
#'                        rolloffExact = rolloffExact2)
#' spectrogram(s2, 16000, ylim = c(0, 4))
#' # playme(s2, 16000)
generateHarmonics = function(pitch,
                             glottis = 0,
                             attackLen = 50,
                             nonlinBalance = 0,
                             nonlinRandomWalk = NULL,
                             jitterDep = 0,
                             jitterLen = 1,
                             vibratoFreq = 5,
                             vibratoDep = 0,
                             shimmerDep = 0,
                             shimmerLen = 1,
                             rolloff = -9,
                             rolloffOct = 0,
                             rolloffKHz = 0,
                             rolloff_perAmpl = 0,
                             rolloffExact = NULL,
                             formantLocking = NULL,
                             specEnv = NULL,
                             formantSummary = NULL,
                             temperature = .025,
                             pitchDriftDep = .5,
                             pitchDriftFreq = .125,
                             amplDriftDep = 1,
                             subDriftDep = 4,
                             rolloffDriftDep = 3,
                             randomWalk_trendStrength = .1,
                             shortestEpoch = 300,
                             subRatio = 1,
                             subDep = 0,
                             ampl = NA,
                             normalize = TRUE,
                             smoothing = list(),
                             samplingRate = 16000,
                             pitchFloor = 75,
                             pitchCeiling = 3500,
                             pitchSamplingRate = 3500,
                             dynamicRange = 80) {
  ## PRE-SYNTHESIS EFFECTS (NB: the order in which effects are added is NOT arbitrary!)
  # vibrato (performed on pitch, not pitch_per_gc!)
  if (is.list(vibratoDep)) {
    if (any(vibratoDep$value > 0)) {
      lp = length(pitch)
      for (p in c('vibratoFreq', 'vibratoDep')) {
        old = get(p)
        if (length(old) > 1) {
          new = do.call(getSmoothContour, c(smoothing, list(
            anchors = old,
            len = lp,
            valueFloor = permittedValues[p, 'low'],
            valueCeiling = Inf  # permittedValues[p, 'high'],
          )))
          assign(p, new)
        }
      }
      vibrato = 2 ^ (sin(2 * pi * cumsum(vibratoFreq) /
                           pitchSamplingRate) * vibratoDep / 12)
      # plot(vibrato, type = 'l')
      pitch = pitch * vibrato  # plot(pitch, type = 'l')
    }
  }

  # sometimes add a pair of pitch jumps if temperature is very high
  if (temperature > .25) {
    add_jump = rbinom(1, 1, temperature)
    if (add_jump) {
      pitch = addPitchJumps(
        pitch,
        nj = 1,
        magn = rbeta(1, 1, 3) * 12,  # hist(rbeta(1000, 1, 3) * 12)
        prop = rbeta(1, 1, 4)
      )
    }
  }

  # transform f0 per s to f0 per glottal cycle
  gc = getGlottalCycles(pitch, samplingRate = pitchSamplingRate)  # our "glottal cycles"
  pitch_per_gc = pitch[gc]
  nGC = length(pitch_per_gc)
  # to avoid recalculating gc for each contour like jitterDep,
  # we can upsample them to length nGC and just take gc indices
  # scaled to length nGC instead of length(pitch)
  gc1 = ceiling(gc * nGC / length(pitch))

  # vectorized par-s should be upsampled and converted from ms to gc scale
  update_pars = c(
    'rolloff', 'rolloffOct', 'rolloffKHz',
    'jitterDep', 'jitterLen', 'shimmerDep', 'shimmerLen',
    'subRatio', 'subDep'
  )
  for (p in update_pars) {
    old = get(p)
    if (length(old) > 1) {
      new = do.call(getSmoothContour, c(smoothing, list(
        anchors = old,
        len = nGC,
        valueFloor = permittedValues[p, 'low'],
        valueCeiling = permittedValues[p, 'high'])))[gc1]
      assign(p, new)
    }
  }

  # generate a short amplitude contour to adjust rolloff per glottal cycle
  if (is.list(ampl) && any(ampl$value != 0)) {
    amplContour = do.call(getSmoothContour, c(smoothing, list(
      anchors = ampl,
      len = nGC,
      valueFloor = -dynamicRange,
      valueCeiling = 0,
      samplingRate = samplingRate
    )))
    # plot(amplContour, type='l')
    amplContour = (amplContour + dynamicRange) / dynamicRange - 1
    rolloffAmpl = amplContour * rolloff_perAmpl
  } else {
    rolloffAmpl = rep(0, nGC)
  }

  # get a random walk for intra-syllable variation
  if (temperature > 0 &&
      (nonlinBalance < 100 || !is.null(nonlinRandomWalk))) {
    rw = getRandomWalk(
      len = nGC,
      rw_range = temperature,
      trend = c(randomWalk_trendStrength, -randomWalk_trendStrength),
      rw_smoothing = .95
    ) # plot(rw, type = 'l')
    rw = rw - mean(rw) + 1 # change mean(rw) to 1
    if (is.null(nonlinRandomWalk)) {
      rw_0_100 = zeroOne(rw) * 100
      # plot(rw_0_100, type = 'l')
      rw_bin = getIntegerRandomWalk(
        rw_0_100,
        nonlinBalance = nonlinBalance,
        minLength = ceiling(shortestEpoch / 1000 * pitch_per_gc)
        # minLength is shortestEpoch / period_ms, where
        #   period_ms = 1000 / pitch_per_gc
      )
    } else {
      # interpolate or downsample user-provided random walk by simple repetition,
      # so that its new length = nGC (overrides shortestEpoch)
      rw_bin = approx(nonlinRandomWalk, n = nGC, method = 'constant')$y
      # plot(rw_bin)
    }
    subh_on = (rw_bin > 0) # when are subh on? For ex., rw_bin==1
    #   turs on subh only in regime 1, while rw_bin>0 turns on subh
    #   in regimes 1 and 2 (i.e. together with jitter)
    jitter_on = shimmer_on = (rw_bin == 2)
  } else {
    rw = rep(1, nGC)
    subh_on = jitter_on = shimmer_on = rep(TRUE, nGC)
  }

  ## prepare the harmonic stack
  # calculate the number of harmonics to generate (from lowest pitch to Nyquist)
  nHarmonics = floor(samplingRate / 2 / min(pitch_per_gc))
  # nHarmonics = floor(samplingRate / 2 / max(pitch_per_gc))
  # set to max(pitch) to have ~const n_harmonics throughout the sound
  # (ampl depends on nHarmonics, so changing it also modulates the ampl)

  if (is.null(rolloffExact)) {
    # get rolloff
    if (length(rolloff) < nGC) {
      rolloff = spline(rolloff, n = nGC)$y
    }
    rolloff_source = getRolloff(
      pitch_per_gc = pitch_per_gc,
      nHarmonics = nHarmonics,
      rolloff = (rolloff + rolloffAmpl) * rw ^ rolloffDriftDep,
      rolloffOct = rolloffOct,
      rolloffKHz = rolloffKHz,
      samplingRate = samplingRate,
      dynamicRange = dynamicRange
    )
  } else {
    rolloff_user = as.matrix(rolloffExact)
    rolloff_source = interpolMatrix(
      rolloff_user,
      nr = nrow(rolloff_user),  # don't change the number of harmonics
      nc = nGC,                 # interpolate over time
      interpol = 'approx'
    )
    if (is.null(rownames(rolloff_source)))
      rownames(rolloff_source) = 1:nrow(rolloff_source)
  }
  # NB: rolloff_source at this stage should be a matrix with numbered rows

  # add formantLocking
  if (!is.null(formantLocking) && !is.null(specEnv) && !is.null(formantSummary)) {
    shortestEpoch_points = shortestEpoch / 1000 * samplingRate
    median_gc_points = samplingRate / median(pitch)
    pitch_per_gc = lockToFormants(
      pitch = pitch_per_gc,
      specEnv = specEnv,
      formantSummary = formantSummary,
      rolloffMatrix = rolloff_source,
      lockProb = formantLocking,
      minLength = round(shortestEpoch_points / median_gc_points),
      plot = FALSE
    )
    # if we want to be super-precise, we could actually recalculate rolloff_source
  }

  # calculate jitter (random variation of F0)
  if (any(jitterDep > 0) && any(jitter_on)) {
    jitter_per_gc = wiggleGC(dep = jitterDep / 12,
                             len = jitterLen,
                             nGC = nGC,
                             pitch_per_gc = pitch_per_gc,
                             rw = rw,
                             effect_on = jitter_on)
    pitch_per_gc = pitch_per_gc * jitter_per_gc
    # plot(pitch_per_gc, type = 'l')
  }

  # calculate random drift of F0 (unlike jitter, this is a random walk rather
  # than random variation around a target value, and normally slower than
  # jitter)
  if (temperature > 0) {
    # calculate the amount of smoothing to apply to the random walk
    rw_smoothing = 2 / (1 + exp(100 * temperature * pitchDriftFreq))
    # print(rw_smoothing)
    # temp = seq(0, 1, .01)
    # plot(temp, 2 / (1 + exp(100 * temp * .05)))

    # rw_smoothing is ~n_points in getRandomWalk() as proportion of nGC, but
    # gc's are shorter at higher pitch; to ensure that smoothing is consistent
    # per s, we do * mean(pitch_per_gc) / 100 (ie default at 100 Hz)
    rw_smoothing = 1 - (1 - rw_smoothing) / (mean(pitch_per_gc) / 100)

    # rw_range is 1 semitone per second when temp = .05 and
    # pitchDriftDep = .5 (defaults)
    rw_range = temperature * 40 *  # 40 * .05 * .5 = 1
      length(pitch) / pitchSamplingRate / 12
    drift = getRandomWalk(
      len = length(pitch_per_gc),
      rw_range = rw_range,
      rw_smoothing = rw_smoothing,
      method = 'spline'
    )
    drift_centered = drift - mean(drift)
    drift = 2 ^ drift_centered
    # plot(drift, type = 'l')
    drift_pitch = 2 ^ (drift_centered * pitchDriftDep)
    pitch_per_gc = pitch_per_gc * drift_pitch
    # plot(pitch_per_gc, type = 'l')
  }

  # as a result of adding pitch effects, f0 might have dropped to very low
  # values, so we double-check and flatten if necessary
  pitch_per_gc[pitch_per_gc > pitchCeiling] = pitchCeiling
  pitch_per_gc[pitch_per_gc < pitchFloor] = pitchFloor

  # "glottis" (closed phase)
  targetLen = NULL
  if (is.list(glottis) && any(glottis$value > 0)) {
    glottisClosed_per_gc = do.call(getSmoothContour, c(smoothing, list(
      anchors = glottis,
      len = nGC,
      valueFloor = 0
    )))
    if (var(glottis$value) > .Machine$double.eps) {
      # if we are going to add time-variable closed phase, the pitch contour needs
      # to compensate for it (otherwise, we'll get spurious changes in f0)
      closedFactor = pmax(1, 1 + glottisClosed_per_gc / 100)
      periods_points = samplingRate / pitch_per_gc
      targetLen = round(sum(periods_points))
      K = sum(periods_points * closedFactor) / targetLen
      pitch_per_gc = pitch_per_gc * closedFactor / K
    }
  } else {
    glottisClosed_per_gc = NULL
  }

  # make sure we don't have harmonics above Nyquist with the changed pitch
  nHarmonics = floor(samplingRate / 2 / pitch_per_gc)
  nr = nrow(rolloff_source)
  for (c in 1:ncol(rolloff_source)) {
    if (nHarmonics[c] < nr) {
      rolloff_source[(nHarmonics[c] + 1):nr, c] = 0
    }
  }
  # nHarmonics = min(nr, floor(samplingRate / 2 / max(pitch_per_gc)))
  # rolloff_source = rolloff_source[seq_len(nHarmonics), ]

  # NB: this whole pitch_per_gc trick is purely for computational efficiency.
  #   The entire pitch contour can be fed in, but that's much slower
  # image(t(log(rolloff_source)))

  # add shimmer (random variation in amplitude)
  if (any(shimmerDep > 0) && any(shimmer_on)) {
    shimmer_per_gc = wiggleGC(dep = shimmerDep / 100,
                              len = shimmerLen,
                              nGC = nGC,
                              pitch_per_gc = pitch_per_gc,
                              rw = rw,
                              effect_on = shimmer_on)
    rolloff_source = t(t(rolloff_source) * shimmer_per_gc)
    # multiplies the first column of rolloff_source by shimmer_per_gc[1],
    # the second column by shimmer_per_gc[2], etc
  }

  # add subharmonics
  if (any(subDep > 0) && any(subh_on)) {
    subh = addSubh(
      rolloff = rolloff_source,
      pitch_per_gc = pitch_per_gc,
      subRatio = subRatio,
      subDep = subDep * rw ^ subDriftDep * subh_on,
      shortestEpoch = shortestEpoch,
      dynamicRange = dynamicRange
    )
    rolloff_source = subh$rolloff # list of matrices
    epochs = subh$epochs # dataframe
  } else {
    # if we don't need to add subharmonics
    rolloff_source = list(rolloff_source)
    epochs = data.frame ('start' = 1, 'end' = length(pitch_per_gc))
  }

  ## WAVEFORM GENERATION
  # synthesize continuously
  waveform = generateEpoch(
    pitch_per_gc = pitch_per_gc,
    epochs = epochs,
    rolloff_per_epoch = rolloff_source,
    samplingRate = samplingRate,
    glottisClosed_per_gc = glottisClosed_per_gc,
    targetLen = targetLen,
    closedFadeMs = 0.25,
    closedFadeShape = 'cos'
  )
  # sum(is.na(waveform))
  # plot(waveform[], type = 'l')
  # spectrogram(waveform, samplingRate = samplingRate)
  # playme(waveform, samplingRate = samplingRate)
  # meanSpectrum(waveform,samplingRate)

  ## POST-SYNTHESIS EFFECTS
  # add attack
  if (is.numeric(attackLen)) {
    if (any(attackLen > 0)) {
      l = floor(attackLen * samplingRate / 1000)
      if (length(l) == 1) l = c(l, l)
      waveform = .fade(list(sound = waveform, samplingRate = samplingRate),
                       fadeIn_points = l[1],
                       fadeOut_points = l[2])
      # plot(waveform, type = 'l')
    }
  }

  # pitch drift is accompanied by amplitude drift
  if (temperature > 0 && amplDriftDep > 0 && diff(range(drift)) != 0) {
    drift_ampl = zeroOne(drift) * temperature
    # otherwise pitchDriftDep scales amplDriftDep
    drift_ampl = drift_ampl - mean(drift_ampl) + 1  # hist(drift_ampl)
    # gc_upsampled = upsampleGC(pitch_per_gc, samplingRate = samplingRate)$gc
    # don't need very precise timing - just use an approximation to avoid
    # re-running upsampleGC or re-calculating midtimes
    gc_upsampled = c(1, cumsum(round(samplingRate / pitch_per_gc)))
    drift_upsampled = approx(c(drift_ampl, tail(drift_ampl, 1)),
                             n = length(waveform),
                             x = gc_upsampled)$y
    waveform = waveform * drift_upsampled ^ amplDriftDep
    # plot(drift_upsampled, type = 'l')
    # plot(waveform, type = 'l')
  }

  # normalize to be on the same scale as waveform (NB: after adding attack,
  # b/c fading the ends can change the overall range if eg peak ampl is at the beg.)
  if (normalize) {
    maw = max(abs(waveform))
    if (maw > 0) waveform = waveform / maw
  }

  # apply amplitude envelope
  if (is.numeric(ampl) || is.list(ampl)) {
    if (any(ampl$value != 0)) {
      amplEnvelope = do.call(getSmoothContour, c(smoothing, list(
        anchors = ampl,
        len = length(waveform),
        valueFloor = -dynamicRange,
        samplingRate = samplingRate
      )))
      # plot(amplEnvelope, type = 'l')
      # convert from dB to linear multiplier
      amplEnvelope = 10 ^ (amplEnvelope / 20)
      waveform = waveform * amplEnvelope
    }
  }

  # playme(waveform, samplingRate = samplingRate)
  # spectrogram(waveform, samplingRate = samplingRate)
  return(waveform)
}


#' Generate an epoch
#'
#' Takes descriptives of a number of glottal cycles (f0, closed phase, rolloff)
#' and creates a continuous waveform. The principle is to work with one epoch
#' with stable regime of subharmonics at a time and create a sine wave for each
#' harmonic, with amplitudes adjusted by rolloff.
#' @param pitch_per_gc pitch per glottal cycle, Hz
#' @param rolloff_per_epoch a list of matrices with one matrix for each epoch; each
#'   matrix should contain one column for each glottal cycle and one row for
#'   each harmonic (linear multiplier, ie NOT in dB). Rownames specify the ratio
#'   to F0 (eg 1.5 means it's a subharmonic added between f0 and its first
#'   harmonic)
#' @param epochs a dataframe specifying the beginning and end of each epoch
#' @param samplingRate the sampling rate of generated sound, Hz
#' @param glottisClosed_per_gc numeric vector: closed phase per glottal cycle,
#'   \%. If not NULL and any value is positive, closed-phase silences are
#'   inserted after synthesis, and the waveform is resampled back to the target
#'   duration implied by \code{upsampleGC()}
#' @param targetLen the expected length of output, in samples (needed for
#'   time-varying closed phase)
#' @param closedFadeMs approximate final fade length at closed-phase
#'   transitions, ms
#' @param closedFadeShape fade shape passed to \code{.fade()}
#' @return A waveform as a non-normalized numeric vector centered at zero.
#' @noRd
#' @examples
#' pitch_per_gc = seq(100, 150, length.out = 90)
#' epochs = data.frame (start = c(1, 51),
#'                      end = c(50, 90))
#' m1 = matrix(rep(10 ^ (-6 * log2(1:200) / 20), 50), ncol = 50, byrow = FALSE)
#' m2 = matrix(rep(10 ^ (-12 * log2(1:200) / 20), 40), ncol = 40, byrow = FALSE)
#' rownames(m1) = 1:nrow(m1)
#' rownames(m2) = 1:nrow(m2)
#' rolloff_source = list(m1, m2)
#' s = soundgen:::generateEpoch(pitch_per_gc, epochs,
#'                              rolloff_source, samplingRate = 16000)
#' # plot(s, type = 'l')
#' # playme(s)
generateEpoch = function(pitch_per_gc,
                         epochs,
                         rolloff_per_epoch,
                         samplingRate,
                         glottisClosed_per_gc = NULL,
                         targetLen = NULL,
                         closedFadeMs = 0.25,
                         closedFadeShape = 'cos') {
  up = upsampleGC(pitch_per_gc, samplingRate = samplingRate)
  pitch_upsampled = up$pitch
  if (is.null(targetLen)) targetLen = tail(up$end, 1)

  # cumulative f0 phase in cycles
  integr = cumsum(pitch_upsampled) / samplingRate
  waveform = NULL

  for (e in seq_len(nrow(epochs))) {
    st = as.integer(round(epochs$start[e]))
    en = as.integer(round(epochs$end[e]))

    if (is.na(st) || is.na(en) || st < 1 || en < st ||
        st > length(up$start) || en > length(up$end)) next

    # global sample indices for this epoch
    idx_up = up$start[st]:up$end[en]
    if (length(idx_up) == 0) next

    rol = rolloff_per_epoch[[e]]
    if (!is.matrix(rol)) rol = as.matrix(rol)

    nGC_epoch = en - st + 1
    x_epoch = up$midtimes[st:en]
    waveform_epoch = rep(0, length(idx_up))
    if (nrow(rol) > 0 && ncol(rol) > 0) {
      times_f0_vector = as.numeric(rownames(rol))
      if (length(times_f0_vector) != nrow(rol) || anyNA(times_f0_vector)) {
        times_f0_vector = seq_len(nrow(rol))
      }
      integr_epoch = integr[idx_up]

      for (h in seq_len(nrow(rol))) {
        times_f0 = times_f0_vector[h]
        if (nGC_epoch == 1 || ncol(rol) == 1) {
          am_upsampled = rep(rol[h, 1], length(idx_up))
        } else {
          am_upsampled = approx(
            x = x_epoch,
            y = rol[h, ],
            xout = idx_up,
            rule = 2
          )$y
        }
        waveform_epoch = waveform_epoch +
          sinpi(2 * integr_epoch * times_f0) * am_upsampled
      }
    }

    if (is.null(waveform)) {
      waveform = waveform_epoch
    } else {
      waveform = crossFade(
        waveform,
        waveform_epoch,
        samplingRate = samplingRate,
        crossLen = 15,
        shape = 'lin'
      )
    }
  }

  if (is.null(waveform)) waveform = numeric(0)

  # insert closed-phase silences and restore target duration
  if (length(waveform) > 0 &&
      length(up$end) > 0 &&
      !is.null(glottisClosed_per_gc) &&
      any(glottisClosed_per_gc > 0, na.rm = TRUE)) {

    waveform_long = addClosedPhase(
      audio = waveform,
      pitch_per_gc = pitch_per_gc,
      glottisClosed_per_gc = glottisClosed_per_gc,
      samplingRate = samplingRate,
      gc_start = up$start,
      gc_end = up$end,
      targetLen = targetLen,
      fade_ms = closedFadeMs,
      fadeShape = closedFadeShape
    )

    if (targetLen > 0 && length(waveform_long) != targetLen) {
      waveform_resampled = .resample(
        audio = list(
          sound = waveform_long,
          samplingRate = samplingRate
        ),
        len = targetLen,
        lowPass = length(waveform_long) > targetLen,
        interpol = 'linear'
      )
      waveform = waveform_resampled
    } else {
      waveform = waveform_long
    }
  }
  waveform
}


#' Add closed phases between glottal cycles
#'
#' Inserts silences after glottal cycles based on glottisClosed_per_gc.
#' Returns the lengthened waveform. Resampling back to target length is
#' intended to be done by the caller.
#'
#' @param audio numeric vector: continuous voiced waveform
#' @param pitch_per_gc numeric vector: f0 per glottal cycle, Hz
#' @param glottisClosed_per_gc numeric vector: closed phase per gc, %
#' @param samplingRate sampling rate, Hz
#' @param gc_start optional integer vector: start sample of each gc
#' @param gc_end optional integer vector: end sample of each gc
#' @param targetLen optional target length in samples, used to scale fades
#' @param fade_ms approximate final fade length, ms
#' @param fadeShape fade shape passed to .fade()
#' @return longer numeric vector
#' @noRd
addClosedPhase = function(audio,
                          pitch_per_gc,
                          glottisClosed_per_gc,
                          samplingRate,
                          gc_start,
                          gc_end,
                          targetLen,
                          fade_ms = 0.25,
                          fadeShape = 'cos') {
  n = length(audio)
  if (n == 0) return(audio)
  nGC = length(gc_end)
  if (nGC == 0) return(audio)
  segLen_expected = gc_end - gc_start + 1
  expectedLen = sum(segLen_expected)
  if (expectedLen <= 0) return(audio)

  # Rescale expected gc lengths to the actual audio length.
  # This matters because epoch cross-fading can slightly shorten the waveform.
  segLen_float = segLen_expected * n / expectedLen
  segLen_int = floor(segLen_float)
  extra = n - sum(segLen_int)
  if (extra > 0) {
    remainders = segLen_float - segLen_int
    ord = order(remainders, decreasing = TRUE)
    idx = ord[seq_len(min(extra, length(ord)))]
    segLen_int[idx] = segLen_int[idx] + 1
  }
  segLen_int = as.integer(segLen_int)
  ends = cumsum(segLen_int)
  starts = c(1, if (length(ends) > 1) ends[-length(ends)] + 1 else numeric(0))
  validIdx = which(segLen_int > 0)
  if (length(validIdx) == 0) return(audio)

  # first pass: calculate silence lengths
  closedLen_all = integer(nGC)
  for (i in validIdx) {
    closedLen_all[i] = max(0, round(segLen_int[i] * glottisClosed_per_gc[i] / 100))
  }
  lastValid = validIdx[length(validIdx)]
  totalSilence = sum(closedLen_all)
  if (totalSilence == 0) return(audio)

  # If the caller will resample to targetLen after this function,
  # make the fades longer now so that they remain short after resampling
  if (!is.null(targetLen) && targetLen > 0) {
    longLen = n + totalSilence
    fadeScale = max(1, longLen / targetLen)
  } else {
    fadeScale = 1
  }
  fade_points_target = round(fade_ms * samplingRate / 1000)

  # second pass: build the lengthened waveform
  out = vector('list', length(validIdx) * 2)
  k = 1
  prevSilence = 0L

  for (i in validIdx) {
    st = starts[i]
    en = ends[i]
    if (st > en || st > n || en < 1) next

    seg = audio[st:en]
    segLen = length(seg)
    closedLen = closedLen_all[i]

    fadeIn = prevSilence > 0
    fadeOut = closedLen > 0
    if ((fadeIn || fadeOut) && fade_points_target > 0 && segLen >= 4) {
      fadeLen = ceiling(fade_points_target * fadeScale)
      fadeLen = max(0, min(fadeLen, floor((segLen - 1) / 2)))

      # .fade() needs at least 2 points for an audible fade-in
      if (fadeLen > 1) {
        seg = .fade(
          list(sound = seg, samplingRate = samplingRate),
          fadeIn_points = if (fadeIn) fadeLen else 0,
          fadeOut_points = if (fadeOut) fadeLen else 0,
          shape = fadeShape
        )
      }
    }

    # save the faded-in/out segment
    if (k > length(out)) out = c(out, vector('list', length(out)))
    out[[k]] = seg
    k = k + 1

    # save the silence (closed phase)
    if (closedLen > 0) {
      if (k > length(out)) out = c(out, vector('list', length(out)))
      out[[k]] = numeric(closedLen)
      k = k + 1
    }

    prevSilence = closedLen
  }

  # concatenate all segments and silences
  if (k > 1) {
    out = out[1:(k - 1)]
    audio = do.call(`c`, out)
  }
  audio
}



#' Generate beat
#'
#' Generates percussive sounds from clicks through drum-like beats to sliding
#' tones. The principle is to create a sine wave with rapid frequency modulation
#' and to add a fade-out. No extra harmonics or formants are added. For this
#' specific purpose, this is vastly faster and easier than to tinker with
#' \code{\link{soundgen}} settings, especially since percussive syllables tend
#' to be very short.
#'
#' @seealso \code{\link{soundgen}}
#'
#' @inheritParams soundgen
#' @param nSyl the number of syllables to generate
#' @param sylLen average duration of each syllable, ms
#' @param pauseLen average duration of pauses between syllables, ms
#' @param pitch fundamental frequency, Hz (numeric vector or anchor format, see
#'   \code{\link{soundgen}})
#' @param fadeOut if TRUE, a linear fade-out is applied to the entire syllable
#' @return The synthesized waveform as a numeric vector.
#' @export
#' @examples
#' playback = c(TRUE, FALSE)[2]
#' # a drum-like sound
#' s = beat(nSyl = 1, sylLen = 200,
#'          pitch = c(200, 100), play = playback)
#' # plot(s, type = 'l')
#'
#' # a dry, muted drum
#' s = beat(nSyl = 1, sylLen = 200,
#'          pitch = c(200, 10), play = playback)
#'
#' # sci-fi laser guns
#' s = beat(nSyl = 3, sylLen = 300,
#'          pitch = c(1000, 50), play = playback)
#'
#' # machine guns
#' s = beat(nSyl = 10, sylLen = 10, pauseLen = 50,
#'          pitch = c(2300, 300), play = playback)
beat = function(nSyl = 10,
                sylLen = 200,
                pauseLen = 50,
                pitch = c(200, 10),
                samplingRate = 16000,
                fadeOut = TRUE,
                play = FALSE) {
  len = sylLen * samplingRate / 1000
  pitchContour = getSmoothContour(anchors = pitch,
                                  len = len,
                                  valueFloor = 0,
                                  thisIsPitch = TRUE)
  int = cumsum(pitchContour)
  beat = sin(2 * pi * int / samplingRate)
  if (fadeOut) {
    beat = .fade(list(sound = beat, samplingRate = samplingRate),
                 fadeIn_points = 0,
                 fadeOut_points = length(beat))
  }
  # plot(beat, type = 'l')
  # spectrogram(beat, samplingRate, ylim = c(0, 1))
  if (nSyl > 1) {
    pause = rep(0, round(pauseLen / 1000 * samplingRate))
    beat = rep(c(beat, pause), nSyl)
  }
  beat = beat / max(abs(beat))  # normalize

  if (isTRUE(play)) {
    playme(beat, samplingRate)
  } else if (is.character(play)) {
    playme(beat, samplingRate, player = play)
  }
  beat
}
