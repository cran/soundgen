### UTILITIES FOR SOUND GENERATION ###

#' Add pitch jumps
#'
#' Adds random discontinuities (jumps) to a pitch contour in a manner that
#' shifts a segment of pitch contour up or down. Careful when adding several
#' jumps: one can land on top of another, and it gets rather weird rather
#' quickly.
#'
#' @param pitch numeric vector of f0 values over time (any step is OK)
#' @param magn magnitude of jump(s) in semitones, a numeric vector of length 1
#'   or nj
#' @param nj number of jump pairs = affected segments (e.g., a single fragment
#'   is transposed if nj = 1)
#' @param prop duration of transposed episode(s) as a proportion of input vector
#'   \code{pitch} (capped to 1 / nj / 2 to avoid overlapping jumps)
#' @param plot if TRUE, plots the original and modified contours
#' @return The input vector of frequencies with added jumps.
#' @noRd
#' @examples
#' pitch = interpolate(x = (0:3)/3, y = c(100, 350, 320, 110), xout = (0:100)/100)
#' addPitchJumps(pitch, magn = runif(1, 3, 12), plot = TRUE)
#' addPitchJumps(pitch, magn = c(6, 1), nj = 2, plot = TRUE)
#' addPitchJumps(pitch, magn = 3, nj = 5, plot = TRUE)
#'
#' pitch2 = c(rep(NA, 10), pitch[1:50], rep(NA, 25), pitch[51:100], rep(NA, 17))
#' addPitchJumps(pitch2, magn = c(6, 1), nj = 2, plot = TRUE)
addPitchJumps = function(pitch, magn, nj = 1, prop = .1, plot = FALSE) {
  if (nj < 1 || length(pitch) < 2) return(pitch)
  if (length(magn) == 1) {
    magn = rep(magn[1], nj)
  } else if (length(magn) != nj) {
    stop('magn must be of length 1 or nj')
  }
  prop = min(prop, 1 / nj / 2)  # avoid overlap
  if (sum(!is.na(pitch)) < 2) return(pitch)
  len = length(pitch)
  out = pitch
  magn_lin = 2^(magn/12)
  for (j in 1:nj) {
    start_min = (j - 1) / nj
    start_max = (j - prop) / nj
    # try max 10 times to land on a non-NA pitch value
    for (i in 1:10) {
      start = max(1, round(runif(1, start_min, start_max) * len))
      if (!is.na(pitch[start])) break
    }
    # if that fails, default to the first non-NA
    if (is.na(pitch[start])) start = which(!is.na(pitch))[1]
    idx = start : min(round(start + len * prop), len)
    if (start == 1) {
      dir = sample(c('up', 'down'), size = 1)
    } else {
      dj = pitch[start] - pitch[start - 1]
      if (is.na(dj) || dj > 0) prob = c(.8, .2) else prob = c(.2, .8)
      dir = sample(c('up', 'down'), size = 1, prob = prob)
    }
    if (dir == 'up') {
      out[idx] = out[idx] * magn_lin[j]
    } else {
      out[idx] = out[idx] / magn_lin[j]
    }
  }
  if (plot) {
    plot(pitch, type = 'l', ylim = range(c(pitch, out), na.rm = TRUE))
    points(out, type = 'l', col = 'blue')
  }
  out
}


#' Upsample glottal cycles
#' Upsamples a pitch contour to samplingRate through linear interpolation
#' between successive glottal cycles.
#' @param pitch_per_gc a vector of fundamental frequencies per glottal cycle
#' @param samplingRate target sampling rate after upsampling, in Hz
#' @noRd
#' @examples
#' soundgen:::upsampleGC(pitch_per_gc = 150, samplingRate = 16000)
#' soundgen:::upsampleGC(pitch_per_gc = c(100, 150), samplingRate = 16000)
#' soundgen:::upsampleGC(pitch_per_gc = c(100, 150, 140, 130), samplingRate = 16000)
upsampleGC = function(pitch_per_gc, samplingRate = 16000) {
  pitch_per_gc = as.numeric(pitch_per_gc)
  l = length(pitch_per_gc)

  if (l == 0) {
    return(list(
      pitch = numeric(0),
      gc = 1,
      start = numeric(0),
      end = numeric(0),
      midtimes = numeric(0)
    ))
  }

  # number of audio samples in each glottal cycle
  gccrossLenPoints = as.integer(pmax(1, round(samplingRate / pitch_per_gc)))
  cc = cumsum(gccrossLenPoints)
  totalN = cc[l]

  # gc boundaries, as in old versions of soundgen <3.0
  gc_upsampled = c(1, cc)

  # unambiguous sample intervals for each glottal cycle:
  # GC i spans start[i]:end[i], inclusive
  gc_start = c(1, if (l > 1) cc[-l] + 1 else numeric(0))
  gc_end = cc

  # Time points representing each GC for interpolation
  # - first GC --> first sample
  # - last GC --> last sample
  # - internal GCs --> near their centers
  if (l == 1) {
    pitch_upsampled = rep(pitch_per_gc, totalN)
    midtimes = 1
  } else if (l == 2) {
    pitch_upsampled = seq(pitch_per_gc[1], pitch_per_gc[2],
                          length.out = totalN)
    midtimes = c(1, totalN)
  } else {
    midtimes = c(
      1,
      cc[1:(l - 2)] + ceiling(gccrossLenPoints[2:(l - 1)] / 2),
      totalN
    )

    pitch_upsampled = splinefun(
      x = midtimes,
      y = pitch_per_gc,
      method = "monoH.FC"
    )(seq_len(totalN))
  }

  list(
    pitch = pitch_upsampled,
    gc = gc_upsampled,
    start = gc_start,
    end = gc_end,
    midtimes = midtimes
  )
}


#' Divide f0 contour into glottal cycles
#' Returns a vector of indices giving the borders between "glottal cycles",
#' assuming that we know the true f0 at each time point (as we do in synthesized
#' sounds). The first index is always 1.
#' @param pitch a vector of fundamental frequency values
#' @param samplingRate sampling rate at which f0 values are provided
#' @noRd
#' @examples
#' # 100 ms of audio with f0 steadily increasing from 150 to 200 Hz
#' soundgen:::getGlottalCycles(seq(150, 200, length.out = 350),
#'   samplingRate = 3500)
getGlottalCycles = function (pitch, samplingRate) {
  len = length(pitch)
  if (len < 2) return(1)
  glottalCycles = vector('numeric', len)  # max possible length
  # (pre-allocating the glottalCycles vector is faster than growing it iteratively)
  i = 1 # the first border is the first time point
  j = 1
  while (i < len) {
    glottalCycles[j] = i
    # take steps proportionate to the current F0
    i = i + max(2, floor(samplingRate / pitch[i]))
    j = j + 1
  }
  glottalCycles[1:(j - 1)]
}


#' Syllable structure of a bout
#' Stochastic generation of syllable structure of a bout. Calls
#' \code{\link{rnorm_truncated2}} to vary the duration of each new syllable and of
#' pauses between syllables. Total bout duration will also vary, unless
#' temperature is zero. However, the output will always contain exactly
#' \code{nSyl} syllables.
#' @param nSyl the desired number of syllables
#' @param sylLen the desired mean syllable duration, in ms (vectorized)
#' @param pauseLen the desired mean pause between syllables, in ms (vectorized)
#' @param sylDur_min,sylDur_max the lower and upper bounds on possible syllable
#'   duration, in ms
#' @param pauseDur_min,pauseDur_max the lower and upper bounds on possible pause
#'   duration, in ms
#' @param temperature a non-negative float regulating the stochasticity of
#'   syllable segmentation; 0 = no stochasticity; 1 = sd of proposals is equal
#'   to sylLen (very strong stochasticity)
#' @return A data.frame with a list of start-end points for syllables.
#' @noRd
#' @examples
#' soundgen:::divideIntoSyllables(nSyl = 1, sylLen = 180)
#' soundgen:::divideIntoSyllables(nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0.2, plot = TRUE)
#' soundgen:::divideIntoSyllables(nSyl = 5, sylLen = 180,
#'   pauseLen = 55, temperature = 0)
#' soundgen:::divideIntoSyllables(nSyl = 3, sylLen = 100,
#'   pauseLen = 25, temperature = 0.5)
#'
#' # sylLen and pauseLen are vectorized:
#' soundgen:::divideIntoSyllables(nSyl = 15, sylLen = 100:200,
#'   pauseLen = c(80, 25, 80), temperature = 0.05, plot = TRUE)
divideIntoSyllables = function(
    nSyl,
    sylLen,
    pauseLen,
    sylDur_min = 20,
    sylDur_max = 10000,
    pauseDur_min = 20,
    pauseDur_max = 1000,
    temperature = 0.025,
    invalidArgAction = 'adjust',
    plot = FALSE) {
  # up- or downsample durations to nSyl
  if (length(sylLen) > 1 && length(sylLen) != nSyl) {
    sylLen = approx(sylLen, n = nSyl)$y
  }
  if (length(pauseLen) > 1 && length(pauseLen) != (nSyl - 1)) {
    pauseLen = approx(pauseLen, n = nSyl - 1)$y
  }

  # generate random lengths of syllables and pauses under constraints
  syls = rnorm_truncated2(
    n = nSyl,
    mean = sylLen,
    low = sylDur_min,
    high = sylDur_max,
    sd = sylLen * temperature,
    invalidArgAction = invalidArgAction
  )
  pauses = rnorm_truncated2(
    n = nSyl - 1,
    mean = pauseLen,
    low = pauseDur_min,
    high = pauseDur_max,
    sd = pauseLen * temperature,
    invalidArgAction = invalidArgAction
  )

  out = data.frame(start = rep(0, nSyl), end = rep(0, nSyl))
  for (i in 1:nSyl) {
    if (i == 1) {
      out$start[i] = 0
    } else {
      out$start[i] = out$end[i - 1] + pauses[i - 1]  # start time of syllable, in ms
    }
    out$end[i] = out$start[i] + syls[i] # end time of syllable, in ms
  }
  out$dur = out$end - out$start

  if (plot) {
    # for the UI
    t = seq_len(max(out$end))
    plot(t, rep(1, length(t)), type = 'n', xlab = 'Time, ms', ylab = '',
         bty = 'n', yaxt = 'n', ylim = c(0.8, 1.2))
    for (i in seq_len(nrow(out))) {
      rect(xleft = out[i, 1], xright = out[i, 2], ybottom = .9, ytop = 1.1,
           col = 'blue')
      text(x = mean(c(out[i, 2], out[i, 1])), y = 1,
           col = 'yellow', cex = 5, labels = i)
    }
  }
  out
}


#' Randomly modify anchors
#' A helper function for introducing random variation into any anchors (for
#' pitch / breathing / amplitude / ...). At higher temperatures can also add or
#' delete an anchor. NB: make sure the lower and upper bounds are reasonable
#' given the scale of df$value!
#' @param df dataframe of anchors, for ex. \code{data.frame(time = c(0, .1, .8,
#'   1), value = c(100, 230, 180, 90))}
#' @param temperature,temp_coef regulate the amount of stochasticity
#'   ("wiggling"). Since \code{temperature} is used in several functions,
#'   \code{temp_coef} gives more flexibility by controlling how much temperature
#'   affects this particular aspect, namely random variation in anchors. These
#'   two are multiplied, so \code{temp_coef} of 0.5 halves the effect of
#'   temperature.
#' @param low,high bounds on possible variation. Both \code{low} and \code{high}
#'   should be vectors of length 2: the first element specifies the boundary for
#'   \code{df$time} and the second for \code{df$value}. Ex.: low = c(0,1) - low
#'   bound on "time"=0, low bound on "value"=1
#' @param wiggleAllRows should the first and last time anchors be wiggled? (TRUE
#'   for breathing, FALSE for other anchors)
#' @param sd_values (optional) the exact value of sd used by rnorm_truncated2 in
#'   columns 2 and beyond
#' @param roundToInteger if TRUE, rounds the values (not time points)
#' @inheritParams soundgen
#' @return Modified original dataframe.
#' @noRd
#' @examples
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(0, .1, .8, 1), value = c(100, 230, 180, 90)),
#'   temperature = .2, temp_coef = .1, low = c(0, 50), high = c(1, 1000),
#'   wiggleAllRows = FALSE) # pitch
#' soundgen:::wiggleAnchors(df = data.frame(time = 0, value = 240),
#'   temperature = .2, temp_coef = .1, low = c(0, 50), high = c(1, 1000),
#'   wiggleAllRows = FALSE) # pitch, single anchor
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(-100, 100, 600, 900), value = c(-120, -80, 0, -120)),
#'   temperature = .4, temp_coef = .5, low = c(-Inf, -120), high = c(+Inf, 30),
#'   wiggleAllRows = TRUE) # noise
#'  soundgen:::wiggleAnchors(df = data.frame(
#'    time = c(0, 1/3, 2/3, 1), value = c(2, 4, 3, 2)),
#'    low = c(0, 1), high = c(1, 20), roundToInteger = TRUE)
#'
#' # formants
#' formants = list(f1 = list(time = 0, freq = 860, amp = 30, width = 120),
#'                 f2 = list(time = c(0,1), freq = 1280,
#'                 amp = c(10,40), width = 120))
#' for (f in seq_along(formants)) {
#'   formants[[f]] = soundgen:::wiggleAnchors(
#'     df = formants[[f]],
#'     temperature = .4, temp_coef = .5,
#'     low = c(0, 50, 0, 1),
#'     high = c(1, 8000, 120, 2000),
#'     wiggleAllRows = FALSE
#'   )
#' }
#' print(formants)
#'
#' # manually provided sd (temp only affects prob of adding/dropping anchors)
#' soundgen:::wiggleAnchors(df = data.frame(
#'   time = c(0, .1, .8, 1), value = c(100, 230, 180, 90)),
#'   wiggleAllRows = FALSE, sd_values = 5)
wiggleAnchors = function(df,
                         temperature = .05,
                         temp_coef = 1,
                         low = c(0, -Inf),
                         high = c(1, Inf),
                         wiggleAllRows = FALSE,
                         sd_values = NULL,
                         roundToInteger = FALSE,
                         invalidArgAction = 'adjust') {
  if (temperature == 0 || temp_coef == 0) return(df)
  if (!is.data.frame(df)) df = as.data.frame(df)  # not reformatAnchors() b/c it normalizes time by default
  nc = ncol(df)
  nr = nrow(df)
  if (any(is.na(df)) || nr == 0) return(df)
  if (nc != length(low) ||
      nc != length(high) ||
      length(low) != length(high)) {
    warning('Vectors "low" and "high" should be the same length as ncol(df)')
  }

  # should we add a new anchor or remove one?
  action = sample(c('nothing', 'remove', 'add'),
                  size = 1,
                  prob = c(1 - temperature, rep(temperature / 2, 2)))
  if (action == 'add') {  # add an anchor
    # print('adding an anchor')
    if (nr == 1) {
      # the first anchor is the original, the second random
      idx = 2:nc
      newAnchor = try(rnorm_truncated(
        n = nc - 1,
        mean = as.numeric(df[1, idx]),
        sd = ifelse(is.numeric(sd_values),
                    sd_values,
                    abs(as.numeric(df[1, idx]) * temperature * temp_coef)),
        low = low[idx],
        high = high[idx],
        roundToInteger = ifelse(colnames(df)[idx] == 'time', FALSE, roundToInteger),
        invalidArgAction = invalidArgAction))
      if (inherits(newAnchor, 'try-error')) {
        stop(paste('Failed to add an anchor to df:', paste(df, collapse = ', ')))
      } else {
        new_row = df[1, ]
        new_row[, idx] = newAnchor
        df = rbind(df[1, ], new_row)
        df[, 1] = c(0, 1)  # make time c(0, 1)
      }
    } else {
      # insert between any two existing anchors
      a1 = sample(1:nrow(df), size = 1)
      direction = sample(c(-1, 1), size = 1)
      a2 = ifelse(a1 + direction < 1 | a1 + direction > nrow(df),
                  a1 - direction,
                  a1 + direction)
      i1 = min(a1, a2)
      i2 = max(a1, a2)  # insert between rows i1 and i2
      newAnchor = colMeans(df[i1:i2, , drop = FALSE])
      df = rbind(df[1:i1, ],
                 newAnchor,
                 df[i2:nrow(df), ])
    }
  } else if (action == 'remove') {
    # print('removing an anchor')
    if (wiggleAllRows) {
      # we can remove any anchor
      idx = sample(1:nrow(df), 1)
      df = df[-idx, ]
    } else {
      # we don't touch the first and last anchors
      if (nrow(df) > 2) {
        # NB: sample() may return 1 if nrow(df) = 2, hence sampleModif()
        idx = sampleModif(x = (2:(nrow(df) - 1)), size = 1)
        df = df[-idx, ]
      }
    }
  }
  rownames(df) = 1:nrow(df)  # in case we added / removed an anchor

  # wiggle anchors
  if (wiggleAllRows) {
    orig = NULL
  } else {
    # save the original time values and put them back in later (usually 0 and 1)
    orig = c(df[1, 1], df[nrow(df), 1])
  }
  if (nrow(df) == 1) {
    ranges = as.numeric(df)
  } else {
    ranges = as.numeric(apply(df, 2, function(x) diff(range(x))))
    # if no variation in values, defaults to value
    z = which(ranges == 0)
    ranges[z] = abs(as.numeric(df[1, z]))
  }
  for (i in 1:ncol(df)) {
    w = try(rnorm_truncated2(
      n = nrow(df),
      mean = as.numeric(df[, i]),
      sd = if (i > 1 & !is.null(sd_values)) sd_values else
        as.numeric(ranges[i] * temperature * temp_coef),
      low = low[i],
      high = high[i],
      roundToInteger = if (colnames(df)[i] == 'time') FALSE else roundToInteger,
      invalidArgAction = invalidArgAction
    ))
    if (inherits(w, 'try-error')) {
      warning(paste('Failed to wiggle column', i, 'of df:',
                    paste(df, collapse = ', ')))
    } else {
      df[, i] = w
    }
  }
  if (is.numeric(orig)) {
    df[c(1, nrow(df)), 1] = orig
  }

  # make sure the anchors are still in the right time order
  df[order(df$time), , drop = FALSE]
}


#' Scale noise anchors
#' Scales a dataframe containing noise anchors so as to preserve the timing of
#' positive anchors relative to the new syllable duration. Negative time anchors
#' are not changed: the pre-aspiration length is constant, regardless of the
#' actual syllable duration. Time anchors from 0 to sylLen are proportional to
#' the actual syllable duration re the average expected duration (which the user
#' sees in the UI when choosing time anchors). Time anchors beyond sylLen are
#' scaled to preserve post-aspiration duration.
#' @param noiseTime vector of time points at which noise anchors are defined
#' @param sylLen_old syllable length relative to which the timing of noise anchors is
#' specified
#' @param sylLen_new the new syllable length
#' @noRd
#' @examples
#' noiseTime = c(-20, 50, 120)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 100, sylLen_new = 200)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 100, sylLen_new = 50)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 200, sylLen_new = 300)
scaleNoiseAnchors = function(noiseTime, sylLen_old, sylLen_new) {
  if (sylLen_old <= 0) return(noiseTime)
  idx_mid = which(noiseTime > 0 &             # not before syl
                    noiseTime < sylLen_old)   # not after syl
  idx_after = which(noiseTime >= sylLen_old)  # after syl
  noiseTime[idx_mid] = noiseTime[idx_mid] * sylLen_new / sylLen_old
  noiseTime[idx_after] = noiseTime[idx_after] - sylLen_old + sylLen_new
  noiseTime
}


#' Wiggle glottal cycles
#' Helper function for preparing a vector of multiplication factors for adding
#' jitter and shimmer per glottal cycle. Generates random anchors for each
#' jitter/shimmer period and draws a smooth contour between them by spline
#' interpolation.
#' @param dep a vector of any length specifying the strength of applied effect as
#'   2 ^ rnorm(..., 0, dep))
#' @param len a vector of any length specifying the period of applied effect in
#'   ms
#' @param nGC number of glottal cycles
#' @param pitch_per_gc vector of length nGC specifying pitch per glottal cycle,
#'   Hz
#' @param rw vector of length nGC specifying a random walk around 1 to multiply
#'   the effect with
#' @param effect_on vector of length nGC specifying glottal cycles to which the
#'   effect should be applied (0 = off, 1 = on)
#' @noRd
#' @examples
#' plot(soundgen:::wiggleGC(dep = 5 / 12, len = c(3, 50), nGC = 100,
#'               pitch_per_gc = rnorm(100, 150, 10),
#'               rw = rep(1, 100), effect_on = rep(1, 100)),
#'      type = 'b')
#' plot(soundgen:::wiggleGC(dep = 5 / 12, len = c(3, 50), nGC = 100,
#'               pitch_per_gc = rnorm(100, 150, 10),
#'               rw = rep(1, 100),
#'               effect_on = c(rep(1, 30), rep(0, 20), rep(1, 50))),
#'      type = 'b')
#' plot(soundgen:::wiggleGC(dep = c(1/12, 10/12), len = c(3, 50), nGC = 100,
#'               pitch_per_gc = rnorm(100, 150, 10),
#'               rw = rep(1, 100), effect_on = rep(1, 100)),
#'      type = 'b')
wiggleGC = function(dep, len, nGC, pitch_per_gc, rw, effect_on) {
  # get the number of gc that make up one period of effect (vector of length nGC)
  ratio = pmax(0.01, pitch_per_gc * len / 1000)

  # pre-allocate maximum possible size
  idx = numeric(nGC)
  idx[1] = 1
  pos = 1
  count = 1
  while (pos < nGC) {
    pos = pos + ratio[pos]
    count = count + 1
    idx[count] = pos
  }
  idx = unique(round(idx[1:(count - 1)]))  # trim to actual size
  # idx gives the gc's whose pitch will be wiggled
  len_idx = length(idx)

  len_dep = length(dep)
  if (len_dep == 1) {
    dep_idx = rep(dep, len_idx)
  } else {
    dep_idx = approx(dep, n = len_idx)$y
  }
  effect = 2 ^ (rnorm(
    n = len_idx,
    mean = 0,
    sd = dep_idx
  ) * rw[idx] * effect_on[idx])
  # plot(effect, type = 'b')

  # upsample to length nGC
  effect_per_gc = approx(effect, n = nGC, x = idx)$y
  effect_per_gc = pitchSmoothPraat(effect_per_gc, samplingRate = 1,
                                   bandwidth = len_idx / nGC / 2)
  # plot(effect_per_gc, type = 'l', ylim = range(c(effect, effect_per_gc)))
  # points(idx, effect, col = 'blue')
  effect_per_gc
}


#' Validate parameters
#' Checks whether the value of a numeric parameter falls within the allowed
#' range. Options: abort, reset to default, throw a warning and continue.
#' @param p parameter name
#' @param gp parameter value
#' @param def matrix or dataframe containing reference values (low, high,
#'   default)
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   range: 'adjust' = reset to default value, 'abort' = stop execution,
#'   'ignore' = throw a warning and continue (may crash)
#' @noRd
#' @examples
#' validatePars('sylLen', c(100, 200, 100000), permittedValues, 'adjust')
validatePars = function(p, gp, def,
                        invalidArgAction = c('adjust', 'abort', 'ignore')) {
  invalidArgAction = match.arg(invalidArgAction)
  if (any(gp < def[p, 'low']) ||
      any(gp > def[p, 'high'])) {
    if (invalidArgAction == 'abort') {
      # exit with a warning
      stop(paste0(
        "\n", p, " should be between ", def[p, 'low'],  " and ",
        def[p, 'high'], "; exiting",
        ". Use invalidArgAction = 'ignore' to override"))
    } else if (invalidArgAction == 'ignore') {
      # throw a warning and continue
      warning(paste0(
        "\n", p, " should be between ", def[p, 'low'],  " and ",
        def[p, 'high'], "; override with caution"))
    } else {
      # reset only the out-of-bounds elements to default, with a warning
      gp[gp < def[p, 'low']] = def[p, 'default']
      gp[gp > def[p, 'high']] = def[p, 'default']
      warning(paste0(
        "\n", p, " should be between ", def[p, 'low'],  " and ",
        def[p, 'high'], "; resetting out-of-bounds elements to ", def[p, 'default'],
        ". Use invalidArgAction = 'ignore' to override"))
    }
  }
  gp
}


#' Object to string
#' Converts any object to a string that preserves all internal structure and names.
#' @param x any R object (unquoted)
#' @noRd
#' @examples
#' soundgen:::objectToString('adja')
#' soundgen:::objectToString(500)
#' soundgen:::objectToString(c(870, 1250, 1900))
#' soundgen:::objectToString(list(f1 = c(870, 1250), f2 = list(freq = 500, amp = 30)))
#' soundgen:::objectToString(list(
#'   pitch = list(time = c(0, 1), value = c(160, 150)),
#'   noise = list(time = c(-1, 170, 362), value = c(-14, 0, -26)),
#'   mouth = list(time = c(0, 0.07, 1), value = c(0, 0.48, 0.32))))
#' # NB: no matter how long, the object is still returned as an unbroken string
objectToString = function(x) {
  if (is.character(x) && length(x) == 1) {
    cp = x
  } else {
    # tried and failed: toString, capture.output(call('print', x)), etc.
    cp = deparse(x, width.cutoff = 500, control = c('keepNA', 'niceNames'))
    if (length(cp) > 1) cp = paste(cp, collapse = '')
    # deparse1 comes close, but it require R 4.0 and mishandles strings
  }
  cp
}


#' Silence sound segments
#' Fills specified segments with silence (0) and fades in-out the ends of the
#' silenced segment.
#' @param x sound as a numeric vector
#' @param samplingRate sampling rate, Hz
#' @param na_seg dataframe containing columns "prop_start" and "prop_end"
#' @param attackLen attack length, ms
#' @noRd
#' @examples
#' s = runif(4000) * 2 - 1
#' s1 = soundgen:::silenceSegments(s, 16000,
#'        na_seg = data.frame(prop_start = c(.1, .5), prop_end = c(.2, .85)),
#'        attackLen = c(5, 15))
#' osc(s1)
silenceSegments = function(
    x,
    samplingRate,
    na_seg,
    attackLen = 50
) {
  ls = length(x)
  l = floor(attackLen * samplingRate / 1000)
  if (length(l) == 1) l = c(l, l)
  for (r in seq_len(nrow(na_seg))) {
    idx_start = max(1, round(na_seg$prop_start[r] * ls))
    idx_end = min(ls, round(na_seg$prop_end[r] * ls))
    if (idx_start > idx_end) next
    idx_zero = idx_start:idx_end
    x[idx_zero] = 0
    if (any(attackLen > 0)) {
      if (na_seg$prop_start[r] > 0) {
        # fade out at idx_start
        fade_from = max(1, idx_start - l[2])
        fade_idx = fade_from:idx_start
        x[fade_idx] = .fade(
          list(sound = x[fade_idx], samplingRate = samplingRate),
          fadeIn_points = 0,
          fadeOut_points = length(fade_idx))
      }
      if (na_seg$prop_end[r] < 1) {
        # fade out the start of the next syl
        fade_to = min(ls, idx_end + l[1])
        fade_idx = idx_end:fade_to
        x[fade_idx] = .fade(
          list(sound = x[fade_idx], samplingRate = samplingRate),
          fadeIn_points = length(fade_idx),
          fadeOut_points = 0)
      }
    }
    # spectrogram(x, samplingRate)
  }
  x
}


#' Wiggle parameters
#' Helper function for \code{\link{matchPars}}. Takes a list of control
#' parameters for \code{\link{soundgen}} and introduces some random variation in
#' their values.
#' @param parList full list of considered parameters
#' @param parsToWiggle a list of the names of pars that might be mutated
#' @inheritParams matchPars
#' @noRd
#' @examples
#' soundgen:::wigglePars(
#'   parList = list(
#'     sylLen = 250,
#'     pitch = data.frame(time = c(0, 1), value = c(200, 300))
#'   ),
#'   parsToWiggle = c('sylLen', 'pitch'),
#'   probMutation = .75,
#'   stepVariance = .5
#' )
wigglePars = function(parList,
                      parsToWiggle,
                      probMutation,
                      stepVariance) {
  parsToRound = c('repeatBout', 'nSyl')

  # choose pars to mutate
  if (length(parsToWiggle) > 1) {
    idx_mut = rbinom(n = length(parsToWiggle),
                     size = 1,
                     prob = probMutation)
    idx_mut_bin = which(idx_mut == 1)
    parsToMutate = parsToWiggle[idx_mut_bin]
    if (length(parsToMutate) == 0) {
      # need to mutate at least one par
      parsToMutate = sample(parsToWiggle, size = 1)
    }
  } else {
    parsToMutate = parsToWiggle
  }

  # prepare a list of mutated par values to feed to the sound generator
  for (p in parsToMutate) {
    if (is.numeric(parList[[p]])) {  # continuous pars
      l = permittedValues[p, 'low']
      h = permittedValues[p, 'high']
      r = ifelse(parList[[p]] != 0,  # if par = 0, we wiggle based on its range
                 abs(parList[[p]]),  # otherwise based on its current value
                 h - l)
      parList[[p]] = rnorm_truncated2(
        n = 1,
        mean = parList[[p]],
        low = l,
        high = h,
        sd = r * stepVariance,
        roundToInteger = (p %in% parsToRound)
      )
    } else if (is.list(parList[[p]])) {  # anchors
      if (p == 'formants' || p == 'formantsNoise') {  # formants
        for (f in seq_along(parList[[p]])) {
          parList[[p]][[f]] = wiggleAnchors(
            df = parList[[p]][[f]],
            temperature = stepVariance,
            temp_coef = 1,
            low = c(0, 50, -120, 1),  # time freq amp width
            high = c(1, 8000, 120, 2000),
            wiggleAllRows = FALSE
          )
        }
      } else {  # pitch / ampl / etc
        wiggleAllRows = FALSE
        if (p == 'pitch') {
          low = c(0, permittedValues['pitchFloor', 'default'])
          high = c(1, permittedValues['pitchCeiling', 'default'])
        } else if (p == 'pitchGlobal') {
          low = c(0, permittedValues['pitchDeltas', 'low'])
          high = c(1, permittedValues['pitchDeltas', 'high'])
        } else if (p == 'ampl' ||
                   p == 'amplGlobal') {
          low = c(0, 0)
          high = c(1, permittedValues['dynamicRange', 'default'])
        } else if (p == 'noise') {
          low = c(-Inf, -permittedValues['dynamicRange', 'default'])
          high = c(+Inf, 40)
          wiggleAllRows = TRUE
        }
        parList[[p]] = wiggleAnchors(
          df = parList[[p]],
          temperature = stepVariance,
          temp_coef = 1,
          low = low,  # time, value
          high = high,
          wiggleAllRows = wiggleAllRows
        )
      }
    }
  }
  parList
}


#' Match soundgen pars (experimental)
#'
#' Attempts to find settings for \code{\link{soundgen}} that will reproduce an
#' existing sound. The principle is to mutate control parameters, trying to
#' improve fit to target. The currently implemented optimization algorithm is
#' simple hill climbing. Disclaimer: this function is experimental and may or
#' may not work for particular tasks. It is intended as a supplement to - not
#' replacement of - manual optimization. See
#' https://cogsci.se/soundgen/sound_generation.html and
#' https://cogsci.se/soundgen/matching/matching.html for more information.
#'
#' @inheritParams getSpec
#' @param target the sound we want to reproduce using soundgen: path to an audio
#'   file or numeric vector
#' @param samplingRate sampling rate of \code{target} (only needed if target is
#'   a numeric vector, rather than a .wav file)
#' @param pars arguments to \code{\link{soundgen}} that we are attempting to
#'   optimize
#' @param init a list of initial values for the optimized parameters \code{pars}
#'   and the values of other arguments to soundgen that are fixed at non-default
#'   values (if any)
#' @param probMutation the probability of a parameter mutating per iteration
#' @param stepVariance scale factor for calculating the size of mutations
#' @param maxIter maximum number of mutated sounds produced without improving
#'   the fit to target; \code{iter = 0} means acoustic analysis only, no
#'   optimization
#' @param minExpectedDelta minimum improvement in fit to target required to
#'   accept the new sound candidate
#' @param compareSounds_pars a list of control parameters passed to
#'   \code{\link{compareSounds}}
#' @param verbose if TRUE, reports the outcome at each iteration
#' @param play if TRUE, plays back the accepted candidate at each iteration
#' @return A list containing the \code{history} of parameters tried and their
#'   final values (\code{pars}).
#' @export
#' @examples
#' \dontrun{
#' target = soundgen(sylLen = 600, pitch = c(300, 200),
#'                   rolloff = -20, play = TRUE, plot = TRUE)
#' # we hope to reproduce this sound
#'
#' # Match pars based on acoustic analysis alone, without any optimization.
#' # This *MAY* match temporal structure, pitch, and stationary formants
#' m1 = matchPars(target = target,
#'                samplingRate = 16000,
#'                maxIter = 0,  # no optimization, only acoustic analysis
#'                verbose = TRUE)
#' cand1 = do.call(soundgen, c(m1$pars, list(
#'   temperature = 0.001, play = TRUE, plot = TRUE)))
#'
#' # Try to improve the match by optimizing rolloff
#' # (this may take a few minutes to run, and the results may vary)
#' m2 = matchPars(target = target,
#'                samplingRate = 16000,
#'                pars = 'rolloff',
#'                maxIter = 100,
#'                verbose = TRUE)
#' # rolloff should be moving from default (-12) to target (-20):
#' lapply(m2$history, function(x) x$pars$rolloff)
#' cand2 = do.call(soundgen, c(m2$pars, list(play = TRUE, plot = TRUE)))
#' }
matchPars = function(target,
                     samplingRate = NULL,
                     pars = NULL,
                     specFun = 'melspec',
                     specFun_pars = list(),
                     init = NULL,
                     probMutation = .25,
                     stepVariance = 0.1,
                     maxIter = 50,
                     minExpectedDelta = 0.001,
                     compareSounds_pars = list(),
                     verbose = TRUE,
                     play = FALSE) {
  if (is.numeric(target) && is.null(samplingRate))
    stop("'samplingRate' must be provided when target is a numeric vector")
  if (maxIter > 0 && length(pars) < 1)
    stop(paste("No parameters for optimization are specified!",
               "Either list them in 'pars' or set 'maxIter = 0'"))
  tgt_audio = readAudio(target, samplingRate = samplingRate)
  if (is.null(samplingRate)) samplingRate = tgt_audio$samplingRate
  spec1 = getSpec(tgt_audio, specFun = specFun, specFun_pars = specFun_pars)

  ## initialize
  # start with default par values, unless initial values are provided
  parDefault = defaults[pars]
  parDefault[names(parDefault) %in% names(init)] = init[names(parDefault)]
  parDefault[['samplingRate']] = samplingRate

  # analyse the target and update the default pars
  aa = analyze(target, samplingRate = samplingRate, plot = FALSE,
               nFormants = 0, roughness = NULL, novelty = NULL, loudness = NULL)
  af = phonTools::findformants(tgt_audio$sound,
                               fs = tgt_audio$samplingRate, verify = FALSE)
  if (FALSE) {
    # syllable analysis
    as = suppressMessages(segment(target,
                                  samplingRate = samplingRate, plot = FALSE))
    parDefault$nSyl = max(as$summary$nSyl, 1)
    parDefault$sylLen = as$summary$sylLen_median
    medianPause = as$summary$pauseLen_median
  } else {
    # just a single pitch contour with NAs
    parDefault$sylLen = aa$summary$duration_noSilence * 1000
  }
  parDefault$pitch = aa$detailed$pitch
  # downsample F0 measures to 5 Hz
  # parDefault$pitch = resample(p, samplingRate_new = 5, samplingRate = 1 / 20 * 1000)
  if (is.list(af)) {
    if (nrow(af) > 0) {
      for (f in 1:min(3, nrow(af))) {  # add max 3 formants
        parDefault$formants[[paste0('f', f)]] = list(
          freq = round(af$formant[f]),
          width = round(af$bandwidth[f])
        )
      }
    }
  }

  # replace defaults with user-provided values, if any
  if (is.list(init)) {
    for (i in seq_along(init)) {
      if (!names(init)[i] %in% names(defaults)) {
        stop(paste('init parameter not recognized:', init[i]))
      }
      parDefault[[names(init)[i]]] = init[[i]]
    }
  }

  # calculate method of initial par values
  output = list(list(pars = parDefault, sim = NA))
  parLoop = parDefault
  cand = try(do.call(soundgen, parLoop), silent = FALSE)
  if (inherits(cand, 'try-error')) {
    stop ('Invalid initial pars')
  }
  output[[1]]$sim = suppressWarnings(1 - mean(do.call(compareSounds, c(list(
    x = spec1,
    y = cand,
    samplingRate = samplingRate,
    specFun = specFun,
    specFun_pars = specFun_pars
  ), compareSounds_pars))$distance))

  # iteratively mutate pars and save par values that improve fit to target ('sim')
  i = 1
  while (i < maxIter) {
    if (!is.numeric(length(pars)) || length(pars) < 1) {
      stop(paste("No parameters for optimization are specified!",
                 "Either list them in 'pars' or set 'maxIter = 0'"))
    }
    # mutate pars
    parMut = parLoop
    parMut = wigglePars(parMut,
                        parsToWiggle = pars,
                        probMutation = probMutation,
                        stepVariance = stepVariance)
    # generate a sound based on mutated pars
    cand = try(do.call(soundgen, parMut), silent = FALSE)
    if (inherits(cand, 'try-error')) {
      sim_new = -Inf
      delta = -Inf
      warning(paste('soundgen crashed with settings',
                    paste(parMut, collapse = ', ')))
    } else {
      # compare to target
      sim_new = suppressWarnings(1 - mean(do.call(compareSounds, c(list(
        x = spec1,
        y = cand,
        samplingRate = samplingRate,
        specFun = specFun,
        specFun_pars = specFun_pars
      ), compareSounds_pars))$distance))
      delta = sim_new - output[[length(output)]]$sim  # want to maximize similarity
    }

    condition = (delta > minExpectedDelta)
    if (condition) {
      i = 1  # reset the count of iterations
      output = c(output, list(list(pars = parMut,
                                   sim = sim_new)))
      parLoop = parMut
      if (verbose) print(paste('Best similarity: ', round(output[[length(output)]]$sim, 4)))
      if (play) playme(cand, samplingRate)
    } else {
      parMut = parLoop # back to previous step
    }
    i = i + 1
  }

  if (verbose && maxIter > 0) {
    if (length(output) == 1) {
      print('Failed to improve fit to target! Try increasing maxIter.')
    } else {
      print('Improved fit to target')
    }
  }

  list(history = output, pars = output[[length(output)]]$pars)
}


#' Reformat anchors
#' Checks that the anchors are formatted in a valid way and expands them to a
#' standard dataframe with two columns: time and value. NB: works for all
#' anchors except "noise", whose anchors have to be scaled by sylLen and are
#' therefore processed directly in soundgen()
#' @param anchors a numeric vector of values or a list/dataframe with one column
#'   (value) or two columns (time and value)
#' @param normalizeTime if TRUE, normalizes anchors$time values to range from 0 to 1
#' @param name,invalidArgAction if name is provided, attempts to verify the
#'   values with validatePars() against permittedValues
#' @noRd
#' @examples
#' soundgen:::reformatAnchors(150)
#' soundgen:::reformatAnchors(c(150, 200, 220))
#' soundgen:::reformatAnchors(anchors = list(value=c(150, 200, 220)))
#' soundgen:::reformatAnchors(anchors = list(time = c(0, 10, 100),
#'                                           value = c(150, 200, 220)))
#' # returns NA
#' soundgen:::reformatAnchors('aha')
#' # returns NA with a warning
#' soundgen:::reformatAnchors(anchors = list(time = c(0, .1, 1),
#'                                           freq = c(150, 200, 220)))
#'
#' # throws a warning and rearranges in order of time stamps
#' soundgen:::reformatAnchors(anchors = list(time = c(0, .8, .7, 1),
#'                                           value = c(150, 200, 150, 220)))
#'
#' # named anchors can be validated against permittedValues
#' reformatAnchors(c(300, 500, 15000), name = 'sylLen')
reformatAnchors = function(anchors,
                           normalizeTime = TRUE,
                           name = NULL,
                           invalidArgAction = 'adjust') {
  # Handle matrices explicitly so they aren't flattened by is.numeric()
  if (is.matrix(anchors)) {
    anchors = as.data.frame(anchors)
  }

  if (is.numeric(anchors)) {
    # for numeric vectors, assume these are equally spaced anchor values
    anchors_df = data.frame(
      time = seq(0, 1, length.out = max(2, length(anchors))),
      value = anchors
    )
  } else if (is.list(anchors)) {
    # for dataframes or lists, reformat if necessary
    if (!is.data.frame(anchors)) {
      anchors_df = as.data.frame(anchors)
    } else {
      anchors_df = anchors
    }
    if (ncol(anchors_df) == 1) {
      # if there is only one vector, again assume these are values
      anchors_df = data.frame(
        time = seq(0, 1, length.out = max(2, nrow(anchors_df))),
        value = anchors_df[, 1]
      )
    } else if (!identical(colnames(anchors_df), c('time', 'value'))) {
      warning(paste('An anchor should be either numeric or a dataframe',
                    'with two columns: time and value.'))
      return(NA)
    }
  } else {
    return(NA)
  }

  # remove points with the same time, if any
  anchors_df = anchors_df[!duplicated(anchors_df$time), , drop = FALSE]

  # make sure time values are in the right order
  if (any(diff(anchors_df$time) < 0)) {
    anchors_df = anchors_df[order(anchors_df$time), ]
    warning('Time stamps of anchors should increase monotonically; re-ordering...')
  }

  # make sure time ranges from 0 to 1
  if (normalizeTime && nrow(anchors_df) > 1)
    anchors_df$time = zeroOne(anchors_df$time)

  # check the range of values again permittedValues
  if (!is.null(name) && name %in% rownames(permittedValues)) {
    anchors_df$value = validatePars(name, anchors_df$value,
                                    permittedValues, invalidArgAction)
  }
  anchors_df
}
