### UTILITIES FOR SOUND GENERATION ###

#' Find zero crossing
#'
#' Internal soundgen function.
#'
#' \code{findZeroCrossing} looks for the last negative point before a zero
#' crossing as close as possible to the specified location. Since this is
#' primarily intended for joining waveforms without a click, this function only
#' looks at upward segments of a waveform (see example).
#'
#' @param ampl a vector of amplitudes oscillating around zero, such as a sound
#'   waveform
#' @param location the index indicating the desired location of a zero crossing
#' @return Returns the index of the last negative value before zero crossing
#'   closest to specified location.
#' @keywords internal
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
  if (len < 1 | location < 1 | location > len)
    return(NA)
  if (len == 1 & location == 1)
    return(1)
  zc_left = zc_right = NA

  # left of location
  if (location > 1) {
    i = location
    while (i > 1) {
      if (.subset2(ampl, i) > 0 & .subset2(ampl, i - 1) < 0) {
        zc_left = i - 1
        break
      }
      i = i - 1
    }
  }

  # right of location
  if (location < len)
    i = location
  while (i < (len - 1)) {
    if (.subset2(ampl, i + 1) > 0 & .subset2(ampl, i) < 0) {
      zc_right = i
      break
    }
    i = i + 1
  }

  if (is.na(zc_left) & is.na(zc_right)) return (NA)
  zc_nearest = which.min(abs(location - c(zc_left, zc_right)))
  if (zc_nearest == 1) {
    return(zc_left)
  } else if (zc_nearest == 2) {
    return(zc_right)
  } else {
    return(NA) # zc not found
  }
}


#' Upsample glottal cycles
#'
#' Internal soundgen function.
#'
#' Upsamples a pitch contour to samplingRate through linear interpolation
#' between successive glottal cycles.
#' @param pitch_per_gc a vector of fundamental frequencies per glottal cycle
#' @param samplingRate target sampling rate after upsampling, in Hz
#' @return Returns a list of two vectors: pitch_upsampled (the upsampled version
#'   of the input) and gc_upsampled (new indices of glottal cycles on an
#'   upsampled scale)
#' @keywords internal
#' @examples
#' soundgen:::upsampleGC(pitch_per_gc = c(100, 150, 130), samplingRate = 16000)
upsampleGC = function(pitch_per_gc, samplingRate = 16000) {
  l = length(pitch_per_gc)
  gccrossLenPoints = round(samplingRate / pitch_per_gc)
  c = cumsum(gccrossLenPoints)
  gc_upsampled = c(1, c)

  if (l == 1) {
    pitch_upsampled = rep(pitch_per_gc, gccrossLenPoints)
  } else if (l == 2) {
    pitch_upsampled = seq(pitch_per_gc[1], pitch_per_gc[2],
                          length.out = sum(gccrossLenPoints))
  } else {
    # find time stamps (in gc) corresponding to centers of each pitch value
    t = rep(1, l) # start at 1
    t[l] = sum(gccrossLenPoints)  # end at total number of gc
    for (i in 2:(l - 1)) {
      t[i] = c[i - 1] + round(gccrossLenPoints[i] / 2)
    }
    pitch_upsampled = spline(x = t,
                             y = pitch_per_gc,
                             n = tail(c, 1))$y
  }
  # plot(pitch_upsampled, type = 'l')
  list(pitch = pitch_upsampled, gc = gc_upsampled)
}


#' Divide f0 contour into glottal cycles
#'
#' Internal soundgen function.
#'
#' Returns a vector of indices giving the borders between "glottal cycles",
#' assuming that we know the true f0 at each time point (as we do in synthesized
#' sounds). The first index is always 1.
#' @param pitch a vector of fundamental frequency values
#' @param samplingRate sampling rate at which f0 values are provided
#' @keywords internal
#' @examples
#' # 100 ms of audio with f0 steadily increasing from 150 to 200 Hz
#' soundgen:::getGlottalCycles(seq(150, 200, length.out = 350),
#'   samplingRate = 3500)
getGlottalCycles = function (pitch, samplingRate) {
  if (length(pitch) < 2) return(1)
  glottalCycles = numeric()
  i = 1 # the first border is the first time point
  while (i < length(pitch)) {
    glottalCycles = c(glottalCycles, i)
    # take steps proportionate to the current F0
    i = i + max(2, floor(samplingRate / pitch[i]))
  }
  glottalCycles
}


#' Syllable structure of a bout
#'
#' Internal soundgen function.
#'
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
#' @param plot produce a plot of syllable structure?
#' @inheritParams soundgen
#' @return Returns a matrix with a list of start-end points for syllables
#' @keywords internal
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
    invalidArgAction = c('adjust', 'abort', 'ignore')[1],
    plot = FALSE) {
  if (nSyl == 1) {
    # no variation for a single syllable
    out = data.frame(start = 0, end = sylLen)
  } else {
    # up- or downsample durations to nSyl
    if (length(sylLen) > 1 & length(sylLen) != nSyl) {
      sylLen = getSmoothContour(anchors = sylLen, len = nSyl)
    }
    if (length(pauseLen) > 1 & length(pauseLen) != (nSyl - 1)) {
      pauseLen = getSmoothContour(anchors = pauseLen, len = nSyl - 1)
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
  }
  out$dur = out$end - out$start

  if (plot) {
    # for the UI
    t = 1:max(out)
    plot(t, rep(1, length(t)), type = 'n', xlab = 'Time, ms', ylab = '',
         bty = 'n', yaxt = 'n', ylim = c(0.8, 1.2))
    for (i in 1:nrow(out)) {
      rect(xleft = out[i, 1], xright = out[i, 2], ybottom = .9, ytop = 1.1,
           col = 'blue')
      text(x = mean(c(out[i, 2], out[i, 1])), y = 1,
           col = 'yellow', cex = 5, labels = i)
    }
  }
  out
}


#' Randomly modify anchors
#'
#' Internal soundgen function.
#'
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
#' @keywords internal
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
                         invalidArgAction = c('adjust', 'abort', 'ignore')[1]) {
  if (temperature == 0 | temp_coef == 0) return(df)
  if (any(is.na(df))) return(NA)
  if (!is.data.frame(df)) df = as.data.frame(df)
  nc = ncol(df)
  nr = nrow(df)
  if (nc != length(low) |
      nc != length(high) |
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
                    as.numeric(df[1, idx] * temperature * temp_coef)),
        low = low[idx],
        high = high[idx],
        roundToInteger = roundToInteger,
        invalidArgAction = invalidArgAction))
      if (inherits(newAnchor, 'try-error')) {
        stop(paste('Failed to add an anchor to df:', paste(df, collapse = ', ')))
      } else {
        df = rbind(df, c(1, newAnchor))
        df[1, 1] = 0  # make time c(0, 1)
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
      newAnchor = colMeans(df[i1:i2, ])
      df = rbind(df[1:i1, ],
                 newAnchor,
                 df[i2:nrow(df), ])
    }
  } else if (action == 'remove') {
    # print('removing an anchor')
    if (wiggleAllRows) {
      # we can remove any anchor
      idx = sample(seq_len(nrow(df)), 1)
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
  rownames(df) = seq_len(nrow(df))  # in case we added / removed an anchor

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
    ranges = as.numeric(apply(df, 2, function(x) abs(diff(range(x)))))
    # if no variation in values, defaults to value
    z = which(ranges == 0)
    ranges[z] = abs(as.numeric(df[1, z]))
  }
  for (i in seq_len(ncol(df))) {
    w = try(rnorm_truncated2(
      n = nrow(df),
      mean = as.numeric(df[, i]),
      sd = if (i > 1 & !is.null(sd_values)) sd_values else
        as.numeric(ranges[i] * temperature * temp_coef),
      low = low[i],
      high = high[i],
      roundToInteger = roundToInteger,
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
  df[order(df$time), ]
}


#' Scale noise anchors
#'
#' Internal soundgen function.
#'
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
#' @keywords internal
#' @examples
#' noiseTime = c(-20, 50, 120)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 100, sylLen_new = 200)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 100, sylLen_new = 50)
#' soundgen:::scaleNoiseAnchors(noiseTime, sylLen_old = 200, sylLen_new = 300)
scaleNoiseAnchors = function(noiseTime, sylLen_old, sylLen_new) {
  idx_mid = which(noiseTime > 0 &             # not before syl
                    noiseTime < sylLen_old)   # not after syl
  idx_after = which(noiseTime >= sylLen_old)  # after syl
  noiseTime[idx_mid] = noiseTime[idx_mid] * sylLen_new / sylLen_old
  noiseTime[idx_after] = noiseTime[idx_after] - sylLen_old + sylLen_new
  noiseTime
}


#' Wiggle glottal cycles
#'
#' Internal soundgen function
#'
#' Helper function for preparing a vector of multiplication factors for adding
#' jitter and shimmer per glottal cycle. Generates random anchors for each
#' jitter/shimmer period and draws a smooth contour between them by spline
#' interpolation.
#' @param dep a vector of any length specifying the strengh of applied effect as
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
#' @keywords internal
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
  # if (length(dep) > 1) dep = getSmoothContour(dep, len = nGC)
  # if (length(len) > 1) len = getSmoothContour(len, len = nGC)
  ratio = pitch_per_gc * len / 1000 # the number of gc that make
  #   up one period of effect (vector of length nGC)
  idx = 1
  i = 1
  while (i < nGC) {
    i = tail(idx, 1) + ratio[i]
    idx = c(idx, i)
  }
  idx = round(idx)
  idx = idx[idx <= nGC]
  idx = unique(idx)  # pitch for these gc will be wiggled
  len_dep = length(dep)
  if (len_dep == 1) {
    dep_idx = rep(dep, length(idx))
  } else {
    dep_idx = approx(dep, n = length(idx))$y
  }
  effect = 2 ^ (rnorm(
    n = length(idx),
    mean = 0,
    sd = dep_idx
  ) * rw[idx] * effect_on[idx])
  # plot(effect, type = 'b')

  # upsample to length nGC
  effect_per_gc = spline(effect, n = nGC, x = idx)$y
  # plot(effect_per_gc, type = 'b')
  effect_per_gc
}


#' Validate parameters
#'
#' Internal soundgen function
#'
#' Checks whether the value of a numeric parameter falls within the allowed
#' range. Options: abort, reset to default, throw a warning and continue.
#' @param p parameter name
#' @param gp parameter value
#' @param def matrix or dataframe containing reference values (low, high,
#'   default)
#' @param invalidArgAction what to do if an argument is invalid or outside the
#'   range: 'adjust' = reset to default value, 'abort' = stop execution,
#'   'ignore' = throw a warning and continue (may crash)
#' @keywords internal
validatePars = function(p, gp, def,
                        invalidArgAction = c('adjust', 'abort', 'ignore')[1]) {
  if (any(gp < def[p, 'low']) |
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
      # reset p to default, with a warning
      gp = def[p, 'default']
      warning(paste0(
        "\n", p, " should be between ", def[p, 'low'],  " and ",
        def[p, 'high'], "; resetting to ", def[p, 'default'],
        ". Use invalidArgAction = 'ignore' to override"))
    }
  }
  gp
}


#' Object to string
#'
#' Internal soundgen function. Converts any object to a string that preserves all internal structure and names.
#' @param x any R object (unquoted)
#' @keywords internal
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
  if (is.character(x)) {
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
#'
#' Internal soundgen function
#'
#' Fills specified segments with silence (0) and fades in-out the ends of the
#' silenced segment.
#' @param x sound as a numeric vector
#' @param samplingRate sampling rate, Hz
#' @param na_seg dataframe containing columns "start_prop" and "end_prop"
#' @param attackLen attack length, ms
#' @keywords internal
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
  for (r in 1:nrow(na_seg)) {
    idx_start = round(na_seg$prop_start[r] * ls)
    idx_end = round(na_seg$prop_end[r] * ls)
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
          fadeOut_points = l[2])
      }
      if (na_seg$prop_end[r] < 1) {
        # fade out the start of the next syl
        fade_to = min(ls, idx_end + l[1])
        fade_idx = idx_end:fade_to
        x[fade_idx] = .fade(
          list(sound = x[fade_idx], samplingRate = samplingRate),
          fadeIn_points = l[1],
          fadeOut_points = 0)
      }
    }
    # spectrogram(x, samplingRate)
  }
  x
}


#' Wiggle parameters
#'
#' Internal soundgen function
#'
#' Helper function for \code{\link{matchPars}}. Takes a list of control
#' parameters for \code{\link{soundgen}} and introduces some random variation in
#' their values.
#' @param parList full list of considered parameters
#' @param parsToWiggle a list of the names of pars that might be mutated
#' @inheritParams matchPars
#' @keywords internal
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
  parsToRound = c('repeatBout', 'nSyl', 'rolloffParabHarm')

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
      if (p == 'formants' | p == 'formantsNoise') {  # formants
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
        } else if (p == 'ampl' |
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
