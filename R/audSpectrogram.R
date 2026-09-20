#' Auditory spectrogram
#'
#' Produces an auditory spectrogram by convolving the sound with a bank of
#' bandpass filters. The main difference from STFT is that we don't window the
#' signal and de facto get variable temporal resolution in different frequency
#' channels, as with a wavelet transform. The key settings are
#' \code{filterType}, \code{nFilters_oct}, and \code{yScale}, which determine the
#' type, number, and spacing of the filters, respectively. Gammatone filters
#' were designed as a simple approximation of human perception - see Slaney 1993
#' "An Efficient Implementation of the Patterson–Holdsworth Auditory Filter
#' Bank". Butterworth or Chebyshev filters are not meant to model perception,
#' but can be useful for quickly plotting a sound.
#'
#' @inheritParams .roxygen_defaults
#' @inheritParams spectrogram
#' @param step step, ms (determines time resolution of the plot, but not of the
#'   returned envelopes per channel). step = NULL means no downsampling at all
#'   when \code{envelope = "hil"} (ncol of output = length of input audio) and a
#'   default of 5 ms when \code{envelope = "rms"}
#' @param filterType "butterworth" = Butterworth filter (IIR)
#'   \code{\link[signal]{butter}}, "chebyshev" = Chebyshev filter (IIR)
#'   \code{\link[signal]{cheby1}}, "gammatone" = gammatone filter (FIR)
#' @param envelope the method of computing the envelope of each channel: "rms" =
#'   root mean square per window, which is faster but gives limited time
#'   resolution (default), "hil" = analytic envelope obtained with a Hilbert
#'   transform, low-pass filtered and downsampled unless \code{step = NULL},
#'   which is slower but gives the best possible time resolution. As a simple
#'   heuristic, you may want to use "hil" if your desired time step is smaller
#'   than ~5 ms
#' @param nFilters_oct the approximate number of filters per octave between
#'   \code{minFreq} and \code{maxFreq}; the actual resolution depends on
#'   \code{yScale}: for instance, if \code{yScale = 'ERB'}, center frequencies
#'   are equally spaced on the ERB scale (set \code{plotFilters = TRUE} to check)
#' @param nFilters an alternative way to specify frequency resolution: if
#'   specified, overrides \code{nFilters_oct}
#' @param yScale determines the location of center frequencies of the filters
#' @param filterOrder filter order (defaults to 4 for gammatones, 3 otherwise)
#' @param bandwidth filter bandwidth, octaves; if NULL, defaults to ERB
#'   bandwidths
#' @param bandwidthMult a scaling factor for all bandwidths (1 = no effect)
#' @param minFreq,maxFreq the range of frequencies to analyze. If the
#'   spectrogram looks empty, try increasing minFreq - the lowest filters are
#'   prone to returning very large values, which can make the rest of the
#'   spectrogram look empty
#' @param minBandwidth minimum filter bandwidth, Hz (otherwise filters may
#'   become too narrow when nFilters is high); only affects Butterworth and
#'   Chebyshev filters, not gammatones
#' @param output character vector specifying which measures to return.
#'   Defaults to everything, but this takes a lot of RAM, so shorten to
#'   what's needed if analyzing many files at once
#' @param plotFilters if TRUE, plots the filters as central frequencies ±
#'   bandwidth/2
#'
#' @return A list for each analyzed file, including:
#'   \describe{
#'     \item{audSpec}{auditory spectrogram: a matrix with frequency in rows
#'       (kHz) and time in columns (ms), offset by step/2}
#'     \item{audSpec_processed}{same dimensions, rescaled for plotting
#'       (log-transformed, contrast/brightness-adjusted, range 0–1)}
#'     \item{filterbank}{raw filter outputs: a matrix with one row per filter
#'       (ordered by center frequency) and one column per audio sample}
#'     \item{filterbank_env}{Hilbert envelopes of the filterbank, same
#'       dimensions as \code{filterbank}; NA if \code{envelope = "rms"}}
#'     \item{filters}{a dataframe giving the center frequencies, bandwidths, and
#'     lower/upper bounds of the used filters, all in Hz}
#'   }
#'
#' @export
#' @examples
#' data('speechEx', package = 'soundgen')
#'
#' # auditory spectrogram
#' asp = audSpectrogram(speechEx, to = 1, step = 5)
#' dim(asp$audSpec)
#'
#' # compare to STFT with similar time and frequency resolution (~100 times faster)
#' fs = spectrogram(speechEx, to = 1, yScale = 'ERB', windowLength = 5, step = 5)
#' dim(fs)
#'
#' \dontrun{
#' # add bells and whistles
#' audSpectrogram(speechEx,
#'   nFilters = 128,
#'   dynamicRange = 150,
#'   osc = 'none',
#'   heights = c(2, 1),  # spectro/osc height ratio
#'   contrast = .4,  # increase contrast
#'   brightness = -.2,  # reduce brightness
#'   colorTheme = 'matlab',  # pick color theme...
#'   # col = hcl.colors(100, palette = 'Plasma'),  # ...or specify the colors
#'   cex.lab = .75, cex.axis = .75,  # text size and other base graphics pars
#'   grid = 5,  # to customize, add manually with graphics::grid()
#'   ylim = c(0.05, 8),  # always in kHz
#'   main = 'My auditory spectrogram' # title
#'   # + axis labels, etc
#' )
#'
#' # NB: frequency resolution is controlled by both nFilters and bandwidth
#' audSpectrogram(speechEx, to = 1, nFilters = 15, bandwidth = 1/2)
#' audSpectrogram(speechEx, to = 1, nFilters = 15, bandwidth = 1/10)
#' audSpectrogram(speechEx, to = 1, nFilters = 100, bandwidth = 1/2)
#' audSpectrogram(speechEx, to = 1, nFilters = 100, bandwidth = 1/10)
#' audSpectrogram(speechEx, to = 1, nFilters_oct = 5, bandwidth = 1/10)
#' audSpectrogram(speechEx, to = 1, nFilters = 200, bandwidthMult = 1/3)
#'
#' # caution: if bandwidths are too narrow relative to nFilters, there may be gaps
#' audSpectrogram(speechEx, to = 1, nFilters = 30, bandwidthMult = 1/3,
#'   plotFilters = TRUE, plot = FALSE)
#'
#' # different filter types
#' audSpectrogram(speechEx, to = 1, filterType = 'gammatone')
#' audSpectrogram(speechEx, to = 1, filterType = 'butterworth')
#' audSpectrogram(speechEx, to = 1, filterType = 'chebyshev')
#'
#' # save auditory spectrograms of all audio files in a folder
#' audSpectrogram('~/Downloads/temp', savePlots = TRUE, cores = 4)
#' }
audSpectrogram = function(
    x,
    samplingRate = NULL,
    scale = NULL,
    from = NULL,
    to = NULL,
    step = 10,
    dynamicRange = 80,
    filterType = c('gammatone', 'butterworth', 'chebyshev'),
    envelope = c('rms', 'hil'),
    nFilters_oct = 6,
    nFilters = NULL,
    yScale = c('ERB', 'bark', 'mel', 'log'),
    filterOrder = NULL,
    bandwidth = NULL,
    bandwidthMult = 1,
    minFreq = 20,
    maxFreq = NULL,
    minBandwidth = 10,
    output = c('all', 'audSpec', 'audSpec_processed', 'filterbank', 'filterbank_env', 'filters'),
    reportEvery = NULL,
    cores = 1,
    plot = TRUE,
    savePlots = FALSE,
    embed = FALSE,
    plotFilters = FALSE,
    osc = c('linear', 'dB', 'none'),
    heights = c(3, 1),
    ylim = NULL,
    contrast = 0,
    brightness = 0,
    maxPoints = c(1e5, 5e5),
    colorTheme = 'bw',
    col = NULL,
    extraContour = NULL,
    xlab = NULL,
    ylab = NULL,
    xaxp = NULL,
    mar = c(5.1, 4.1, 4.1, 2),
    main = NULL,
    grid = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  # match args
  filterType = match.arg(filterType)
  osc = match.arg(osc)
  yScale = match.arg(yScale)
  envelope = match.arg(envelope)
  output = match.arg(
    output,
    choices = c('audSpec', 'audSpec_processed', 'filterbank', 'filterbank_env', 'filters', 'all'),
    several.ok = TRUE
  )
  if ('all' %in% output) output = c('audSpec', 'audSpec_processed', 'filterbank',
                                    'filterbank_env', 'filters')
  output = unique(output)
  myPars = c(as.list(environment()), list(...))

  # exclude some args
  myPars = myPars[!names(myPars) %in% c(
    'x', 'samplingRate', 'scale', 'from', 'to',
    'reportEvery', 'cores', 'savePlots', 'embed')]

  # call .audSpectrogram
  pa = processAudio(
    x,
    samplingRate = samplingRate,
    scale = scale,
    from = from,
    to = to,
    funToCall = '.audSpectrogram',
    suffix = "audSpectrogram",
    savePlots = savePlots,
    myPars = myPars,
    reportEvery = reportEvery,
    cores = cores
  )

  # htmlPlots
  if (isTRUE(savePlots) && pa$input$n > 1) {
    try(htmlPlots(pa$input, changesAudio = FALSE, suffix = "audSpectrogram",
                  width = paste0(width, units), embed = embed))
  }
  if (pa$input$n == 1) pa$result = pa$result[[1]]
  invisible(pa$result)
}


#' Auditory spectrogram per sound
#' @noRd
.audSpectrogram = function(
    audio,
    step = 10,
    dynamicRange = 80,
    filterType = 'gammatone',
    envelope = 'rms',
    nFilters_oct = 6,
    nFilters = NULL,
    yScale = 'ERB',
    filterOrder = NULL,
    bandwidth = NULL,
    bandwidthMult = 1,
    minFreq = 20,
    maxFreq = NULL,
    minBandwidth = 10,
    output = c('audSpec', 'audSpec_processed', 'filterbank', 'filterbank_env', 'filters'),
    plot = FALSE,
    plotFilters = FALSE,
    osc = 'linear',
    heights = c(3, 1),
    ylim = NULL,
    contrast = 0,
    brightness = 0,
    maxPoints = c(1e5, 5e5),
    colorTheme = 'bw',
    col = NULL,
    extraContour = NULL,
    xlab = NULL,
    ylab = NULL,
    xaxp = NULL,
    mar = c(5.1, 4.1, 4.1, 2),
    main = NULL,
    grid = NULL,
    width = 900,
    height = 500,
    units = 'px',
    res = NA,
    ...
) {
  ## input validation
  if (any(!is.finite(audio$sound)))
    stop('the input cannot contain non-finite values')
  if (audio$ls == 0 || !any(abs(audio$sound) > 0))
    stop('nothing to do: the input is silent')
  # center
  audio$sound = audio$sound - mean(audio$sound)
  nyquist = audio$samplingRate / 2

  max_allowed_freq = 0.90 * nyquist  # no filters too close to nyquist
  if (is.null(filterOrder)) filterOrder = if (filterType == 'gammatone') 4 else 3
  if (length(filterOrder) != 1 || !is.finite(filterOrder) || filterOrder < 1)
    stop("filterOrder must be a positive integer")

  if (is.null(maxFreq) || !is.finite(maxFreq) || length(maxFreq) < 1) {
    maxFreq = max_allowed_freq
  } else {
    maxFreq = min(maxFreq, max_allowed_freq)
  }
  if (!is.finite(minFreq) || minFreq <= 0) stop("minFreq must be positive")
  if (!is.finite(maxFreq) || maxFreq <= minFreq) stop("maxFreq must be greater than minFreq")

  if (!is.null(step)) {
    len = max(1, round(audio$duration * 1000 / step))
    step = audio$duration * 1000 / len  # avoid rounding error
  } else {
    if (envelope == 'hil') {
      step = 1000 / audio$samplingRate
      len = audio$ls
    } else if (envelope == 'rms') {
      step = 5
      len = max(1, round(audio$duration * 1000 / step))
      step = audio$duration * 1000 / len  # avoid rounding error
      warning('specify a step if using envelope = "rms"; defaulting to 5 ms')
    }
  }

  if (is.null(nFilters)) {
    if (!is.null(nFilters_oct)) {
      nFilters = round(log2(maxFreq / minFreq) * nFilters_oct)
    } else {
      stop('either nFilters or nFilters_oct must be specified')
    }
  }
  if (!is.finite(nFilters) || nFilters < 1)
    stop("nFilters must be a positive integer")
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
  }
  if (plot) {
    # need to return the spectrogram if we want a plot
    output = unique(c(output, "audSpec", "audSpec_processed"))
  }

  ## set up the filters
  cf = otherToHz(seq(HzToOther(minFreq, yScale), HzToOther(maxFreq, yScale),
                     length.out = nFilters), yScale)
  filters = data.frame(cf = cf)
  eps_hz = max(1, 1e-5 * nyquist)
  if (is.null(bandwidth)) {
    # default bandwidths from Slaney 1993 / Glasberg & Moore (1990)
    filters$bandwidth = 24.7 * (4.37 * filters$cf / 1000 + 1) * bandwidthMult
    filters$from = pmax(filters$cf - filters$bandwidth / 2, eps_hz)
    filters$to = pmin(filters$cf + filters$bandwidth / 2, 0.95 * nyquist)
  } else {
    # user-specified bandwidths in octaves: cf = geometric center
    half_oct = bandwidth / 2 * bandwidthMult
    filters$from = pmax(cf * 2^(-half_oct), eps_hz)
    filters$to   = pmin(cf * 2^( half_oct), 0.95 * nyquist)
  }
  filters$bandwidth = filters$to - filters$from

  # make sure the bandwidth in Hz (!) is always wide enough to avoid crashing
  if (filterType == 'gammatone') minBandwidth = 0.1
  idx_narrow = which(filters$bandwidth < minBandwidth)
  if (length(idx_narrow) > 0) {
    half_bw_hz = ceiling(minBandwidth / 2)
    filters$from[idx_narrow] = filters$cf[idx_narrow] - half_bw_hz
    filters$to[idx_narrow] = filters$cf[idx_narrow] + half_bw_hz
  }
  filters$from[filters$from < eps_hz] = eps_hz
  filters$to[filters$to > (nyquist * .95)] = nyquist * .95
  filters$bandwidth = filters$to - filters$from
  valid_filter =
    is.finite(filters$from) &
    is.finite(filters$to) &
    filters$from >= eps_hz &
    filters$to <= (nyquist * .95) &
    filters$from < filters$to &
    filters$bandwidth > minBandwidth
  if (any(!valid_filter)) {
    filters = filters[valid_filter, ]
    nFilters = nrow(filters)
    cf = filters$cf
    if (nrow(filters) < 1) stop('failed to construct any valid filters')
  }

  # summary(filters)

  # plot the filters
  if (plotFilters) {
    pf = filters  # avoid modifying the actual returned "filters"
    y = 1 + 5 / 1000 * (seq_len(nrow(pf)) - nrow(pf) / 2)
    pf[, c('cfz', 'fromz', 'toz')] = apply(pf[, c('cf', 'from', 'to')], 2,
                                           HzToOther, scale = yScale)
    plot(pf$cfz, rep(1, nrow(pf)), type = 'n', yaxt = 'n',
         ylim = range(y), bty = 'n', ylab = '', xlab = paste('Center frequency,', yScale))
    for (i in seq_len(nrow(pf))) {
      segments(x0 = pf$fromz[i], x1 = pf$toz[i], y0 = y[i], y1 = y[i])
    }
    lbls_Hz = pretty(pf$cf)
    axis(3, at = round(HzToOther(lbls_Hz, yScale)), labels = lbls_Hz)
    mtext('Center frequency, Hz', side = 3, line = 2.5)
  }

  # prepare for downsampling routines
  if (len != audio$ls) {
    if (envelope == 'hil') {
      idx = unique(round(seq(1, audio$ls, length.out = len)))
      if (length(idx) != len) {
        # skip downsampling altogether as step is very small anyway
        idx = seq_len(audio$ls)
        len = audio$ls
        step = 1000 / audio$samplingRate
      }
      sr_env = 1000 / step
    } else if (envelope == 'rms') {
      step_points = max(1, round(step / 1000 * audio$samplingRate))
      # ensure at least 2 periods of the carrier are in the window (longer
      # averaging windows are necessary for low-frequency channels)
      min_window_points = ceiling(2 / filters$cf * audio$samplingRate)
      wl = pmax(min_window_points, step_points)
      wl = pmin(wl, audio$ls)
      # with these wl, overlap drops to 0% for high-freq channels
      idx = 1 +  step_points * (0:(len - 1))
    }
  } else {
    step_points = 1
    wl = rep(1, nFilters)
    idx = 1:audio$ls
  }


  ## apply the filters
  fb = fb_env = sp = vector('list', nFilters)
  for (i in seq_len(nFilters)) {
    # bandpass-filter the signal
    if (filterType == 'butterworth') {
      # design the filter
      btord = signal::FilterOfOrder(
        n = filterOrder,
        Wc = c(filters$from[i], filters$to[i]) / nyquist,
        type = 'pass'
      )
      bt = signal::butter(btord)

      # check filter stability
      # poles = polyroot(bt$a)
      # stable = all(Mod(poles) < 1 - 1e-6)
      fr = signal::freqz(bt, n = 4096, Fs = audio$samplingRate)
      gain = max(abs(fr$h), na.rm = TRUE)
      if (!is.finite(gain) || gain == 0) {
        # zero the channel
        fb[[i]] = fb_env[[i]] = rep(0, audio$ls)
        sp[[i]] = matrix(0, nrow = 1, ncol = len)
        next
      }

      # apply the filter
      fb[[i]] = signal::filter(filt = bt, x = audio$sound) / gain
      # possibly more stable (but takes 6 times longer and requires gsignal): sos form
      # bt_sos = gsignal::tf2sos(b = bt$b, a = bt$a)
      # fb[[i]] = gsignal::filter(filt = bt_sos, x = audio$sound)
      # or forward and backward filter to avoid a phase delay - rings in both directions, not physiological
      # fb[[i]] = signal::filtfilt(filt = bt, x = audio$sound)
    } else if (filterType == 'chebyshev') {
      btord = signal::FilterOfOrder(
        n = filterOrder,
        Wc = c(filters$from[i], filters$to[i]) / nyquist,
        type = 'pass'
      )
      ch = signal::cheby1(btord, Rp = 0.5)

      # check filter stability
      fr = signal::freqz(ch, n = 4096, Fs = audio$samplingRate)
      gain = max(abs(fr$h), na.rm = TRUE)
      if (!is.finite(gain) || gain == 0) {
        # zero the channel
        fb[[i]] = fb_env[[i]] = rep(0, audio$ls)
        sp[[i]] = matrix(0, nrow = 1, ncol = len)
        next
      }

      # apply the filter
      fb[[i]] = signal::filter(filt = ch, x = audio$sound) / gain
    } else if (filterType == 'gammatone') {
      # decay to about -60 dB or 10 fundamental periods of cf, but max half the dur of audio
      a = 2 * pi * 1.019 * filters$bandwidth[i]
      t_decay = (filterOrder - 1 + 6.9) / a
      t_cycles = 10 / filters$cf[i]
      d = min(max(t_cycles, t_decay), audio$duration / 2)
      n_filt = max(1, round(audio$samplingRate * d))
      tt = (seq_len(n_filt) - 1) / audio$samplingRate
      filter_i = tt^(filterOrder - 1) *
        exp(-2 * pi * 1.019 * filters$bandwidth[i] * tt) *
        cos(2 * pi * filters$cf[i] * tt)
      # plot(filter_i, type = 'l')

      # check filter stability
      nfft = 2^ceiling(log2(max(4096, 2 * n_filt)))
      hpad = c(filter_i, rep(0, nfft - n_filt))
      H = fft(hpad)
      gain = max(abs(H))
      if (!is.finite(gain) || gain == 0) {
        # zero the channel
        fb[[i]] = fb_env[[i]] = rep(0, audio$ls)
        sp[[i]] = matrix(0, nrow = 1, ncol = len)
        next
      }

      # apply the filter
      # audio_filt = stats::convolve(audio$sound, filter_i, type = 'filter') / gain
      # fb[[i]] = matchLengths(audio_filt, audio$ls)
      # or as FIR, ~10 times faster than convolve:
      # fir = signal::Ma(filter_i / gain)
      # fb[[i]] = signal::filter(fir, audio$sound)
      # or with FFT, ~100 times faster than convolve
      fb[[i]] = signal::fftfilt(filter_i / gain, audio$sound)
    }


    # # double-check that the filtered signal is reasonable relative to input scale
    # if (max(abs(fb[[i]])) > 100 * audio$scale_used) {
    #   # zero the channel
    #   fb[[i]] = fb_env[[i]] = rep(0, audio$ls)
    #   sp[[i]] = matrix(0, nrow = 1, ncol = len)
    #   next
    # }

    # extract the envelope
    if (envelope == 'rms') {
      # rms per window - fast, doesn't require FFT or even low-pass filtering,
      # but limited time resolution
      signal_i = c(fb[[i]], rep(0, wl[i] + step_points))
      fb_env_i = vapply(idx, function(x) {
        sqrt(mean(signal_i [x:(wl[i] + x - 1)] ^ 2))
      }, numeric(1))
      sp[[i]] = matrix(fb_env_i, nrow = 1)
    } else if (envelope == 'hil') {
      # analytic signal: much slower, but better time resolution if needed to
      # detect fast modulation (up to audio$samplingRate)
      fb_env_i = try(hilbert_approx(fb[[i]])$envelope)
      if (!inherits(fb_env_i, 'try-error') && !any(!is.finite(fb_env_i))) {
        fb_env[[i]] = fb_env_i
        # plot(fb_env_i, type = 'l')
        if (len != audio$ls) {
          # low-pass filter and downsample
          fc_env = min(
            0.45 * sr_env,          # slightly below new Nyquist
            filters$bandwidth[i]    # envelope cannot meaningfully exceed channel bandwidth
          )
          fb_env_i = pitchSmoothPraat(fb_env_i, samplingRate = audio$samplingRate,
                                      bandwidth = fc_env, preprocess = FALSE)
          fb_env_i = fb_env_i[idx]
        }
        sp[[i]] = matrix(fb_env_i, nrow = 1)
      } else {
        # zero the channel
        fb[[i]] = fb_env[[i]] = rep(0, audio$ls)
        sp[[i]] = matrix(0, nrow = 1, ncol = len)
      }
    }
  }

  # prepare output
  if ('audSpec' %in% output || 'audSpec_processed' %in% output) {
    # audSpec: frequency in rows, time in columns (standard orientation)
    audSpec = do.call(rbind, sp)
    # safety: envelopes should be finite and non-negative
    audSpec[!is.finite(audSpec) | audSpec < 0] = 0
    rownames(audSpec) = names(fb) = names(fb_env) = filters$cf / 1000
    if (envelope == 'hil') {
      # idx gives the exact location
      colnames(audSpec) = (idx - 1) / audio$samplingRate * 1000 +
        audio$timeShift * 1000
    } else if (envelope == 'rms') {
      # offset times by step/2 (only an approximation as the actual center of
      # each frame depends on wl, which is greater for low frequency channels)
      colnames(audSpec) = colnames(audSpec) = (idx - 1) / audio$samplingRate * 1000 +
        audio$timeShift * 1000 + step / 2
    }

    # rescale for plotting (all operations are element-wise, orientation irrelevant)
    Z1 = audSpec
    # set to zero under dynamic range, log-transform
    Z1 = floor_log(Z1, dynamicRange = dynamicRange)

    # contrast
    contrast_exp = exp(3 * contrast)
    if (contrast_exp != 1) {
      Z1 = Z1 ^ contrast_exp
    }
    if (any(Z1 != 0)) Z1 = Z1 / max(Z1)  # now max(Z1) = 1

    # brightness: smooth sigmoid transfer curve (same as spectrogram())
    if (brightness != 0) {
      k_max = 10  # steepness at |brightness| = 1
      k = abs(brightness) * k_max  # 0 --> identity
      m = 0.5 + brightness / 2  # inflection point
      sig = function(u) 1 / (1 + exp(-k * (u - m)))
      s0 = sig(0)
      s1 = sig(1)
      Z1 = (sig(Z1) - s0) / (s1 - s0)  # pinned: f(0) = 0, f(1) = 1
    }
    Z1[Z1 < 0] = 0
  } else {
    audSpec = Z1 = NA
  }

  # PLOTTING
  if (isTRUE(audio$savePlots)) {
    plot = TRUE
    png(filename = file.path(audio$path_output, paste0(audio$filename_noExt, ".png")),
        width = width, height = height, units = units, res = res)
    on.exit(dev.off())
  }
  if (plot) {
    if (is.null(ylim)) ylim = c(minFreq, maxFreq) / 1000
    plotSpec(
      X = as.numeric(colnames(audSpec)),  # time, ms
      Y = as.numeric(rownames(audSpec)),  # freq, kHz
      Z = Z1,  # freq (rows) x time (cols)
      audio = audio, internal = NULL, dynamicRange = dynamicRange,
      osc = osc, heights = heights, ylim = ylim, yScale = yScale,
      maxPoints = maxPoints, colorTheme = colorTheme, col = col,
      extraContour = extraContour,
      xlab = xlab, ylab = ylab, xaxp = xaxp,
      mar = mar, main = main, grid = grid,
      ...
    )
  }

  res = list()
  if ("audSpec" %in% output) res$audSpec = audSpec
  if ("audSpec_processed" %in% output) res$audSpec_processed = Z1
  if ("filterbank" %in% output) res$filterbank = do.call(rbind, fb)
  if ("filterbank_env" %in% output) {
    if (envelope == 'hil') {
      res$filterbank_env = do.call(rbind, fb_env)
    } else {
      res$filterbank_env = NA
    }
  }
  if("filters" %in% output) res$filters = filters
  invisible(res)
}

