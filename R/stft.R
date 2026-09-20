#' STFT and inverse STFT
#'
#' Short-Time Fourier Transform and its inverse for converting a signal between
#' time and frequency domains. \code{stft_simple} returns the full complex
#' spectrogram containing both positive and negative frequencies. Any final
#' samples that do not fit into a complete frame are silently dropped unless
#' \code{padWithSilence = TRUE}. \code{istft_simple} performs inverse STFT.
#'
#' If \code{wnSyn = "rectangle"}, no synthesis window is applied and the
#' original signal can be recovered exactly, without any distortion at the
#' beginning and the end (windows that taper to 0 give edge samples zero weight,
#' so pad with silence to reconstruct them). The reconstruction is then exact
#' for the covered portion of the signal when inverting an unnormalized windowed
#' STFT produced by \code{stft_simple}, provided there is no zero-padding and
#' the frame grid covers the signal. However, this works only if the complex
#' spectrogram is not modified between STFT and iSTFT. When the spectrogram is
#' modified - for example, when some filter is applied in the frequency domain -
#' it is better to set \code{wnSyn = "wola"}, which applies the same windowing
#' function to the iFFT of each frame. Finally, the much slower function
#' \code{istft_timevar} works with time-variable step sizes in the context of
#' dynamic pitch shifting or time stretching (see \code{\link{shiftPitch}}). The
#' \code{zp} argument of \code{stft_simple()} is for analysis/display only.
#' Spectrograms produced with \code{zp > wl} cannot be inverted by
#' \code{istft_simple()} or \code{istft_timevar()} in their current form.
#'
#' @inheritParams .roxygen_defaults
#' @param x numeric vector
#' @param samplingRate sampling rate (Hz). If left NULL, time and frequency
#'   labels are not added (faster, but not by much)
#' @param wl window length in samples (can be even or odd, but >=3). If wl
#'   exceeds the length of input vector, it is reset to wl = length(x). For
#'   maximum speed, set \code{wl = nextn(your_original_wl)}
#' @param step step in samples
#' @param padWithSilence if TRUE, pads the sound at both ends with half a window
#'   length of silence to resolve the edges properly and avoid dropping the last
#'   few samples (not needed if your sound is already padded with some silence)
#' @param wnSyn synthesis window: 'wola' = weighted overlap-add (WOLA) / STFT
#'   pseudoinverse, avoids spectral artifacts when the complex spectrogram is
#'   modified prior to iSTFT (default, recommended for most applications);
#'   'rectangle' = no synthesis window, allows reconstructing the original
#'   signal exactly provided that the spectrogram is not modified between STFT
#'   and iSTFT
#' @param spec input complex spectrogram (rows = frequency, columns = time)
#' @param type "full" = the full spectrogram returned by \code{stft_simple};
#'   "half" = just the positive frequencies up to Nyquist (default)
#' @param fade if TRUE, a linear fade-in and fade-out of length wl (but no more
#'   than 1/4 of input length) is applied to the output vector ("wola" method
#'   only)
#' @param tol to avoid division by 0, denominator values smaller than \code{tol}
#'   are incremented by \code{tol} times sum of squared windows ("wola" method only)
#' @param multPitch pitch multiplier interpolated across frames; > 1 --> raise
#'   pitch; must be positive
#' @param timeStretch time stretch factor interpolated across frames; > 1 -->
#'   increase duration; must be positive
#' @return \code{stft_simple} returns a complex spectrogram with both positive
#'   and negative frequencies as a matrix. If samplingRate is not NULL, row
#'   names give frequency in kHz and column names give time in ms. If
#'   samplingRate is NULL, no row or column names are added. Rows beyond the
#'   Nyquist frequency (half the sampling rate) correspond to negative
#'   frequencies. Time stamps correspond to the middle of each frame, starting
#'   with half a window length if \code{padWithSilence = FALSE} or starting at 0
#'   ms if \code{padWithSilence = TRUE}. \code{istft_simple} returns a numeric
#'   vector.
#' @export
#' @examples
#' ## Ex. 1: obtaining a spectrogram
#' data(speechEx, package = "soundgen")
#' spec = stft_simple(speechEx@left[1:10000], samplingRate = speechEx@samp.rate,
#'   wl = 512, step = 100)
#' image(t(Mod(spec)))
#' # the spectrum of one frame
#' plot(as.numeric(rownames(spec)), Mod(spec[, 15]), type = 'l', xlab = 'Freq, kHz')
#' spec[1:5, 1:5]
#'
#' # To get the positive frequencies only, use:
#' halfspec = spec[1:(nrow(spec) %/% 2 + 1), , drop = FALSE]
#' image(t(Mod(halfspec)))
#'
#'
#' ## Ex. 2: manual narrow-band spectral filter to turn white noise into a whistle
#' samplingRate = 16000
#' wl = 317; step = 51
#' noise = fade(rnorm(samplingRate), samplingRate = samplingRate)
#' spec = stft_simple(noise, samplingRate, wl = wl, step = step, zp = 0)
#' spec = spec[1:(wl %/% 2 + 1), ]  # discard negative frequencies
#'
#' # amplify one frequency band by 50 dB
#' spec_filtered = spec
#' spec_filtered[50, ] = spec_filtered[50, ] * 10^(50/20)
#' image(y = as.numeric(rownames(spec_filtered)), t(log(Mod(spec_filtered))))
#'
#' # inverse STFT using wnSyn = 'wola'
#' noise_filtered = fade(istft_simple(spec_filtered, wl = wl, step = step,
#'   type = 'half', wnSyn = 'wola'), samplingRate = samplingRate)
#' spectrogram(noise_filtered, samplingRate)
#' # playme(noise_filtered, samplingRate)
#'
#'
#' ## Ex. 3: reconstructing the input exactly with wnSyn = 'rectangle'
#' a = rnorm(64)
#' wl = 11; step = 3  # any wl and step are fine, even or odd
#' spec_full = stft_simple(a, wl = wl, step = step, wn = 'gaussian')
#' new_1 = istft_simple(spec_full, wl = wl, step = step, type = "full",
#'   wn = 'gaussian', wnSyn = 'rectangle')
#' plot(a, type = "l"); lines(new_1, col = 'green')
#' # note the missing bit at the end - incomplete final frame dropped
#'
#' spec_half = spec_full[1:(nrow(spec_full) %/% 2 + 1), ]
#' new_2 = istft_simple(spec_half, wl = wl, step = step, type = "half",
#'   wn = 'gaussian', wnSyn = 'wola')  # wola is also exact here
#' plot(a, type = "l"); lines(new_2, col = 'green')
#'
#'
#' ## Ex. 4: identity check for istft_timevar()
#' new_timevar = istft_timevar(spec_full, wl = wl, step = step,
#'   type = "full", multPitch = 1, timeStretch = 1, fade = FALSE)
#' plot(a, type = "l"); lines(new_timevar, col = 'green')
#' all(round(new_2, 5) == round(new_timevar, 5))  # should be TRUE
#' all(round(new_timevar, 5) == round(a[1:length(new_timevar)], 5))
#' # should be identical as well, except that two last samples in "a" are dropped
stft_simple = function(x,
                       samplingRate = NULL,
                       wl = 512,
                       step = wl %/% 2,
                       wn = 'gaussian',
                       zp = 0,
                       padWithSilence = FALSE) {
  # check the validity of inputs
  if (!is.finite(wl) || wl < 3 || wl != round(wl))
    stop('wl must be a positive integer >=3')
  if (!is.finite(step) || step < 1 || step != round(step))
    stop('step must be a positive integer >=1')
  if (!is.finite(zp) || zp < 0 || zp != round(zp))
    stop('zp must be a non-negative integer')
  n = length(x)
  if (n < 3) stop('input must be at least 3 samples long')
  if (wl > n) {
    wl = n  # still returns a matrix with a single column
    warning(paste('wl cannot be longer than the input; resetting wl to', n))
  }
  if (!is.null(samplingRate) && (!is.finite(samplingRate) || samplingRate <= 0))
    stop('if specified, samplingRate must be a finite positive number')

  # pad with silence to make sure edges are properly analyzed
  if (padWithSilence) {
    cwp2 = ceiling(wl / 2)
    x = c(rep(0, cwp2), x, rep(0, (wl + step)))
    n = n + cwp2 + wl + step
  }

  # STFT
  win = winFun(wl, wn)
  idx = seq(1, n + 1 - wl, step)
  # NB: mvfft(matrix) is not much faster and much more hassle than just vapply
  if (zp > wl) {
    zpad = rep(0, zp - wl)
    spec = vapply(idx, function(i) fft(c(x[i:(wl + i - 1)] * win, zpad)), complex(zp))
  } else {
    spec = vapply(idx, function(i) fft(x[i:(wl + i - 1)] * win), complex(wl))
  }

  # add rownames (frequency) and colnames (time)
  if (!is.null(samplingRate)) {
    nr = max(wl, zp)
    bin_width = samplingRate / nr
    # freq, kHz
    rnms = (0:(nr - 1)) * bin_width / 1000
    rownames(spec) = rnms
    idx_neg = (nr %/% 2 + 2):nr
    rownames(spec)[idx_neg] = rnms[idx_neg] - samplingRate / 1000
    # time, ms (offset by wl/2) - same definition as in getFrameBank()
    if (padWithSilence) {
      colnames(spec) = (idx - 1 + (wl / 2 - cwp2)) * 1000 / samplingRate
    } else {
      colnames(spec) = (idx - 1 + wl / 2) * 1000 / samplingRate
    }

  }
  spec
}


#' @rdname stft_simple
#' @export
istft_simple = function(spec,
                        wl,
                        step,
                        wn = "gaussian",
                        wnSyn = c('wola', 'rectangle'),
                        type = c("half", "full"),
                        fade = FALSE,
                        tol = 1e-6) {
  # check the validity of inputs
  type = match.arg(type)
  wnSyn = match.arg(wnSyn)
  if (!is.finite(wl) || wl < 3 || wl != round(wl))
    stop('wl must be a positive integer >=3')
  if (!is.finite(step) || step < 1 || step != round(step))
    stop('step must be a positive integer >=1')
  spec = as.matrix(spec)
  nc = ncol(spec)
  if (nc <1)
    stop('spec must be a matrix with at least 1 column')
  nr = nrow(spec)
  if (nr < 1)
    stop('spec must be a matrix with at least 1 row')

  # For real output, DC and Nyquist must be real
  if (type == "half") {
    spec[1, ] = Re(spec[1, ])
    if (wl %% 2 == 0 && nr > 1) {
      spec[nr, ] = Re(spec[nr, ])
    }

    # Recreate negative frequencies.
    if (wl %% 2 == 0) {  # even wl
      if (nr > 2) {
        spec = rbind(spec, Conj(spec[(nr - 1):2, , drop = FALSE]))
        # Why? Try:
        # a = fft(1:10)
        # b = a[1:(length(a) %/% 2 + 1)]
        # a; c(b, Conj(rev(b[-c(1, length(b))])))
      }
    } else {  # odd wl
      if (nr > 1) {
        spec = rbind(spec, Conj(spec[nr:2, , drop = FALSE]))
        # Why? Try:
        # a = fft(1:11)
        # b = a[1:(length(a) %/% 2 + 1)]  # see spectrogram freq range of DFT
        # a; c(b, Conj(rev(b[-1])))
      }
    }
  }
  if (nrow(spec) != wl)
    stop('dimension mismatch between wl and spec; zero-padded STFTs are not supported')

  # Analysis window
  x = winsum = rep(0, (nc - 1) * step + wl)
  win = winFun(wl, wn)

  # Inverse STFT
  idx_nc = 1:nc
  idx_i_start = step * (idx_nc - 1) + 1
  if (wnSyn == 'rectangle') {
    # rectangle synthesis window - exact reconstruction from unmodified spectrogram
    for (i in idx_nc) {
      x_i = Re(fft(spec[, i], inverse = TRUE))
      idx_i = idx_i_start[i]:(idx_i_start[i] + wl - 1)
      x[idx_i] = x[idx_i] + x_i  # add unwindowed x_i
      winsum[idx_i] = winsum[idx_i] + win
    }

    # Undo windowing, normalize
    if (max(winsum) == 0) return(x / wl)
    idx_0 = which(winsum == 0)
    x[idx_0] = 0
    winsum[idx_0] = 1
    x = x / wl / winsum
  } else if (wnSyn == 'wola') {
    # weighted overlap-add (WOLA) / STFT pseudoinverse
    win_square = win * win
    for (i in idx_nc) {
      x_i = Re(fft(spec[, i], inverse = TRUE))
      idx_i = idx_i_start[i]:(idx_i_start[i] + wl - 1)
      x[idx_i] = x[idx_i] + x_i * win  # add windowed x_i
      winsum[idx_i] = winsum[idx_i] + win_square
    }
    if (max(winsum) == 0) return(x / wl)

    # Undo windowing, normalize
    idx_0 = which(winsum == 0)
    x[idx_0] = 0
    winsum[idx_0] = 1
    floor = max(winsum) * tol
    x = x / wl / pmax(winsum, floor)

    # Optional: fade the first and last wl samples in case there are
    # discontinuities at the beginning / end
    if (fade) {
      fl = min(wl, length(x) %/% 4)
      if (fl > 1) {
        ramp = seq(0, 1, length.out = fl)
        x[seq_len(fl)] = x[seq_len(fl)] * ramp
        n = length(x)
        x[(n - fl + 1):n] = x[(n - fl + 1):n] * rev(ramp)
      }
    }
  }
  x
}


#' @rdname stft_simple
#' @export
istft_timevar = function(spec,
                         wl,
                         step,
                         wn = "gaussian",
                         type = c("half", "full"),
                         multPitch = 1,
                         timeStretch = 1,
                         fade = TRUE,
                         tol = 1e-6) {
  # check the validity of inputs
  type = match.arg(type)
  if (!is.finite(wl) || wl < 3 || wl != round(wl))
    stop('wl must be a positive integer >=3')
  if (!is.finite(step) || step < 1 || step != round(step))
    stop('step must be a positive integer >=1')
  spec = as.matrix(spec)
  nc = ncol(spec)
  if (nc <1)
    stop('spec must be a matrix with at least 1 column')
  nr = nrow(spec)
  if (nr < 1)
    stop('spec must be a matrix with at least 1 row')
  if (any(!is.finite(multPitch) | multPitch <= 0)) {
    stop("multPitch must be positive")
  }
  if (any(!is.finite(timeStretch) | timeStretch <= 0)) {
    stop("timeStretch must be positive")
  }

  if (length(multPitch) == 1) {
    multPitch_long = rep(multPitch, nc)
  } else {
    multPitch_long = approx(multPitch, n = nc)$y
  }
  if (length(timeStretch) == 1) {
    timeStretch_long = rep(timeStretch, nc)
  } else {
    timeStretch_long = approx(timeStretch, n = nc)$y
  }

  target_len = pmax(3L, round(wl / multPitch_long))
  # need at least 3 samples b/c winFun() needs n>=3
  if (nc == 1) {
    starts = 0
  } else {
    hops = step * timeStretch_long[1:(nc - 1)]
    starts = round(c(0, cumsum(hops)))
  }
  len_out = max(starts + target_len)
  x = winsum = numeric(len_out)

  # Reconstruct full spectrum if needed
  nr = nrow(spec)
  if (type == "half") {
    spec[1, ] = Re(spec[1, ])
    if (wl %% 2 == 0 && nr > 1) {
      spec[nr, ] = Re(spec[nr, ])
    }
    if (wl %% 2 == 0) {
      if (nr > 2) {
        spec = rbind(spec, Conj(spec[(nr - 1):2, , drop = FALSE]))
      }
    } else {
      if (nr > 1) {
        spec = rbind(spec, Conj(spec[nr:2, , drop = FALSE]))
      }
    }
  }
  if (nrow(spec) != wl)
    stop('dimension mismatch between wl and spec; zero-padded STFTs are not supported')

  # Correct normalized frequencies
  freq_norm = (0:(wl - 1)) / wl
  if (wl %% 2 == 0) {
    neg_idx = (wl / 2 + 2):wl
  } else {
    neg_idx = (wl %/% 2 + 2):wl
  }
  if (length(neg_idx) > 0) {
    freq_norm[neg_idx] = freq_norm[neg_idx] - 1
  }
  abs_freqs = abs(freq_norm)

  # iSTFT
  win = winFun(wl, wn)
  win_square = win * win
  for (i in 1:nc) {
    if (abs(multPitch_long[i] - 1) > 1e-8) {
      target_len_i = target_len[i]

      # Apply anti-aliasing filter if pitch is being raised
      # (mostly an issue if multPitch > 1.5)
      if (multPitch_long[i] > 1) {
        # The new Nyquist frequency in normalized units
        # When we compress time by multPitch, frequencies scale by multPitch
        # So the new Nyquist is 0.5 / multPitch on the ORIGINAL frequency scale
        nyquist_new = 0.5 / multPitch_long[i]

        # Create frequency-domain lowpass filter
        # We want to keep frequencies where |freq_norm| <= nyquist_new
        # 0 = stopband,  1 = passband
        # transition band - cosine taper (10% of the cutoff)
        transition_width = nyquist_new * 0.1
        distances = (abs_freqs - (nyquist_new - transition_width)) / transition_width
        distances = pmin(pmax(distances, 0), 1)
        filter_response = 0.5 * (1 + cos(pi * distances))
        # plot(freq_norm, filter_response, type = 'l')

        # Apply filter in frequency domain
        spec_filtered = spec[, i] * filter_response

        # Convert back to time domain
        x_i = Re(fft(spec_filtered, inverse = TRUE))
      } else {
        x_i = Re(fft(spec[, i], inverse = TRUE))
      }

      # Energy normalization for the stretched/compressed frame
      # x_i = x_i * sqrt(wl / target_len_i)

      # Resample using spline
      x_i = spline(x_i, n = target_len_i)$y
      win_i = winFun(target_len_i, wn)
      idx_i = (starts[i] + 1):(starts[i] + target_len_i)
      x[idx_i] = x[idx_i] + x_i * win_i
      winsum[idx_i] = winsum[idx_i] + win_i * win_i
    } else {
      x_i = Re(fft(spec[, i], inverse = TRUE))
      idx_i = (starts[i] + 1):(starts[i] + wl)
      x[idx_i] = x[idx_i] + x_i * win
      winsum[idx_i] = winsum[idx_i] + win_square
    }
  }

  if (max(winsum) == 0) return(x / wl)

  x[winsum == 0] = 0
  winsum[winsum == 0] = 1
  myfloor = max(winsum) * tol
  x = x / wl / pmax(winsum, myfloor)

  # Optional: fade the first and last wl samples in case there are
  # discontinuities at the beginning / end
  if (fade) {
    fl = min(wl, length(x) %/% 4)
    if (fl > 1) {
      ramp = seq(0, 1, length.out = fl)
      x[seq_len(fl)] = x[seq_len(fl)] * ramp
      n = length(x)
      x[(n - fl + 1):n] = x[(n - fl + 1):n] * rev(ramp)
    }
  }

  x
}


#' Frame bank
#'
#' Saves windowed (and optionally zero-padded) frames, i.e. chunks of the sound
#' file of the right size and spacing. Used by analyze() to prepare frames for
#' ACF.
#' @param sound numeric vector
#' @inheritParams spectrogram
#' @param wl length of fft window (points)
#' @param filter fft window filter (defaults to NULL)
#' @param timeShift time (s) added to timestamps
#' @return A matrix with windowed frames in columns.
#' @noRd
#' @examples
#' a = soundgen:::getFrameBank(sin(1:1000), 16000, 512, 'gaussian', 15, 0)
#' str(a)
getFrameBank = function(sound,
                        samplingRate,
                        wl,
                        wn,
                        step,
                        zp,
                        normalize = TRUE,
                        filter = NULL,
                        padWithSilence = FALSE,
                        timeShift = NULL) {
  # normalize to range from no less than -1 to no more than +1
  if (!is.numeric(sound) || length(sound) < wl)
    stop('expect numeric input of length >= wl')
  if (normalize && any(sound != 0)) {
    sound = sound - mean(sound)
    m = max(abs(sound))
    if (m > 0) sound = sound / m
  }
  step_points = max(1, round(step / 1000 * samplingRate))

  if (padWithSilence) {
    # pad with silence to make sure edges are properly analyzed
    cwp2 = ceiling(wl / 2)
    sound = c(rep(0, cwp2),
              sound,
              rep(0, (wl + step_points)))
  }
  myseq = seq(1, length(sound) - wl + 1,
              by = step_points)
  if (padWithSilence) {
    time_stamps = (myseq - 1 + wl / 2 - cwp2) * 1000 / samplingRate
  } else {
    time_stamps = (myseq - 1 + wl / 2) * 1000 / samplingRate
  }
  if (!is.null(timeShift)) time_stamps = time_stamps + round(timeShift * 1000)

  if (is.null(filter)) {
    filter = winFun(wl, wn)
  }

  # zero padding
  zpExtra = max(0, zp - wl)
  if (zpExtra > 0) {
    frameBank = vapply(myseq, function(x) {
      c(sound[x:(wl + x - 1)] * filter,
        rep(0, zpExtra))
    }, numeric(wl + zpExtra))
  } else {
    frameBank = vapply(myseq, function(x) {
      sound[x:(wl + x - 1)] * filter
    }, numeric(wl))
  }
  colnames(frameBank) = time_stamps
  frameBank
}
